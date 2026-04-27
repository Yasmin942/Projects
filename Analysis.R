load("DroughtData.RData")
set.seed(20256637)

rem_ind <- sample(1:nrow(DroughtData), 20, replace=FALSE)
data <- DroughtData[-rem_ind,]

remove(DroughtData,rem_ind)


library(tidyverse)

# Summary of Data


head(data)
summary(data)

# Grouping summaries

data %>% group_by(location) %>%
  summarise(count=n(), .groups = "drop") %>%
  print(n=Inf)

data %>% group_by(height) %>%
  summarise(count=n(), .groups = "drop") 


data %>% group_by(measure) %>%
  summarise(count=n(), .groups = "drop")

head(data$height)


#month name changes

data <- data %>%
  mutate(MONTH = ifelse(MONTH == "1", "Jan",
                        ifelse(MONTH == "2", "Feb",
                               ifelse(MONTH == "3", "Mar",
                                      ifelse(MONTH == "4", "Apr",
                                             ifelse(MONTH == "5", "May",
                                                    ifelse(MONTH == "6", "Jun",
                                                           ifelse(MONTH == "7", "Jul",
                                                                  ifelse(MONTH == "8", "Aug",
                                                                         ifelse(MONTH == "9", "Sep",
                                                                                ifelse(MONTH == "10", "Oct",
                                                                                       ifelse(MONTH == "11", "Nov",
                                                                                              ifelse(MONTH == "12", "Dec", MONTH)))))))))))))


data <- data %>%
  mutate(MONTH = factor(MONTH, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul",
                                          "Aug", "Sep", "Oct", "Nov", "Dec")))

data %>% arrange((MONTH))

str(data)

# filtering height by 2

data1 <- data %>% filter(height == "2") %>%
  group_by(location, YEAR, MONTH, score, PRECTOT, PS, TS, value, measure) %>%
  summarise(value = mean(value), .groups = "drop") %>%
  pivot_wider(names_from = measure, values_from = value) %>%
  arrange((MONTH))

head(data1)


# Creating wind columns

wind_10 <- data %>%
  filter(height == 10, measure == "Wind_Speed") %>%
  group_by(location, YEAR, MONTH) %>%
  summarise(Wind_Speed_10=mean(value), .groups = "drop")

summary(wind_10)
head(wind_10)

wind_50 <- data %>%
  filter(height == 50, measure == "Wind_Speed") %>%
  group_by(location, YEAR, MONTH) %>%
  summarise(Wind_Speed_50=mean(value), .groups = "drop")

summary(wind_50)
head(wind_50)


# final dataset

final <- data1 %>% left_join(wind_10, by = c("location", "MONTH", "YEAR")) %>%
  left_join(wind_50, by = c("location", "MONTH", "YEAR"))

head(final)
str(final)

# renaming

final <- final %>%
  rename(Location = location,
         Month = MONTH,
         Year = YEAR,
         Score = score,
         Prectot = PRECTOT,
         SPressure = PS,
         ESTemperature = TS,
         WBTemperature = Wet_bulb_temperature,
         Dew_Point = Dew_point,
         WindSpeed_50 = Wind_Speed_50,
         WindSpeed_10 = Wind_Speed_10)

head(final)

#removing any missing or duplicted variables present

sum(duplicated(final))

sum(is.na(final))

final_clean <- na.omit(final)

sum(is.na(final_clean))

head(final_clean)

summary(final_clean)
sd(final_clean$Score)
# PART 2


# leaps and bounds

library(leaps)

pairs(final [, sapply(final, is.numeric)])

pairs(final_clean [, sapply(final_clean, is.numeric)])



best_subset <- leaps(x = final_clean[, 5:13], 
                     y = final_clean$Score,
                     nbest = 9, method = "adjr2",
                     names = colnames(final_clean[, 5:13]))


data.frame(size = best_subset$size, AdjR2 = round(best_subset$adjr2, 3),
           best_subset$which, row.names = NULL)

plot(best_subset$size, best_subset$adjr2, 
     ylab = "Adjusted R-Squared",
     xlab = "Number of Variables (including Intercept)")


best_subset


colnames(final_clean)
nrow(final_clean)
ncol(final_clean)


names = colnames(final_clean[, 5:13])

#55 matches - at 8 plateaus


#Forward selection in R

final_0 <- lm(Score ~ 1, data = final_clean)
add1(final_0, test = "F",
     scope = ~ Prectot + SPressure + Specific_Humidity + ESTemperature +
       WBTemperature + Temperature + Dew_Point + WindSpeed_50 + WindSpeed_10)

final_1 <- lm(Score ~ SPressure, data = final_clean)
add1(final_1, test = "F",
     scope = ~ Prectot + SPressure + Specific_Humidity + ESTemperature +
       WBTemperature + Temperature + Dew_Point + WindSpeed_50 + WindSpeed_10)

final_2 <- lm(Score ~ SPressure + Specific_Humidity, data = final_clean)
add1(final_2, test="F",
     scope =  ~ Prectot + SPressure + Specific_Humidity + ESTemperature +
       WBTemperature + Temperature + Dew_Point + WindSpeed_50 + WindSpeed_10)



final_3 <- lm(Score ~ SPressure + Specific_Humidity + Temperature, data = final_clean)
add1(final_3, test = "F",
     scope =  ~ Prectot + SPressure + Specific_Humidity + ESTemperature +
       WBTemperature + Temperature + Dew_Point + WindSpeed_50 + WindSpeed_10)  



final_4 <- lm(Score ~ SPressure + Specific_Humidity + Temperature + ESTemperature,
              data = final_clean)
add1(final_4, test = "F",
     scope = ~ Prectot + SPressure + Specific_Humidity + ESTemperature +
       WBTemperature + Temperature + Dew_Point + WindSpeed_50 + WindSpeed_10)



final_5 <- lm(Score~ SPressure + Specific_Humidity + Temperature + ESTemperature +
                WindSpeed_50, data = final_clean)
add1(final_5, test = "F",
     scope = ~ Prectot + SPressure + Specific_Humidity + ESTemperature +
       WBTemperature + Temperature + Dew_Point + WindSpeed_50 + WindSpeed_10)



final_6 <- lm(Score~ SPressure + Specific_Humidity + Temperature + ESTemperature +
                WindSpeed_50 + WBTemperature, data = final_clean)
add1(final_6, test = "F",
     scope = ~ Prectot + SPressure + Specific_Humidity + ESTemperature +
       WBTemperature + Temperature + Dew_Point + WindSpeed_50 + WindSpeed_10)




final_7 <- lm(Score~ SPressure + Specific_Humidity + Temperature + ESTemperature +
                WindSpeed_50 + WBTemperature + Prectot, data = final_clean )
add1(final_7, test = "F",
     scope =~ Prectot + SPressure + Specific_Humidity + ESTemperature +
       WBTemperature + Temperature + Dew_Point + WindSpeed_50 + WindSpeed_10)



#Backwards selection

final_b0 <- lm(Score ~ Prectot + SPressure + Specific_Humidity + ESTemperature + WBTemperature +
                 Temperature + Dew_Point + WindSpeed_50 + WindSpeed_10, data = final_clean)
drop1(final_b0, test = "F",
      scope = ~ Prectot + SPressure + Specific_Humidity + ESTemperature +
        WBTemperature + Temperature + Dew_Point + WindSpeed_50 + WindSpeed_10)


final_b1 <- lm(Score ~ Prectot + SPressure + Specific_Humidity + ESTemperature + WBTemperature +
                 Temperature + Dew_Point + WindSpeed_50, data = final_clean)
drop1(final_b1, test = "F",
      scope = ~ Prectot + SPressure + Specific_Humidity + ESTemperature +
        WBTemperature + Temperature + Dew_Point + WindSpeed_50)


final_b2 <- lm(Score ~ Prectot + SPressure + Specific_Humidity + ESTemperature + WBTemperature +
                 Temperature + WindSpeed_50, data = final_clean)
drop1(final_b2, test = "F",
      scope = ~ Prectot + SPressure + Specific_Humidity + ESTemperature +
        WBTemperature + Temperature + WindSpeed_50)




#Stepwise Selection

final1 <- lm(Score~1, data = final_clean)

step(final1, scope = ~ Prectot + SPressure + Specific_Humidity + ESTemperature +
       WBTemperature + Temperature + Dew_Point + WindSpeed_50 + WindSpeed_10,
     direction = "both")


# correlation matrix - to avoid multicolinearity

cor(final_clean[, c("Prectot", "SPressure", "Specific_Humidity", "ESTemperature", 
                    "WBTemperature", "Temperature", "Dew_Point", 
                    "WindSpeed_50", "WindSpeed_10")])


#model with wbt included

wbt_model <- lm(Score ~ SPressure + Specific_Humidity + WindSpeed_50 + Temperature + Prectot + WBTemperature,
                data = final_clean)

summary(wbt_model)

#model without wbt included

non_wbt_model <- lm(Score ~ SPressure + Specific_Humidity + Temperature + WindSpeed_50 + Prectot,
                    data = final_clean)

summary(non_wbt_model)

# final model

final_model <- lm(Score ~ SPressure + Specific_Humidity + Temperature + WindSpeed_50 + Prectot + WBTemperature,
                  data = final_clean)

final_model
summary(final_model)



#Assumption checking linear regression

plot(final_model)
plot(final_model, which = 1) #assessing whether errors have constant variance
plot(final_model, which = 2) #qq norm plot with qqline
plot(final_model, which = 3)
plot(final_model, which = 4)
plot(final_model, which = 5)
plot(final_model, which = 6)

# errors are independent
acf(residuals(final_model))

#Estimating lambda and whether transformation is required - only covers 0.5
library(MASS)
boxcox(final_model, plotit=TRUE)


#Partition of variance - residual sum of squares 334.42
anova(final_model)





#redoing lm with sqrt due to Boxcox and assumptions being violated

transformed_model <- lm(sqrt(Score) ~ SPressure + Specific_Humidity + Temperature + WindSpeed_50 + Prectot + WBTemperature,
                        data = final_clean)

summary(transformed_model) #adjusted r^2 34.2% - still very low

# Plotting assumptions

plot(transformed_model)
plot(transformed_model, which = 1) #assessing whether errors have constant variance
plot(transformed_model, which = 2) #qq norm plot with qqline
plot(transformed_model, which = 3)
plot(transformed_model, which = 4)
plot(transformed_model, which = 5)
plot(transformed_model, which = 6)



#  errors are independent
acf(residuals(transformed_model))



# Repeat - estimating lambda again - covers 1 - no more transforming is required

boxcox(transformed_model, plotit=TRUE)

#partition of variance - much lower residual sum of sqrs 56.372
anova(transformed_model)

#confidence interval for a fitted value - Napa county prediction - transformed model
Napa_prediction <- predict(transformed_model, newdata = data.frame (SPressure = 98.7, Prectot = 0.13, WindSpeed_50 = 3.63, Specific_Humidity = 6.67, 
                                                                    Temperature = 20.5, WBTemperature = 7.29), interval = "prediction", level = .95)

Napa_prediction


# squaring prediction due to earlier transformation 
Napa_prediction^2



#Confidence interval for a fitted value - Greenlee, Arizona (using transformed model)
final_clean %>%
  filter(Location == "Arizona, Greenlee", Month == "Jan") %>%
  dplyr::select(Score, Prectot, SPressure, Temperature, WBTemperature, WindSpeed_50, Specific_Humidity)

Greenlee_prediction <- predict(transformed_model, newdata = data.frame (SPressure = 82.70, Prectot = 1.95, WindSpeed_50 = 3.87, Specific_Humidity = 3.86, 
                                                                        Temperature = 4.67, WBTemperature = -2.85), interval = "prediction", level = .95)

Greenlee_prediction     

# squaring prediction due to earlier transformation 

Greenlee_prediction^2



#Confidence interval for a fitted value - Providence, Rhode Island (using transformed model)

final_clean %>%
  filter(Location == "Rhode Island, Providence", Month == "Jul") %>%
  dplyr::select(Score, Prectot, SPressure, Temperature, WBTemperature, WindSpeed_50, Specific_Humidity)


Providence_prediction <- predict(transformed_model, newdata = data.frame (SPressure = 100.03, Prectot = 2.08, WindSpeed_50 = 3.68, Specific_Humidity = 12.65, 
                                                                          Temperature = 22.61, WBTemperature = 17.41), interval = "prediction", level = .95)

Providence_prediction

# squaring prediction due to earlier transformation 

Providence_prediction^2


# Interval estimation for beta
# confidence intervals

summary(transformed_model)
confint(transformed_model, level = .95)




# REDO OF SELECTION METHODS TO INCLUDE MONTH - TESTING WHETHER IT IS SIGNIFICANT TO BE INCLUDED IN THE MODEL


#summary statistics 

summary(final_clean)

tlocation <- table(final_clean$Location)
tmonth <- table(final_clean$Month)
print(tmonth)
print(tlocation) # latter half of the year were moreso recorded for drought than any other month. Focus on highest recorded month -- sep

# creating bar plots

?barplot
?axis
?text
?margin

barplot(tlocation,
        col = "purple",
        main = "Barplot of Location Drought Occured",
        xlab = "Location",
        ylab = "Frequency",
        las = 2,
        cex.names = 0.7)

?barplot

barplot(tmonth,
        col = "purple",
        main = "Barplot of Months Drought was Recorded",
        xlab = "Months",
        ylab = "Frequency",
        las = 2,
        cex.names = 0.9)

#Histogram of Score 

require(ggplot2)

ggplot(final_clean) +
  geom_histogram(aes(Score), bins = 20, binwidth = 0.2,
                 colour = "white", fill = "red") +
  labs(title="Histogram of Drought Score",
       x = "Score",
       y = "Frequency")

# Histogram for SPressure

ggplot(final_clean) +
  geom_histogram(aes(SPressure), bins = 15, binwidth = 1,
                 colour = "white", fill = "red") + 
  labs(title ="Histogram of Surface Pressure",
       x = "Surface Pressure (kPa)",
       y = "Frequency")


# Histogram for ESTemperature

ggplot(final_clean) +
  geom_histogram(aes(ESTemperature), bins = 20, binwidth = 1.5,
                 colour = "white", fill = "red") + 
  labs(title ="Histogram of Earth Skin Temperature",
       x = "Earth Skin Temperature (C)",
       y = "Frequency")


# Histogram for Specific Humidity

ggplot(final_clean) +
  geom_histogram(aes(Specific_Humidity), bins = 20, binwidth = 0.5,
                 colour = "white", fill = "red") + 
  labs(title ="Histogram of Specific Humidity",
       x = "Specific Humidity (g/kg)",
       y = "Frequency")


# Histogram of Temperature

ggplot(final_clean) +
  geom_histogram(aes(Temperature), bins = 20, binwidth = 1.5,
                 colour = "white", fill = "red") + 
  labs(title ="Histogram of Temperature",
       x = "Temperature (C)",
       y = "Frequency")


# Histogram of Dew_point

ggplot(final_clean) +
  geom_histogram(aes(Dew_Point), bins = 20, binwidth = 1.5,
                 colour = "white", fill = "red") + 
  labs(title ="Histogram of Dew Point",
       x = "Dew Point (C)",
       y = "Frequency")


# Histogram for Wet_bulb_temperature

ggplot(final_clean) +
  geom_histogram(aes(WBTemperature), bins = 20, binwidth = 1,
                 colour = "white", fill = "red") + 
  labs(title ="Histogram of Wet Bulb Temperature",
       x = "Wet Bulb Temperature (C)",
       y = "Frequency")

# Plot between Score and SPressure

ggplot(final_clean, aes(x = Score, y = SPressure)) +
  geom_point() +
  labs(title = "Score vs Surface Pressure",
       x = "Score",
       y = "Surface Pressure (kPa)")

#Plot between Score and ESTemperature

ggplot(final_clean, aes(x=Score, y=ESTemperature)) +
  geom_point() +
  labs(title = "Score vs Earth Skin Temperature",
       x = "Score",
       y = "Earth Skin Temperature (C)")

# Plot between Score and Temperature

ggplot(final_clean, aes(x= Score, y = Temperature)) +
  geom_point()+
  labs(title = "Score vs Temperature",
       x = "Score",
       y = "Temperature (C)")

# Plot between Score and Specific_Humidity

ggplot(final_clean, aes( x = Score, y = Specific_Humidity)) +
  geom_point() +
  labs(title = "Score vs Specific Humidity",
       x = "Score",
       y = "Specific Humidity (g/kg")

# Plot between Score and Dew_point

ggplot(final_clean, aes(x = Score, y = Dew_Point)) +
  geom_point() +
  labs(title = "Score vs Dew Point",
       x = "Score",
       y = "Dew Point (C)")

# Plot between Score and Wet_bulb_temperature

ggplot(final_clean, aes(x = Score, y = WBTemperature)) +
  geom_point() +
  labs(title = "Score vs Wet Bulb Temperature",
       x = "Score",
       y = "Wet Bulb Temperature (C)")

# Plot between Score and WindSpeed_10

ggplot(final_clean, aes(x = Score, y = WindSpeed_10)) +
  geom_point() +
  labs(title = "Score vs Wind Speed at 10 Meters",
       x = "Score",
       y = "Wind Speed 10 meters (m/s)")

# Plot between Score and WindSpeed_50

ggplot(final_clean, aes(x = Score, y = WindSpeed_50)) +
  geom_point() +
  labs(title = "Score vs Wind Speed at 50 Meters",
       x = "Score",
       y = "Wind Speed 50 Meters (m/s)")

# Boxplot of Score by Location

ggplot(final_clean, aes(y=Location, x = Score)) +
  geom_boxplot(colour = "brown", fill = "orange") +
  labs(title = "Boxplot of Drought Score by State",
       x = "Score",
       y = "Location by State")

# Boxplot of Score by Month

ggplot(final_clean, aes(y=Month, x = Score)) +
  geom_boxplot(colour = "brown", fill = "orange") +
  labs(title = "Boxplot of Drought Score by Month",
       x = "Score",
       y = "Month")


# Lineplot of Drought Score over time (month) by state (location)

ggplot(final_clean, aes(y=Score, x = Month)) +
  geom_line()

?geom_line


#Extra Summary Statistics

final_clean %>% group_by(Location) %>%
  summarise(count=n(), .groups = "drop") %>%
  print(n=Inf)

final_clean %>% group_by(Month) %>%
  summarise(count=n(), .groups = "drop") %>%
  print(n=Inf)

#redo of selection methods - including Month this time

head(final_clean)

library(leaps)
library(tidyverse)
library(dplyr)

final_clean <- final_clean %>%
  dplyr::select(Location, Year, Score, Month, Prectot, SPressure,
                ESTemperature, Dew_Point, WBTemperature, Temperature,
                Specific_Humidity ,WindSpeed_10, WindSpeed_50)

head(final_clean)


