#Loading relevant packages
install.packages("tidyverse")
library(tidyverse)
install.packages("dplyr")
library(dplyr)
install.packages("tidyr")
library(tidyr)
install.packages("ggplot2")
library(ggplot2)
install.packages("readr")
library(readr)
install.packages("here")
library(here)
install.packages("lubridate")
library(lubridate)
install.packages("data.table")
library(data.table)


#Checking working directory
getwd()

#Importing data
daily_activity <- read.csv("dailyActivity_merged.csv", header = TRUE, sep = ",")
daily_sleep <- read.csv("sleepDay_merged.csv", header = TRUE, sep = ",")

#Information about the data
The daily activity data has 940 observations of 15 variables and the daily sleep data has 413 observations of 5 variable.names

#Checking the structure of the dataset
str(daily_activity)
str(daily_sleep)
colnames(daily_activity)
colnames(daily_sleep)

#Transforming the data
SleepDay_new <- separate(daily_sleep, SleepDay, c("Date", "Time"), " ")
SleepDay_new %>%
  rename(ActivityDate = Date)
daily_activity %>%
  rename(Date = ActivityDate)
colnames(daily_activity)
colnames(SleepDay_new)
SleepDayNewV2 <- SleepDay_new %>%
  rename(ActivityDate = Date)
colnames(SleepDayNewV2)
daily_activity$ActivityDate <- as.Date(daily_activity$ActivityDate, "%m/%d/%Y")
merged_data <- merge(daily_activity, SleepDayNewV2, by=c("Id", "ActivityDate"))
head(merged_data)
colnames(merged_data)
merged_dataV2 <-merged_data %>%
  mutate(hours_sleep = TotalMinutesAsleep/60, hours_in_bed = TotalTimeInBed/60)

#Removing duplicates
unique_merged_data <- merged_dataV2[!duplicated(merged_dataV2),]
Three duplicate rows were removed as a result

unique_merged_dataV2 <-unique_merged_data %>%
  mutate(hours_sedentary = SedentaryMinutes/60)
n_distinct(unique_merged_data$Id)
There are 24 unique participants in this merged data set


#Analysing the data
Descriptive statistics
colnames(unique_merged_dataV2)
unique_merged_dataV2 %>%
  select(TotalSteps,
         hours_sleep,
         hours_in_bed,
         hours_sedentary,
         Calories) %>%
  summary()

A summary of the data shows that the daily steps taken by users ranged from 17 steps to 22770 steps with the mean steps at 8515 and median steps at 8913 steps.
Users daily sleep logged ranged from 0.97 to 13 hours with the mean at 6.9 hours and the median at 7.2 hours. The average time in bed was 7.6 hours although users slept for 6.9 hours.
The data also shows that users are quite sedentary with the mean and median hours users spent sedentary being 11.9 hours

This means that users are quite inactive during the day
Many users are also not getting the required 8 hours of sleep a day

unique_merged_dataV2 %>%
  count(hours_sleep >= "8")

8 or more hours of sleep was true for only 97 of the 410 observations

colnames(unique_merged_dataV2)

ggplot(data = unique_merged_dataV2)+
  geom_point(mapping = aes(x = TotalSteps, y = TotalDistance))

ggplot(data = unique_merged_dataV2)+
  geom_point(mapping = aes(x = TotalSteps, y = Calories))+
  geom_smooth(mapping = aes(x = TotalSteps, y = Calories))+
  labs(title ="The relationship between total steps and total calories")

The data shows that those who take more steps seem to burn more calories, therefore there is a positive correlation between the total steps and the total calories, although there is also a great deal of variance meaning that this isnt consistent
ggsave("Steps vs Calories.pdf")

ggplot(data = unique_merged_dataV2)+
  geom_point(mapping = aes(x = SedentaryMinutes, y = Calories))+
  geom_smooth(mapping = aes(x = SedentaryMinutes, y = Calories))+
  labs(title ="The relationship between sedentary minutes and total calories")

ggplot(data = unique_merged_dataV2)+
  geom_point(mapping = aes(x = VeryActiveMinutes, y = Calories), color = "purple")+
  geom_smooth(mapping = aes(x = VeryActiveMinutes, y = Calories))+
  labs(title ="The relationship very active minutes and total calories")

ggplot(data = unique_merged_dataV2)+
  geom_point(mapping = aes(x = TotalTimeInBed, y = TotalMinutesAsleep), color = "purple")+
  geom_smooth(mapping = aes(x = TotalTimeInBed, y = TotalMinutesAsleep))+
  labs(title ="The relationship total time in bed and total minutes of sleep")

There is a positve correlation between the time people spend in bed and the sleep time. Most of the data points are quite close to the trendline, although there are a few outliers.

ggsave("Sleep vs time in bed.pdf")







