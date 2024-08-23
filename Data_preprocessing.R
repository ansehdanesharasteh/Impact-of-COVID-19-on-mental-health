
#load libraries

library(tidyr)
library(stringr)
library(readr)
library(ggplot2)
library(dplyr)
library(readxl)
library(data.table)
library(writexl)

#Read the data and filter to individuals above 26 years old

data <- read_excel("C:/Works/Lina/temp.xlsx", sheet= "Sheet3")
data <- data[!(data$`2. What is your age?`=="26+"),]
data$`How many days a week did you exercise before COVID-19 quarantine?` <- as.integer(data$`How many days a week did you exercise before COVID-19 quarantine?`)

#Clean data and move the required fields to a new df

prep_data <- data.frame(matrix(, nrow=2321, ncol=0))
prep_data$timestamp <- data$Timestamp

##Clean data
#Convert gender into categorical variable (0/1) to be able to use in machine learning model
prep_data$gender <- ifelse(data$`1. What is your gender?` == "Male", 1, 0)

#Convert age group into categorical variable to be able to use in machine learning model
prep_data$under_18 <- ifelse(data$`2. What is your age?` == "Under 18", 1, 0)
prep_data$`18_29` <- ifelse(data$`2. What is your age?` == "18-29", 1, 0)
prep_data$`30_39` <- ifelse(data$`2. What is your age?` == "30-39", 1, 0)
prep_data$`40_49` <- ifelse(data$`2. What is your age?` == "40-49", 1, 0)
prep_data$above_50 <- ifelse(data$`2. What is your age?` == "50 and above", 1, 0)

#Convert education into categorical variable 
prep_data$`less than high school` <- ifelse(data$`4. What is the highest level of education you have completed?` == "Less than High School", 1, 0)
prep_data$`high school` <- ifelse(data$`4. What is the highest level of education you have completed?` == "High School", 1, 0)
prep_data$college <- ifelse(data$`4. What is the highest level of education you have completed?` == "2 or 4 Years of College Degree (AA, BA, BS)", 1, 0)
prep_data$master <- ifelse(data$`4. What is the highest level of education you have completed?` == "Master's Degree", 1, 0)
prep_data$doctoral <- ifelse(data$`4. What is the highest level of education you have completed?` == "Doctoral Degree", 1, 0)
prep_data$professional <- ifelse(data$`4. What is the highest level of education you have completed?` == "Professional Degree (MD, JD, PharmD, ...)", 1, 0)

#Convert diet into categorical variable 
prep_data$`western diet` <- ifelse(data$`6. What is your dietary pattern?` == "Western Diet?", 1, 0)
prep_data$`eastern diet` <- ifelse(data$`6. What is your dietary pattern?` == "Eastern Diet", 1, 0)
prep_data$`mediterranean diet` <- ifelse(data$`6. What is your dietary pattern?` == "Mediterranean Diet?", 1, 0)
prep_data$`asian diet` <- ifelse(data$`6. What is your dietary pattern?` == "Asian Diet", 1, 0)
prep_data$`korean diet` <- ifelse(data$`6. What is your dietary pattern?` == "Korean Diet", 1, 0)
prep_data$`hindu diet` <- ifelse(data$`6. What is your dietary pattern?` == "Hindu Diet", 1, 0)

#Convert exercise freq into categorical variable 
prep_data$`excercise per week` <- 0
prep_data$`excercise per week`[data$`7. On an average week, how many times do you exercise at least 20 minutes?` == "1 time"] <- 1
prep_data$`excercise per week`[data$`7. On an average week, how many times do you exercise at least 20 minutes?` == "2 times"] <- 2
prep_data$`excercise per week`[data$`7. On an average week, how many times do you exercise at least 20 minutes?` == "3 times"] <- 3
prep_data$`excercise per week`[data$`7. On an average week, how many times do you exercise at least 20 minutes?` == "4 times"] <- 4
prep_data$`excercise per week`[data$`7. On an average week, how many times do you exercise at least 20 minutes?` == "More than 4 times"] <- 5

#Convert dietary variables into categorical variable 
prep_data$`eat breakfast past week` <- 0
prep_data$`eat breakfast past week`[data$`8. In the past 7 days, how many times did you eat breakfast?` == "1 time"] <- 1
prep_data$`eat breakfast past week`[data$`8. In the past 7 days, how many times did you eat breakfast?` == "2 times"] <- 2
prep_data$`eat breakfast past week`[data$`8. In the past 7 days, how many times did you eat breakfast?` == "3 times"] <- 3
prep_data$`eat breakfast past week`[data$`8. In the past 7 days, how many times did you eat breakfast?` == "4 times"] <- 4
prep_data$`eat breakfast past week`[data$`8. In the past 7 days, how many times did you eat breakfast?` == "5 times"] <- 5
prep_data$`eat breakfast past week`[data$`8. In the past 7 days, how many times did you eat breakfast?` == "6 or 7 times"] <- 6

prep_data$`eat whole grain per day` <- 0
prep_data$`eat whole grain per day`[data$`9. On average week, how many times a day do you eat whole grain products?` == "1 time"] <- 1
prep_data$`eat whole grain per day`[data$`9. On average week, how many times a day do you eat whole grain products?` == "2 times"] <- 2
prep_data$`eat whole grain per day`[data$`9. On average week, how many times a day do you eat whole grain products?` == "3 times"] <- 3
prep_data$`eat whole grain per day`[data$`9. On average week, how many times a day do you eat whole grain products?` == "4 times"] <- 4
prep_data$`eat whole grain per day`[data$`9. On average week, how many times a day do you eat whole grain products?` == "More than 4 times"] <- 5

prep_data$`eat dairy per day` <- 0
prep_data$`eat dairy per day`[data$`10. On average week, how many times a day do you have dairy products?` == "1 time"] <- 1
prep_data$`eat dairy per day`[data$`10. On average week, how many times a day do you have dairy products?` == "2 times"] <- 2
prep_data$`eat dairy per day`[data$`10. On average week, how many times a day do you have dairy products?` == "3 times"] <- 3
prep_data$`eat dairy per day`[data$`10. On average week, how many times a day do you have dairy products?` == "4 times"] <- 4
prep_data$`eat dairy per day`[data$`10. On average week, how many times a day do you have dairy products?` == "More than 4 times"] <- 5

prep_data$`caffeine per day` <- 0
prep_data$`caffeine per day`[data$`11. On average week, how many times a day do you consume coffee or other sources of caffeine?`== "1 time"] <- 1
prep_data$`caffeine per day`[data$`11. On average week, how many times a day do you consume coffee or other sources of caffeine?` == "2 times"] <- 2
prep_data$`caffeine per day`[data$`11. On average week, how many times a day do you consume coffee or other sources of caffeine?` == "3 times"] <- 3
prep_data$`caffeine per day`[data$`11. On average week, how many times a day do you consume coffee or other sources of caffeine?` == "4 times"] <- 4
prep_data$`caffeine per day`[data$`11. On average week, how many times a day do you consume coffee or other sources of caffeine?` == "More than 4 times"] <- 5

prep_data$`fruits per day` <- 0
prep_data$`fruits per day`[data$`12. On average week, how many times a day do you consume fruits?`== "1 time"] <- 1
prep_data$`fruits per day`[data$`12. On average week, how many times a day do you consume fruits?` == "2 times"] <- 2
prep_data$`fruits per day`[data$`12. On average week, how many times a day do you consume fruits?` == "3 times"] <- 3
prep_data$`fruits per day`[data$`12. On average week, how many times a day do you consume fruits?` == "4 times"] <- 4
prep_data$`fruits per day`[data$`12. On average week, how many times a day do you consume fruits?` == "More than 4 times"] <- 5

prep_data$`flexedseeds_nuts per day` <- 0
prep_data$`flexedseeds_nuts per day`[data$`13. On average week, how many times a day do you consume flaxseed, and or nuts?`== "1 time"] <- 1
prep_data$`flexedseeds_nuts per day`[data$`13. On average week, how many times a day do you consume flaxseed, and or nuts?` == "2 times"] <- 2
prep_data$`flexedseeds_nuts per day`[data$`13. On average week, how many times a day do you consume flaxseed, and or nuts?` == "3 times?"] <- 3
prep_data$`flexedseeds_nuts per day`[data$`13. On average week, how many times a day do you consume flaxseed, and or nuts?` == "4 times"] <- 4
prep_data$`flexedseeds_nuts per day`[data$`13. On average week, how many times a day do you consume flaxseed, and or nuts?` == "More than 4 times"] <- 5


prep_data$`rice_pasta per day` <- 0
prep_data$`rice_pasta per day`[data$`14. On average week, how many times do you eat rice, and or pasta?`== "1 time"] <- 1
prep_data$`rice_pasta per day`[data$`14. On average week, how many times do you eat rice, and or pasta?` == "2 times"] <- 2
prep_data$`rice_pasta per day`[data$`14. On average week, how many times do you eat rice, and or pasta?` == "3 times"] <- 3
prep_data$`rice_pasta per day`[data$`14. On average week, how many times do you eat rice, and or pasta?` == "4 times"] <- 4
prep_data$`rice_pasta per day`[data$`14. On average week, how many times do you eat rice, and or pasta?` == "More than 4 times"] <- 5


prep_data$meat_chicken_turkey <- 0
prep_data$meat_chicken_turkey[data$`15. On average week, how times do you eat meat, chicken, or turkey?`== "1 time"] <- 1
prep_data$meat_chicken_turkey[data$`15. On average week, how times do you eat meat, chicken, or turkey?` == "2 times"] <- 2
prep_data$meat_chicken_turkey[data$`15. On average week, how times do you eat meat, chicken, or turkey?` == "3 times"] <- 3
prep_data$meat_chicken_turkey[data$`15. On average week, how times do you eat meat, chicken, or turkey?` == "4 times"] <- 4
prep_data$meat_chicken_turkey[data$`15. On average week, how times do you eat meat, chicken, or turkey?` == "More than 4 times"] <- 5


prep_data$vegetables <- 0
prep_data$vegetables[data$`16. On average week, how many times do you eat dark green leafy vegetables?`== "1 times"] <- 1
prep_data$vegetables[data$`16. On average week, how many times do you eat dark green leafy vegetables?` == "2 times"] <- 2
prep_data$vegetables[data$`16. On average week, how many times do you eat dark green leafy vegetables?` == "3 times"] <- 3
prep_data$vegetables[data$`16. On average week, how many times do you eat dark green leafy vegetables?` == "4 times"] <- 4
prep_data$vegetables[data$`16. On average week, how many times do you eat dark green leafy vegetables?` == "More than 4 times"] <- 5


prep_data$beans <- 0
prep_data$beans[data$`17. On average week, how many times do you eat beans?`== "1 time"] <- 1
prep_data$beans[data$`17. On average week, how many times do you eat beans?` == "2 times"] <- 2
prep_data$beans[data$`17. On average week, how many times do you eat beans?` == "3 times"] <- 3
prep_data$beans[data$`17. On average week, how many times do you eat beans?` == "4 times"] <- 4
prep_data$beans[data$`17. On average week, how many times do you eat beans?` == "More than 4 times"] <- 5


prep_data$`sea food` <- 0
prep_data$`sea food`[data$`18. On average week, how many times do you eat fish and shellfish (including sardines and tuna)?`== "1 time"] <- 1
prep_data$`sea food`[data$`18. On average week, how many times do you eat fish and shellfish (including sardines and tuna)?` == "2 times"] <- 2
prep_data$`sea food`[data$`18. On average week, how many times do you eat fish and shellfish (including sardines and tuna)?` == "3 times"] <- 3
prep_data$`sea food`[data$`18. On average week, how many times do you eat fish and shellfish (including sardines and tuna)?` == "4 times"] <- 4
prep_data$`sea food`[data$`18. On average week, how many times do you eat fish and shellfish (including sardines and tuna)?` == "More than 4 times"] <- 5


prep_data$`fast food` <- 0
prep_data$`fast food`[data$`19. On average week, how many times do you eat fast foods or pre-made food?`== "1 time"] <- 1
prep_data$`fast food`[data$`19. On average week, how many times do you eat fast foods or pre-made food?` == "2 times"] <- 2
prep_data$`fast food`[data$`19. On average week, how many times do you eat fast foods or pre-made food?` == "3 times"] <- 3
prep_data$`fast food`[data$`19. On average week, how many times do you eat fast foods or pre-made food?` == "4 times"] <- 4
prep_data$`fast food`[data$`19. On average week, how many times do you eat fast foods or pre-made food?` == "More than 4 times"] <- 5


prep_data$multivitamins <- 0
prep_data$multivitamins[data$`20. On average week, how many times do you take multivitamin supplements?`== "1 time"] <- 1
prep_data$multivitamins[data$`20. On average week, how many times do you take multivitamin supplements?` == "2 times"] <- 2
prep_data$multivitamins[data$`20. On average week, how many times do you take multivitamin supplements?` == "3 times"] <- 3
prep_data$multivitamins[data$`20. On average week, how many times do you take multivitamin supplements?` == "4 times"] <- 4
prep_data$multivitamins[data$`20. On average week, how many times do you take multivitamin supplements?` == "More than 4 times"] <- 5


prep_data$`fish oil` <- 0
prep_data$`fish oil`[data$`21. On average week, how many times do you take fish oil supplements?`== "1 time"] <- 1
prep_data$`fish oil`[data$`21. On average week, how many times do you take fish oil supplements?` == "2 times"] <- 2
prep_data$`fish oil`[data$`21. On average week, how many times do you take fish oil supplements?` == "3 times"] <- 3
prep_data$`fish oil`[data$`21. On average week, how many times do you take fish oil supplements?` == "4 times"] <- 4
prep_data$`fish oil`[data$`21. On average week, how many times do you take fish oil supplements?` == "More than 4 times"] <- 5

#Convert mental distress variables into categorical variable 
prep_data$nervous <- 0
prep_data$nervous[data$`22. During the past month, about how often did you feel NERVOUS?`== "A little of the time"] <- 1
prep_data$nervous[data$`22. During the past month, about how often did you feel NERVOUS?` == "Some of the time"] <- 2
prep_data$nervous[data$`22. During the past month, about how often did you feel NERVOUS?` == "Most of the time"] <- 3
prep_data$nervous[data$`22. During the past month, about how often did you feel NERVOUS?` == "All the time"] <- 4

prep_data$hopeless <- 0
prep_data$hopeless[data$`23. During the past month, about how often did you feel HOPELESS?`== "A little of the time"] <- 1
prep_data$hopeless[data$`23. During the past month, about how often did you feel HOPELESS?` == "Some of the time"] <- 2
prep_data$hopeless[data$`23. During the past month, about how often did you feel HOPELESS?` == "Most of the time"] <- 3
prep_data$hopeless[data$`23. During the past month, about how often did you feel HOPELESS?` == "All the time"] <- 4


prep_data$restless_fidgety <- 0
prep_data$restless_fidgety[data$`24. During the past month, about how often did you feel RESTLESS or FIDGETY?`== "A little of the time"] <- 1
prep_data$restless_fidgety[data$`24. During the past month, about how often did you feel RESTLESS or FIDGETY?` == "Some of the time"] <- 2
prep_data$restless_fidgety[data$`24. During the past month, about how often did you feel RESTLESS or FIDGETY?` == "Most of the time"] <- 3
prep_data$restless_fidgety[data$`24. During the past month, about how often did you feel RESTLESS or FIDGETY?` == "All the time"] <- 4


prep_data$`too depressed` <- 0
prep_data$`too depressed`[data$`25. During the past month, about how often did you feel SO DEPRESSED THAT NOTHING COULD CHEER YOU UP?`== "A little of the time"] <- 1
prep_data$`too depressed`[data$`25. During the past month, about how often did you feel SO DEPRESSED THAT NOTHING COULD CHEER YOU UP?` == "Some of the time"] <- 2
prep_data$`too depressed`[data$`25. During the past month, about how often did you feel SO DEPRESSED THAT NOTHING COULD CHEER YOU UP?` == "Most of the time"] <- 3
prep_data$`too depressed`[data$`25. During the past month, about how often did you feel SO DEPRESSED THAT NOTHING COULD CHEER YOU UP?` == "All the time"] <- 4


prep_data$effort <- 0
prep_data$effort[data$`26. During the past month, about how often did you feel THAT EVERYTHING WAS AN EFFORT?`== "A little of the time"] <- 1
prep_data$effort[data$`26. During the past month, about how often did you feel THAT EVERYTHING WAS AN EFFORT?` == "Some of the time"] <- 2
prep_data$effort[data$`26. During the past month, about how often did you feel THAT EVERYTHING WAS AN EFFORT?` == "Most of the time"] <- 3
prep_data$effort[data$`26. During the past month, about how often did you feel THAT EVERYTHING WAS AN EFFORT?` == "All the time"] <- 4


prep_data$worthless <- 0
prep_data$worthless[data$`27. During the past month, about how often did you feel WORTHLESS?`== "A little of the time"] <- 1
prep_data$worthless[data$`27. During the past month, about how often did you feel WORTHLESS?` == "Some of the time"] <- 2
prep_data$worthless[data$`27. During the past month, about how often did you feel WORTHLESS?` == "Most of the time"] <- 3
prep_data$worthless[data$`27. During the past month, about how often did you feel WORTHLESS?` == "All the time"] <- 4


prep_data$`close to gym` <- 1
prep_data$`close to gym`[data$`28. How close are you to the gym`== "Driving distance (under 15 minutes)"] <- 2
prep_data$`close to gym`[data$`28. How close are you to the gym` == "Walking distance (15 minutes+)"] <- 3
prep_data$`close to gym`[data$`28. How close are you to the gym` == "Driving distance (15 minutes+)"] <- 4

prep_data$`excercise` <- 0
prep_data$`excercise`[data$`30. How many hours a week do you exercise`== "1-2 hours"] <- 1
prep_data$`excercise`[data$`30. How many hours a week do you exercise` == "3-4 hours"] <- 2
prep_data$`excercise`[data$`30. How many hours a week do you exercise` == "5-6 hours"] <- 3
prep_data$`excercise`[data$`30. How many hours a week do you exercise` == "6+ hours"] <- 4


prep_data$athlete <- 0
prep_data$athlete[data$`31. Are you on an Athletics team? If so what level?`== "Club sports"] <- 1
prep_data$athlete[data$`31. Are you on an Athletics team? If so what level?` == "Intramural sports"] <- 2
prep_data$athlete[data$`31. Are you on an Athletics team? If so what level?` == "Division 1 athelte"] <- 3


prep_data$`excercise_time` <- 1
prep_data$`excercise_time`[data$`32. What time of day do you usually exercise?`== "Noon (10:00-1:00)"] <- 2
prep_data$`excercise_time`[data$`32. What time of day do you usually exercise?` == "Afternoon (1:00-4:00)"] <- 3
prep_data$`excercise_time`[data$`32. What time of day do you usually exercise?` == "Evening (4:00-7:00)"] <- 4
prep_data$`excercise_time`[data$`32. What time of day do you usually exercise?` == "Night (After 7:00pm)"] <- 5


prep_data$`sleep_time` <- 1
prep_data$`sleep_time`[data$`33. How many hours of sleep do you usually get?`== "7 hours"] <- 2
prep_data$`sleep_time`[data$`33. How many hours of sleep do you usually get?` == "8 hours"] <- 3
prep_data$`sleep_time`[data$`33. How many hours of sleep do you usually get?` == "9 hours"] <- 4
prep_data$`sleep_time`[data$`33. How many hours of sleep do you usually get?` == "10 hours+"] <- 5

prep_data$`excercise_type` <- 0
prep_data$excercise_type[data$`35. What kind of exercise do you usually do.`== "Cardio"] <- 1
prep_data$excercise_type[data$`35. What kind of exercise do you usually do.` == "Weight Training"] <- 2
prep_data$excercise_type[data$`35. What kind of exercise do you usually do.` == "Both"] <- 3


prep_data$`excercise_location` <- 0
prep_data$excercise_location[data$`36. Do you usually exercise inside or outside?`== "Outside"] <- 1

prep_data$`excercise_freq_before_quarantine` <- 0
prep_data$`excercise_freq_before_quarantine`[data$`How many days a week did you exercise before COVID-19 quarantine?`== 1] <- 1
prep_data$excercise_freq_before_quarantine[data$`How many days a week did you exercise before COVID-19 quarantine?`== 2] <- 2
prep_data$excercise_freq_before_quarantine[data$`How many days a week did you exercise before COVID-19 quarantine?`== 3] <- 3
prep_data$excercise_freq_before_quarantine[data$`How many days a week did you exercise before COVID-19 quarantine?`== 4] <- 4
prep_data$excercise_freq_before_quarantine[data$`How many days a week did you exercise before COVID-19 quarantine?`== 5] <- 5
prep_data$excercise_freq_before_quarantine[data$`How many days a week did you exercise before COVID-19 quarantine?`== 6] <- 6
prep_data$excercise_freq_before_quarantine[data$`How many days a week did you exercise before COVID-19 quarantine?`== 7] <- 7


prep_data$`excercise_freq_in_quarantine` <- 0# never workout
prep_data$`excercise_freq_in_quarantine`[data$`How many days a week did you exercise during quarantine?`== 1] <- 1
prep_data$excercise_freq_in_quarantine[data$`How many days a week did you exercise during quarantine?`== 2] <- 2
prep_data$excercise_freq_in_quarantine[data$`How many days a week did you exercise during quarantine?`== 3] <- 3
prep_data$excercise_freq_in_quarantine[data$`How many days a week did you exercise during quarantine?`== 4] <- 4
prep_data$excercise_freq_in_quarantine[data$`How many days a week did you exercise during quarantine?`== 6] <- 6
prep_data$excercise_freq_in_quarantine[data$`How many days a week did you exercise during quarantine?`== 7] <- 7

prep_data$`excercise_sess_before_covid` <- 0 #less than 1 hour
prep_data$`excercise_sess_before_covid`[(data$`How long were your workout sessions before COVID-19 quarantine?`== "1 hour") | (data$`How long were your workout sessions before COVID-19 quarantine?`== "Hour and a half") | (data$`How long were your workout sessions before COVID-19 quarantine?`== "~ 1.5 hours")] <- 1 # 1 or 2 hours
prep_data$excercise_sess_before_covid[(data$`How long were your workout sessions before COVID-19 quarantine?`== "2 hours") | (data$`How long were your workout sessions before COVID-19 quarantine?`== "3 hours") | (data$`How long were your workout sessions before COVID-19 quarantine?`== "2-3")] <- 2 # 3 or 4 hours
prep_data$excercise_sess_before_covid[data$`How long were your workout sessions before COVID-19 quarantine?`== "8 hours"] <- 3 # more than 4 hours


prep_data$`excercise_sess_in_covid` <- 0 #less than 1 hour
prep_data$`excercise_sess_in_covid`[(data$`How long were your workout sessions during quarantine?`== "1 hour") | (data$`How long were your workout sessions during quarantine?`== "2 hours")] <- 1 # 1 or 2 hours
prep_data$excercise_sess_in_covid[data$`How long were your workout sessions during quarantine?`== "8 hours"] <- 2 # more than 3 hours


prep_data$`happiness_before_quarantine` <- 0
prep_data$`happiness_before_quarantine`[data$`On a scale to 1-10 (10 being the highest), how happy were you before COVID-19 quarantine?`== 1] <- 1 
prep_data$happiness_before_quarantine[data$`On a scale to 1-10 (10 being the highest), how happy were you before COVID-19 quarantine?`== 2] <- 2
prep_data$`happiness_before_quarantine`[data$`On a scale to 1-10 (10 being the highest), how happy were you before COVID-19 quarantine?`== 3] <- 3 
prep_data$happiness_before_quarantine[data$`On a scale to 1-10 (10 being the highest), how happy were you before COVID-19 quarantine?`== 4] <- 4 
prep_data$`happiness_before_quarantine`[data$`On a scale to 1-10 (10 being the highest), how happy were you before COVID-19 quarantine?`== 5] <- 5 
prep_data$happiness_before_quarantine[data$`On a scale to 1-10 (10 being the highest), how happy were you before COVID-19 quarantine?`== 6] <- 6 
prep_data$`happiness_before_quarantine`[data$`On a scale to 1-10 (10 being the highest), how happy were you before COVID-19 quarantine?`== 7] <- 7 
prep_data$happiness_before_quarantine[data$`On a scale to 1-10 (10 being the highest), how happy were you before COVID-19 quarantine?`== 8] <- 8 
prep_data$`happiness_before_quarantine`[data$`On a scale to 1-10 (10 being the highest), how happy were you before COVID-19 quarantine?`== 9] <- 9 
prep_data$happiness_before_quarantine[data$`On a scale to 1-10 (10 being the highest), how happy were you before COVID-19 quarantine?`== 10] <- 10 


prep_data$`happiness_in_quarantine` <- 0
prep_data$`happiness_in_quarantine`[data$`On a scale of 1-10 , how happy were you during quarantine?`== 1] <- 1 
prep_data$happiness_in_quarantine[data$`On a scale of 1-10 , how happy were you during quarantine?`== 2] <- 2
prep_data$`happiness_in_quarantine`[data$`On a scale of 1-10 , how happy were you during quarantine?`== 3] <- 3 
prep_data$happiness_in_quarantine[data$`On a scale of 1-10 , how happy were you during quarantine?`== 4] <- 4 
prep_data$`happiness_in_quarantine`[data$`On a scale of 1-10 , how happy were you during quarantine?`== 5] <- 5 
prep_data$happiness_in_quarantine[data$`On a scale of 1-10 , how happy were you during quarantine?`== 6] <- 6 
prep_data$`happiness_in_quarantine`[data$`On a scale of 1-10 , how happy were you during quarantine?`== 7] <- 7 
prep_data$happiness_in_quarantine[data$`On a scale of 1-10 , how happy were you during quarantine?`== 8] <- 8 
prep_data$`happiness_in_quarantine`[data$`On a scale of 1-10 , how happy were you during quarantine?`== 9] <- 9 
prep_data$happiness_in_quarantine[data$`On a scale of 1-10 , how happy were you during quarantine?`== 10] <- 10 


prep_data$depression_before_quarantine <- 0 # less than 1
prep_data$`depression_before_quarantine`[(data$`Before COVID-19 quarantine how many times a week did you feel depressed?`== "1") | (data$`Before COVID-19 quarantine how many times a week did you feel depressed?`== "2") | (data$`Before COVID-19 quarantine how many times a week did you feel depressed?`== "1 tops") | (data$`Before COVID-19 quarantine how many times a week did you feel depressed?`== "1-2") | (data$`Before COVID-19 quarantine how many times a week did you feel depressed?`== "1 tops") | (data$`Before COVID-19 quarantine how many times a week did you feel depressed?`== "I") | (data$`Before COVID-19 quarantine how many times a week did you feel depressed?`== "1 tops") | (data$`Before COVID-19 quarantine how many times a week did you feel depressed?`== "1 or so")] <- 1 # 1 or 2 times
prep_data$depression_before_quarantine[(data$`Before COVID-19 quarantine how many times a week did you feel depressed?`== "3") | (data$`Before COVID-19 quarantine how many times a week did you feel depressed?`== "4")] <- 2 # 3 or 4 times
prep_data$depression_before_quarantine[(data$`Before COVID-19 quarantine how many times a week did you feel depressed?`== "5") | (data$`Before COVID-19 quarantine how many times a week did you feel depressed?`== "6")] <- 3 # 5 or 6 times
prep_data$depression_before_quarantine[(data$`Before COVID-19 quarantine how many times a week did you feel depressed?`== "7") | (data$`Before COVID-19 quarantine how many times a week did you feel depressed?`== "10")] <- 4 # more than 6 times


prep_data$depression_in_quarantine <- 0 # less than 1
prep_data$`depression_in_quarantine`[(data$`During quarantine how many times a week did you feel depressed?`== 1) | (data$`During quarantine how many times a week did you feel depressed?`== 2)] <- 1 # 1 or 2 times
prep_data$depression_in_quarantine[(data$`During quarantine how many times a week did you feel depressed?`== 3) | (data$`During quarantine how many times a week did you feel depressed?`== 4)] <- 2 # 3 or 4 times
prep_data$depression_in_quarantine[(data$`During quarantine how many times a week did you feel depressed?`== 5) | (data$`During quarantine how many times a week did you feel depressed?`== 6)] <- 3 # 5 or 6 times
prep_data$depression_in_quarantine[(data$`During quarantine how many times a week did you feel depressed?`>= 7)] <- 4 # more than 6 times


prep_data$excercise_diffiulty_in_covid <- 0 # none, not applicable
prep_data$`excercise_diffiulty_in_covid`[(data$`Do you feel that COVID-19 made it difficult to exercise?`== "Yes")] <- 1 # Yes
prep_data$`excercise_diffiulty_in_covid`[(data$`Do you feel that COVID-19 made it difficult to exercise?`== "No")] <- 2 # No
prep_data$`excercise_diffiulty_in_covid`[(data$`Do you feel that COVID-19 made it difficult to exercise?`== "Maybe")] <- 3 # Maybe


#Deviding COVID-19 into three stages: before COVID-19, during COVID-10 quarantine, late COVID-19 (after the quarantine)
prep_data$covid_dur_str <- "in"
prep_data$covid_dur_str[prep_data$timestamp < "2020-03-19"] <- "pre"
prep_data$covid_dur_str[prep_data$timestamp > "2021-05-01"] <- "post"

prep_data$covid_dur <- 1
prep_data$covid_dur[prep_data$covid_dur_str == "pre"] <- 0
prep_data$covid_dur[prep_data$covid_dur_str == "post"] <- 2

write.csv(prep_data,"../prepared data.csv", row.names = FALSE)
write_xlsx(prep_data,"../prepared data.xlsx")














