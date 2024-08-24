
library(tidyr)
library(stringr)
library(readr)
library(ggplot2)
library(dplyr)
library(readxl)
library(data.table)
library(pastecs)
library(psych)
library(lfe)
library(broom)
library(stargazer)
library(comprehenr)
library(plm)

data <- read_csv("C:/Users/Anseh/Downloads/prepared data2.csv")

cols <- colnames(data)[2:73]

data[cols] <- lapply(data[cols], factor)

# data$mental_distres <- "no"
# data$mental_distres[data$mental_disorder_score >= 1 & data$mental_disorder_score <= 5] <- "low"
# data$mental_distres[data$mental_disorder_score >= 6 & data$mental_disorder_score <= 10] <- "moderate"
# data$mental_distres[data$mental_disorder_score >= 11] <- "high"

data$mental_distres <- 0
data$mental_distres[data$mental_disorder_score >= 1 & data$mental_disorder_score <= 5] <- 1
data$mental_distres[data$mental_disorder_score >= 6 & data$mental_disorder_score <= 10] <- 2
data$mental_distres[data$mental_disorder_score >= 11] <- 3

data$exercise_freq <- 0
data$exercise_freq[data$eat_breakfast_past_week >= 2 & data$eat_breakfast_past_week <4] <- 1
data$exercise_freq[data$eat_breakfast_past_week >= 4] <- 2

data$breakfast_freq <- 0
data$breakfast_freq[data$exercise_dur_per_week >= 2 & data$exercise_dur_per_week <4] <- 1
data$breakfast_freq[data$exercise_dur_per_week >= 4 & data$exercise_dur_per_week <6] <- 2
data$breakfast_freq[data$exercise_dur_per_week >= 6] <- 3

data$caffeine_freq <- 0
data$caffeine_freq[data$caffeine_per_day >= 2 & data$caffeine_per_day <4] <- 1
data$caffeine_freq[data$caffeine_per_day >= 4] <- 2

data$dairy_freq <- 0
data$dairy_freq[data$eat_dairy_per_day >= 2 & data$eat_dairy_per_day <4] <- 1
data$dairy_freq[data$eat_dairy_per_day >= 4] <- 2

data$meat_freq <- 0
data$meat_freq[data$meat_chicken_turkey >= 2 & data$meat_chicken_turkey <4] <- 1
data$meat_freq[data$meat_chicken_turkey >= 4] <- 2

data$seafood_freq <- 0
data$seafood_freq[data$sea_food >= 2 & data$sea_food <4] <- 1
data$seafood_freq[data$sea_food >= 4] <- 2

data$fastfood_freq <- 0
data$fastfood_freq[data$fast_food >= 2 & data$fast_food <4] <- 1
data$fastfood_freq[data$fast_food >= 4] <- 2

data$HGI_freq <- 0
data$HGI_freq[data$rice_pasta_per_day>= 2 & data$rice_pasta_per_day <4] <- 1
data$HGI_freq[data$rice_pasta_per_day >= 4] <- 2

data$whole_grain_freq <- 0
data$whole_grain_freq[data$eat_whole_grain_per_day>= 2 & data$eat_whole_grain_per_day <4] <- 1
data$whole_grain_freq[data$eat_whole_grain_per_day >= 4] <- 2

data$fruit_freq <- 0
data$fruit_freq[data$fruits_per_day>= 2 & data$fruits_per_day <4] <- 1
data$fruit_freq[data$fruits_per_day >= 4] <- 2

data$flaxseed_freq <- 0
data$flaxseed_freq[data$flexedseeds_nuts_per_day>= 2 & data$flexedseeds_nuts_per_day <4] <- 1
data$flaxseed_freq[data$flexedseeds_nuts_per_day >= 4] <- 2

data$vegetable_freq <- 0
data$vegetable_freq[data$vegetables>= 2 & data$vegetables <4] <- 1
data$vegetable_freq[data$vegetables >= 4] <- 2

data$beans_freq <- 0
data$beans_freq[data$beans>= 2 & data$beans <4] <- 1
data$beans_freq[data$beans >= 4] <- 2

data$sleep_freq <- 0
data$sleep_freq[data$sleep_time>= 2 & data$sleep_time <4] <- 1
data$sleep_freq[data$sleep_time >= 4] <- 2

data$did = data$covid_dur * data$exercise_freq

didreg = lm(mental_distres ~ breakfast_freq+caffeine_freq+dairy_freq+meat_freq+seafood_freq+fastfood_freq+HGI_freq+whole_grain_freq+fruit_freq+flaxseed_freq+vegetable_freq+beans_freq+sleep_freq +covid_dur * exercise_freq, data = data)
summary(didreg)

didreg1 = lm(mental_distres ~ breakfast_freq+caffeine_freq+dairy_freq+meat_freq+seafood_freq+fastfood_freq+HGI_freq+whole_grain_freq+fruit_freq+flaxseed_freq+vegetable_freq+beans_freq+sleep_freq +did, data = data)
summary(didreg1)

