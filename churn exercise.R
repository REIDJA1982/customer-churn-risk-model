
## churn exercise at https://towardsdatascience.com/hands-on-churn-prediction-with-r-and-comparison-of-different-models-for-churn-prediction-4b79011a082a // 

WA_Fn.UseC_.Telco.Customer.Churn <- read.csv("~/Documents/r practice/WA_Fn-UseC_-Telco-Customer-Churn.csv", row.names=1)

View(WA_Fn.UseC_.Telco.Customer.Churn)

library(tidyverse)
library(lubridate)
library(dplyr)
library(ggplot2)
library(plyr)  
library(rpart.plot) 
library(caret)
library(gridExtra) 
library(rsample)
library(e1071) 
library(GGally)
library(data.table)
library(DT)
library(readr)
library(tidyr)
library(corrplot)
library(rms)
library(MASS)
library(e1071)
library(ROCR)
library(gplots)
library(pROC)
library(rpart)
library(randomForest)
library(ggpubr)

## remove NAs ##

churn <- WA_Fn.UseC_.Telco.Customer.Churn[complete.cases(WA_Fn.UseC_.Telco.Customer.Churn),]

glimpse(churn)

## remove extra words from YES NO variables ##

churn <- data.frame(lapply(churn, function(x) {
  gsub("No internet service", "No", x)}))
churn <- data.frame(lapply(churn, function(x) {
  gsub("No phone service", "No", x)}))

## change senior cits column from 0 1 to Y N ##
  
churn$SeniorCitizen <- as.factor(ifelse(churn$SeniorCitizen==1, 'YES', 'NO'))

## convert dbl type variables into numeric and give them data frames ##

num_columns <- c("tenure", "MonthlyCharges", "TotalCharges")
churn[num_columns] <- sapply(churn[num_columns], as.numeric)
data_int <- churn[,c("tenure", "MonthlyCharges", "TotalCharges")]
data_int <- data.frame(scale(data_int))

## tenure is in months, convert it into years ##
  
churn <- mutate(churn, tenure_year = tenure)
churn$tenure_year[churn$tenure_year >=0 & churn$tenure_year <= 12] <- '0-1 year'
churn$tenure_year[churn$tenure_year > 12 & churn$tenure_year <= 24] <- '1-2 years'
churn$tenure_year[churn$tenure_year > 24 & churn$tenure_year <= 36] <- '2-3 years'
churn$tenure_year[churn$tenure_year > 36 & churn$tenure_year <= 48] <- '3-4 years'
churn$tenure_year[churn$tenure_year > 48 & churn$tenure_year <= 60] <- '4-5 years'
churn$tenure_year[churn$tenure_year > 60 & churn$tenure_year <= 72] <- '5-6 years'
churn$tenure_year <- as.factor(churn$tenure_year)

## take a look at the data ##

summary(churn)

## check for class imbalance of Y variable ##

barplot(prop.table(table(churn$Churn)),
        col = rainbow(2),
        ylim = c(0, 0.7),
        main = "Class Distribution")

## How many rows contain missing values? ##

cat(paste("Proportion of rows with missing values:", sum(!complete.cases(churn)) / nrow(churn)))


## We have none, but note that the `summary()` function shows where the missing values are, and also flags that the `age` column has some negative values.

## let's take a look at monthly spend

hist(churn$MonthlyCharges, main = "Monthly charges of client", xlab = "monthly charges", ylab = "clients")

## how many unique genders are there in the data

unique(churn$gender)

## make a new data frame to look at something and plot it

# Store the regional medians in a new data.frame
areaunit_medians <- churn %>%
  group_by(tenure_year) %>% 
  summarise(med = median(TotalCharges, na.rm = TRUE))

