# predicting churn risk

# access packages 
  
library(dplyr)
library(magrittr)
library(ggplot2)
library(caret)
library(recipes)

# import and rename the data ##

WA_Fn.UseC_.Telco.Customer.Churn <- read.csv("~/Documents/r practice/WA_Fn-UseC_-Telco-Customer-Churn.csv")
  
View(WA_Fn.UseC_.Telco.Customer.Churn)

churndata <- WA_Fn.UseC_.Telco.Customer.Churn

## take a look at it

str(churndata)

summary(churndata)

na_counts <- sapply(churndata, function(x) sum(is.na(x)))

na_counts

# the data is pretty clean but their are 11 rows with TotalCharges missing. //
# I am going to drop those rows because it is probably not costly. //

churndata <- na.omit(churndata)

# ok we have clean data so lets profile our independent variables against our Churn variable

table(churndata$gender)

prop_gender <- churndata %>%
  group_by(gender) %>%
  summarize(prop = mean(Churn == "Yes")) 

ggplot(prop_gender, aes(x = gender, y = prop)) +
  geom_bar(stat = "identity")

# not much difference in churn rates between males and females

table(churndata$SeniorCitizen)

prop_sencit <- churndata %>%
  group_by(SeniorCitizen) %>%
  summarize(prop = mean(Churn == "Yes")) 

ggplot(prop_sencit, aes(x = SeniorCitizen, y = prop)) +
  geom_bar(stat = "identity")

# senior citizens twice as likely to churndata. potentially more price-sensitive and have time on their hands to hunt for bargains.

table(churndata$Partner)

prop_partner <- churndata %>%
  group_by(Partner) %>%
  summarize(prop = mean(Churn == "Yes")) 

ggplot(prop_partner, aes(x = Partner, y = prop)) +
  geom_bar(stat = "identity")

# customers with partner are less likely to churn

table(churndata$Dependents)

prop_dependent <- churndata %>%
  group_by(Dependents) %>%
  summarize(prop = mean(Churn == "Yes")) 

ggplot(prop_dependent, aes(x = Dependents, y = prop)) +
  geom_bar(stat = "identity")

# people with dependents are less likely to churn. Potentially less time to search for other providers and switch.

table(churndata$InternetService)

prop_internet <- churndata %>%
  group_by(InternetService) %>%
  summarize(prop = mean(Churn == "Yes")) 

ggplot(prop_internet, aes(x = InternetService, y = prop)) + 
  geom_bar(stat = "identity")

# customers with fibre are most likely to churn, followed by those with DSL. Customers w/o internet are unlikely to churn

table(churndata$PhoneService)

prop_phone <- churndata %>%
  group_by(PhoneService) %>%
  summarize(prop = mean(Churn == "Yes")) 

ggplot(prop_phone, aes(x = PhoneService, y = prop)) +
  geom_bar(stat = "identity")

# no difference in churn for clients with/without phone service

table(churndata$MultipleLines)

prop_multilines <- churndata %>%
  group_by(MultipleLines) %>%
  summarize(prop = mean(Churn == "Yes")) 

ggplot(prop_multilines, aes(x = MultipleLines, y = prop)) +
  geom_bar(stat = "identity")

# marginal increase in churn for customers with multiple lines

table(churndata$OnlineSecurity)

prop_onlinesec <- churndata %>%
  group_by(OnlineSecurity) %>%
  summarize(prop = mean(Churn == "Yes")) 

ggplot(prop_onlinesec, aes(x = OnlineSecurity, y = prop)) + 
  geom_bar(stat = "identity")

# clients that dont purchase online security (but with internet service) are more likely to switch. Potentially less tolerant to risk.

table(churndata$OnlineBackup)

prop_onlinebu <- churndata %>%
  group_by(OnlineBackup) %>%
  summarize(prop = mean(Churn == "Yes")) 

ggplot(prop_onlinebu, aes(x = OnlineBackup, y = prop)) + 
  geom_bar(stat = "identity")
 
# similarly, customers that dont purchase online backup are more likely to churn

table(churndata$DeviceProtection)

prop_devprot <- churndata %>%
  group_by(DeviceProtection) %>%
  summarize(prop = mean(Churn == "Yes")) 

ggplot(prop_devprot, aes(x = DeviceProtection, y = prop)) + 
  geom_bar(stat = "identity")

# similarly, customers that dont purchase device protection are more likely to churn

table(churndata$TechSupport)

prop_techsup <- churndata %>%
  group_by(TechSupport) %>%
  summarize(prop = mean(Churn == "Yes")) 

ggplot(prop_techsup, aes(x = TechSupport, y = prop)) + 
  geom_bar(stat = "identity")

# similarly, customers that dont purchase tech support are more likely to churn

table(churndata$StreamingTV)

prop_streamtv <- churndata %>%
  group_by(StreamingTV) %>%
  summarize(prop = mean(Churn == "Yes")) 

ggplot(prop_streamtv, aes(x = StreamingTV, y = prop)) + 
  geom_bar(stat = "identity")

# similar levels of churn between clients with and without streaming TV

table(churndata$StreamingMovies)

prop_strmovies <- churndata %>%
  group_by(StreamingMovies) %>%
  summarize(prop = mean(Churn == "Yes")) 

ggplot(prop_strmovies, aes(x = StreamingMovies, y = prop)) + 
  geom_bar(stat = "identity")


# similar levels of churn between clients with and without streaming movies

table(churndata$Contract)

prop_contract <- churndata %>%
  group_by(Contract) %>%
  summarize(prop = mean(Churn == "Yes")) 

ggplot(prop_contract, aes(x = Contract, y = prop)) + 
  geom_bar(stat = "identity")

# month to month contracts are most likely to churn 

table(churndata$PaperlessBilling)

prop_paperless <- churndata %>%
  group_by(PaperlessBilling) %>%
  summarize(prop = mean(Churn == "Yes")) 

ggplot(prop_paperless, aes(x = PaperlessBilling, y = prop)) + 
  geom_bar(stat = "identity")

# paperless billing more likely to churn. 

table(churndata$PaymentMethod)

prop_payment <- churndata %>%
  group_by(PaymentMethod) %>%
  summarize(prop = mean(Churn == "Yes")) 

ggplot(prop_payment, aes(x = PaymentMethod, y = prop)) + 
  geom_bar(stat = "identity")

# electronic check are more likely to churn

table(churndata$tenure)

prop_tenure <- churndata %>%
  group_by(tenure) %>%
  summarize(prop = mean(Churn == "Yes")) 

ggplot(prop_tenure, aes(x = tenure, y = prop)) + 
  geom_bar(stat = "identity")

# customers with longer tenure are less likely to churn

# now i have two continous variables that I want to profile against churn. lets take a look at them.

summary(churndata$MonthlyCharges)

# first I will add a new column to my churndata representing my bins //

churndata$MonthlyChargesRange <- cut(churndata$MonthlyCharges, breaks = c(0, 20, 40, 60, 80, 100, 120), labels = c("0-20", "20-40", "40-60", "60-80", "80-100", "100-120"))



prop_mc <- churndata %>%
  group_by(MonthlyChargesRange) %>%
  summarize(prop = mean(Churn == "Yes")) 

table(churndata$MonthlyChargesRange)

counts_mc <- as.data.frame(table(churndata$MonthlyChargesRange))

colnames(counts_mc) <- c("MonthlyChargesRange", "count")

View(counts_mc)

prop_mc <- merge(prop_mc, counts_mc, by = "MonthlyChargesRange", all.x = TRUE)

View(prop_mc)

ggplot(data = prop_mc, aes(x = MonthlyChargesRange, y = prop)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste("n=", count)), size = 3, vjust = -0.5)
print(p)

# churn rate is increasing with monthly charges up to $80-100 per month, then decreasing for the $100-120 bin.

summary(churndata$TotalCharges)

# this one has a wide distribution. I am going to bin it into deciles to start with. 
  
# Calculate the decile breaks
decile_breaks <- quantile(churndata$TotalCharges, probs = seq(0, 1, 0.1))

# Create a vector of labels for the decile bins
decile_labels <- paste0("Decile ", 1:10)

# Bin TotalCharges by decile
churndata$TotalChargesDeciles <- cut(churndata$TotalCharges, breaks = c(-Inf, decile_breaks, Inf), labels = c("<", decile_labels, ">"))


prop_tc <- churndata %>%
  group_by(TotalChargesDeciles) %>%
  summarize(prop = mean(Churn == "Yes")) 

View(prop_tc)
table(churndata$TotalChargesDecile)

counts_tc <- as.data.frame(table(churndata$TotalChargesDeciles))

colnames(counts_tc) <- c("TotalChargesDeciles", "count")

View(counts_tc)

prop_tc <- merge(prop_tc, counts_tc, by = "TotalChargesDeciles", all.x = TRUE)

prop_tc <- prop_tc[prop_tc$TotalChargesDeciles != "<", ]


View(prop_tc)

ggplot(data = prop_tc, aes(x = TotalChargesDeciles, y = prop)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste("n=", count)), size = 3, vjust = -0.5)

# this looks mostly fairly well behaved with constant decreased in churn rate as total charges increase, except for Decile 7 which is out of whack.

# overall we have a bunch of variables that look like they might help to predict churn. We have a good chance of finding some at-risk clients. 

# next I'd like to check for correlation between some of my independent variables that look like there is potential for multicollinearity in case it causes problems for my subsequent predictive modeling

contingency_secbu <- table(churndata$OnlineSecurity, churndata$OnlineBackup)
print(contingency_secbu)

contingency_sects <- table(churndata$OnlineSecurity, churndata$TechSupport)
print(contingency_sects)

contingency_secdp <- table(churndata$OnlineSecurity, churndata$DeviceProtection)
print(contingency_secdp)

contingency_pappay <- table(churndata$PaperlessBilling, churndata$PaymentMethod)
print(contingency_pappay)

cor(churndata$MonthlyCharges, churndata$TotalCharges)

# there are no obvious problems. There is a moderate level of correlation between MonthlyCharges and Total charges of 0.65. that should not be a problem.

## I'm going to start modelling. 

# Split the data into a training set and a testing set
set.seed(123)  # Set the seed for reproducibility
split <- createDataPartition(churndata$Churn, p = 0.7, list = FALSE)
train <- churndata[split, ]  # Training set
test <- churndata[-split, ]  # Testing set

# train some models


# Set up a list of the models to train
models <- list(
  logistic = train(Churn ~ . - TotalCharges - MonthlyCharges, data = train, method = "glm", family = "binomial"),
  tree = train(Churn ~ . - TotalCharges - MonthlyCharges, data = train, method = "rpart"),
  forest = train(Churn ~ . - TotalCharges - MonthlyCharges, data = train, method = "rf")
)
  
# Evaluate the models on the testing set
results <- lapply(models, function(x) predict(x, test))
names(results) <- names(models)