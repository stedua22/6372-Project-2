# Packages
library(tidyverse)

# Pull in Data
data<-read.csv("C:/Users/aabro/OneDrive/Desktop/SMU Program/Classes/Stats 2/Final Project/bank-additional-full.csv",stringsAsFactors = T, sep=";")

# Split into training and test
train_perc <- .8
num_rows <- nrow(data)
num_train_rows <- round(train_perc*num_rows)
train_ind <- 1:num_train_rows
train_data <- data[train_ind, ] # 32950
test_data <- data[-train_ind, ] # 8238
yes_data <- train_data[train_data$y == 'yes',] # 2100
no_data <- train_data[train_data$y == 'no',] # 30850
yes_data_test <- test_data[test_data$y == 'yes',] # 2540
no_data_test <- test_data[test_data$y == 'no',] # 5698

# It says that duration provides too much information for a predictive model
train_data$duration <- c()

# Which variables have an "unknown" value?
summary(train_data$age)
summary(train_data$job) # not a lot of unknown
summary(train_data$marital) # not a lot of unknown
summary(train_data$education) # not a lot of unknown
summary(train_data$default) # lots of unknown
summary(train_data$housing) # not a lot of unknown
summary(train_data$loan) # not a lot of unknown
summary(train_data$campaign)
hist(train_data$campaign)
hist(train_data$campaign, breaks=50) # lots of 1 values
summary(train_data$pdays) # lots of 999 values
hist(train_data$pdays)
summary(train_data$previous) # lots of 0
summary(train_data$poutcome) # lots of nonexistent
summary(train_data$emp.var.rate)
hist(train_data$emp.var.rate)
hist(train_data$emp.var.rate, breaks=50)
summary(train_data$cons.price.idx)
hist(train_data$cons.price.idx, breaks=50)
summary(train_data$cons.conf.idx)
hist(train_data$cons.conf.idx, breaks=50)
summary(train_data$euribor3m)
hist(train_data$euribor3m, breaks=50)
summary(train_data$nr.employed)
hist(train_data$nr.employed, breaks=50)

# Set indicator variables
train_data$default_unk <- 'no'
train_data$default_unk[train_data$default=='unknown'] <- 'yes'
train_data$default_unk <- as.factor(train_data$default_unk)
train_data$campaign_1 <- 0
train_data$campaign_1[train_data$campaign==1] <- 1
train_data$pdays_999 <- 0
train_data$pdays_999[train_data$pdays==999] <- 1
train_data$previous_0 <- 0
train_data$previous_0[train_data$previous==0] <- 1
train_data$poutcome_non <- 'no'
train_data$poutcome_non[train_data$poutcome=='nonexistent'] <- 'yes'
train_data$poutcome_non <- as.factor(train_data$poutcome_non)

# Logistic Regression
mod <- glm(y~age+job+marital+education+default*default_unk+housing+loan+campaign*campaign_1+pdays*pdays_999+
             previous*previous_0+poutcome*poutcome_non+emp.var.rate+cons.price.idx+cons.conf.idx+euribor3m+
             nr.employed,data=train_data,family="binomial")
summary(mod)
# Intercept, job = retired/student, marital = single, education = university.degree, default = unknown, campaign, previous = 0,
# cons.price.idx, cons.conf.idx, euribor3m, nr.employed

# Set indicator variables
train_data$default_unk <- 'yes'
train_data$default_unk[train_data$default=='unknown'] <- 'no'
train_data$default_unk <- as.factor(train_data$default_unk)
train_data$campaign_1 <- 1
train_data$campaign_1[train_data$campaign==1] <- 0
train_data$pdays_999 <- 1
train_data$pdays_999[train_data$pdays==999] <- 0
train_data$previous_0 <- 1
train_data$previous_0[train_data$previous==0] <- 0
train_data$poutcome_non <- 'yes'
train_data$poutcome_non[train_data$poutcome=='nonexistent'] <- 'no'
train_data$poutcome_non <- as.factor(train_data$poutcome_non)

# Logistic Regression
mod <- glm(y~age+job+marital+education+default*default_unk+housing+loan+campaign*campaign_1+pdays*pdays_999+
             previous*previous_0+poutcome*poutcome_non+emp.var.rate+cons.price.idx+cons.conf.idx+euribor3m+
             nr.employed,data=train_data,family="binomial")
summary(mod)
# Intercept, job = retired/student, marital = single, education = university.degree, default = unknown, campaign, previous = 0,
# cons.price.idx, cons.conf.idx, euribor3m, nr.employed

# Logistic Regression
mod <- glm(y~age+job+marital+education+default+housing+loan+campaign+pdays+
             previous+poutcome+emp.var.rate+cons.price.idx+cons.conf.idx+euribor3m+
             nr.employed,data=train_data,family="binomial")
summary(mod)
# Intercept, job = retired/student, marital = single, education = university.degree, default = unknown, campaign, pdays, 
# poutcome = nonexistent, cons.price.idx, cons.conf.idx, euribor3m, nr.employed

# TODO: Make some plots for job, marital, education, default, campaign, previous, pdays, poutcome,
# cons.price.idx, cons.conf.idx, euribor3m, nr.employed