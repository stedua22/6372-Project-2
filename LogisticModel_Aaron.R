# Packages
library(tidyverse)
library(mlbench)
library(pROC) # for AUC
library(caret)
library(GGally)
library(car)
library(glmnet)

# Pull in Data
data<-read.csv("C:/Users/aabro/OneDrive/Desktop/SMU Program/Classes/Stats 2/Final Project/bank-additional-full.csv",stringsAsFactors = T, sep=";")
data$y <- relevel(data$y, ref="yes")

# Maybe it would make more sense to just do a random split
train_perc <- .8
set.seed(1234)
train_index <- createDataPartition(data$y, p = train_perc, list = FALSE)
train_data <- data[train_index, ] # 32951
test_data <- data[-train_index, ] # 8237
train_data$duration <- c()
yes_data <- train_data[train_data$y == 'yes',] # 3712
no_data <- train_data[train_data$y == 'no',] # 29239
yes_data_test <- test_data[test_data$y == 'yes',] # 928
no_data_test <- test_data[test_data$y == 'no',] # 7309

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
mod <- glm(y~age+job+marital+education+default*default_unk+housing+loan+contact+month+day_of_week+campaign*campaign_1+pdays*pdays_999+
             previous*previous_0+poutcome*poutcome_non+emp.var.rate+cons.price.idx+cons.conf.idx+euribor3m+
             nr.employed,data=train_data,family="binomial")
summary(mod)
# Date split
# monthmay = 2.37e-13
# monthmar = 5.94e-06
# jobretired = 1.33e-05
# monthnov = 9.69e-05
# campaign = 0.000175
# defaultunknown = 0.000481
# day_of_weekwed = 0.000636
# contacttelephone = 0.002231
# jobstudent = 0.003401
# educationuniversity.degree = 0.006034
# day_of_weekthu = 0.010728
# campaign_1 = 0.030360
# emp.var.rate = 0.038265
# previous_0 = 0.045827

# Random split
# monthmar = < 2e-16
# contacttelephone = < 2e-16
# emp.var.rate = < 2e-16
# cons.price.idx = < 2e-16
# (Intercept) = 3.27e-09
# previous_0 = 8.09e-07
# monthjun = 2.18e-06
# monthmay = 3.99e-06
# monthaug = 5.91e-05
# campaign = 6.41e-05
# monthnov = 9.95e-05
# cons.conf.idx = 0.000220
# day_of_weekwed = 0.000560
# defaultunknown = 0.000601
# jobretired = 0.010065
# poutcomesuccess = 0.014187
# monthdec = 0.016944
# jobstudent = 0.031585
# day_of_weekmon = 0.031702

# Set indicator variables, the opposite way of above
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
mod <- glm(y~age+job+marital+education+default*default_unk+housing+loan+contact+month+day_of_week+campaign*campaign_1+pdays*pdays_999+
             previous*previous_0+poutcome*poutcome_non+emp.var.rate+cons.price.idx+cons.conf.idx+euribor3m+
             nr.employed,data=train_data,family="binomial")
summary(mod)
# These look the same as above

# Logistic Regression
mod <- glm(y~age+job+marital+education+default+housing+loan+campaign+pdays+month+day_of_week+contact+
             previous+poutcome+emp.var.rate+cons.price.idx+cons.conf.idx+euribor3m+
             nr.employed,data=train_data,family="binomial")
summary(mod)
# Split by date
# monthmay = 7.85e-14
# monthmar = 4.24e-06
# jobretired = 1.63e-05
# monthnov = 0.000116
# defaultunknown = 0.000473 
# day_of_weekwed = 0.000829 
# campaign = 0.001089 
# contacttelephone = 0.002323 
# jobstudent = 0.003455
# educationuniversity.degree = 0.005998 
# day_of_weekthu = 0.014505 
# pdays = 0.037692 
# poutcomenonexistent = 0.032094 
# emp.var.rate = 0.045670 
# educationbasic.6y = 0.047812 

# Split randomly
# monthmar = < 2e-16
# contacttelephone = < 2e-16
# emp.var.rate = < 2e-16
# cons.price.idx = < 2e-16
# (Intercept) = 2.97e-09
# pdays = 3.36e-08
# poutcomenonexistent = 3.94e-07
# monthjun = 1.98e-06
# monthmay = 3.90e-06
# campaign = 1.64e-05
# monthaug = 6.69e-05
# monthnov = 9.21e-05
# cons.conf.idx = 0.000205
# defaultunknown = 0.000600
# day_of_weekwed = 0.000653
# poutcomesuccess = 0.002377
# jobretired = 0.009832
# monthdec = 0.017769
# day_of_weekmon = 0.030939
# jobstudent = 0.031240
# Honestly, this isn't very different

# TODO: Make some plots for job, marital, education, default, campaign, previous, pdays, poutcome,
# cons.price.idx, cons.conf.idx, euribor3m, nr.employed

################################################################################
# Try seeing which variable has the best predictive score

# First, just see if we can get a CV error metric
set.seed(1)
num_folds <- 10
library(mlbench)
folds <- createFolds(train_data$y, k = num_folds)
accuracy_scores <- numeric(num_folds)
auc_scores <- numeric(num_folds)
for (i in 1:num_folds) {
  train_indices <- unlist(folds[-i])
  test_indices <- unlist(folds[i])
  train <- train_data[train_indices, ]
  test <- train_data[test_indices, ]
  model <- glm(y ~ ., data = train, family = "binomial")
  predictions <- predict(model, newdata = test, type = "response")
  predicted_classes <- as.factor(ifelse(predictions > 0.5, "no", "yes"))
  accuracy_scores[i] <- sum(predicted_classes == test$y) / length(test$y)
  conf_mat <- confusionMatrix(test$y, predicted_classes, positive="yes")
  roc <- roc(response=test$y,predictor=predictions,levels=c("no", "yes"),direction = ">")
  auc_scores[i] <- auc(roc)
}
mean(auc_scores) # 0.7890402

# What is the best threshold?  Because apparently .5 is not very good.
plot(roc,print.thres="best",col="red",add=T,legend=T) # I guess the threshold is 0.946?


# Try getting AUC scores for each of the variables
set.seed(2)
vars <- names(train_data)
vars <- vars[vars!="y"]
num_vars <- length(vars)
var_aucs <- data.frame("vars" = vars)
num_folds <- 10
for (j in 1:num_vars) {
  var <- vars[j]
  print(var)
  folds <- createFolds(train_data$y, k = num_folds)
  auc_scores <- numeric(num_folds)
  for (i in 1:num_folds) {
    train_indices <- unlist(folds[-i])
    test_indices <- unlist(folds[i])
    train <- train_data[train_indices, ]
    test <- train_data[test_indices, ]
    form <- as.formula(paste("y ~ ",var,sep=""))
    model <- glm(form, data = train, family = "binomial")
    predictions <- predict(model, newdata = test, type = "response")
    roc <- roc(response=test$y,predictor=predictions,levels=c("no", "yes"),direction = ">")
    auc_scores[i] <- auc(roc)
  }
  var_aucs$auc[var_aucs$var == var] <- mean(auc_scores)
}
# It looks like the highest performers are 
# nr.employed = 0.7473161
# euribor3m = 0.7425448
# emp.var.rate = 0.7146694
# month = 0.6582208

# emp.var.rate and month make sense, since they had low p-values when we looked at the full model
# However, nr.employed and euribor3m had lower values.  Maybe they're correlated?
library(GGally)
ggpairs(train_data[,c('nr.employed','euribor3m','emp.var.rate')],aes(color = train_data$y))
# As expected, correlation values are .9 and above.  So using any of these variables should be fine.
model <- glm(y ~ nr.employed + euribor3m + emp.var.rate, data = train_data, family = "binomial")
summary(model) # interesting, all p values are actually low
vif(model) # vif scores are huge though, 7, 11, and 22

# What happens if we add nr.employed, and then see what the other variables do?
# If you just use a full model, mean AUC is around .789, so there's room to improve
set.seed(3)
vars <- names(train_data)
vars <- vars[vars!="y"]
vars <- vars[vars!="nr.employed"]
num_vars <- length(vars)
var_aucs <- data.frame("vars" = vars)
num_folds <- 10
for (j in 1:num_vars) {
  var <- vars[j]
  print(var)
  folds <- createFolds(train_data$y, k = num_folds)
  auc_scores <- numeric(num_folds)
  for (i in 1:num_folds) {
    train_indices <- unlist(folds[-i])
    test_indices <- unlist(folds[i])
    train <- train_data[train_indices, ]
    test <- train_data[test_indices, ]
    form <- as.formula(paste("y ~ nr.employed + ",var,sep=""))
    model <- glm(form, data = train, family = "binomial")
    predictions <- predict(model, newdata = test, type = "response")
    roc <- roc(response=test$y,predictor=predictions,levels=c("no", "yes"),direction = ">")
    auc_scores[i] <- auc(roc)
  }
  var_aucs$auc[var_aucs$var == var] <- mean(auc_scores)
}
# This time month did the best
# month = 0.7777252
# contact = 0.7650325
# poutcome = 0.7541319
# job = 0.7535069
# euribor3m = 0.7526529
# education = 0.7521871
# pdays = 0.7520710
model <- glm(y ~ nr.employed + month, data = train_data, family = "binomial")
summary(model) # p values tend to be low
library(car)
vif(model) # both are low

# Now try nr.employed + month
set.seed(4)
vars <- names(train_data)
vars <- vars[vars!="y"]
vars <- vars[vars!="nr.employed"]
vars <- vars[vars!="month"]
num_vars <- length(vars)
var_aucs <- data.frame("vars" = vars)
num_folds <- 10
for (j in 1:num_vars) {
  var <- vars[j]
  print(var)
  folds <- createFolds(train_data$y, k = num_folds)
  auc_scores <- numeric(num_folds)
  for (i in 1:num_folds) {
    train_indices <- unlist(folds[-i])
    test_indices <- unlist(folds[i])
    train <- train_data[train_indices, ]
    test <- train_data[test_indices, ]
    form <- as.formula(paste("y ~ nr.employed + month + ",var,sep=""))
    model <- glm(form, data = train, family = "binomial")
    predictions <- predict(model, newdata = test, type = "response")
    roc <- roc(response=test$y,predictor=predictions,levels=c("no", "yes"),direction = ">")
    auc_scores[i] <- auc(roc)
  }
  var_aucs$auc[var_aucs$var == var] <- mean(auc_scores)
}
# Now poutcome is the best
# poutcome = 0.7843871
# pdays = 0.7829288
model <- glm(y ~ nr.employed + month + poutcome, data = train_data, family = "binomial")
summary(model) # p values tend to be low
vif(model) # vifs are low

# Now try nr.employed + month + poutcome
set.seed(5)
vars <- names(train_data)
vars <- vars[vars!="y"]
vars <- vars[vars!="nr.employed"]
vars <- vars[vars!="month"]
vars <- vars[vars!="poutcome"]
num_vars <- length(vars)
var_aucs <- data.frame("vars" = vars)
num_folds <- 10
for (j in 1:num_vars) {
  var <- vars[j]
  print(var)
  folds <- createFolds(train_data$y, k = num_folds)
  auc_scores <- numeric(num_folds)
  for (i in 1:num_folds) {
    train_indices <- unlist(folds[-i])
    test_indices <- unlist(folds[i])
    train <- train_data[train_indices, ]
    test <- train_data[test_indices, ]
    form <- as.formula(paste("y ~ nr.employed + month + poutcome + ",var,sep=""))
    model <- glm(form, data = train, family = "binomial")
    predictions <- predict(model, newdata = test, type = "response")
    roc <- roc(response=test$y,predictor=predictions,levels=c("no", "yes"),direction = ">")
    auc_scores[i] <- auc(roc)
  }
  var_aucs$auc[var_aucs$var == var] <- mean(auc_scores)
}
# Now emp.var.rate is the best, although the range is now 0.7853459 to 0.7816531
# emp.var.rate = 0.7853459
# marital = 0.7852511
# contact = 0.7848040
# campaign = 0.7844559
# euribor3m = 0.7843742
# I am a bit worried about nr.employed and emp.var.rate being correlated though
model <- glm(y ~ nr.employed + month + poutcome + emp.var.rate, data = train_data, family = "binomial")
summary(model) # p values tend to be low
vif(model) # vifs for nr.employed and emp.var.rate are higher at 4.9 and 4.1.  We could still keep both in the model though.

# Now try nr.employed + month + poutcome + emp.var.rate
set.seed(6)
vars <- names(train_data)
vars <- vars[vars!="y"]
vars <- vars[vars!="nr.employed"]
vars <- vars[vars!="month"]
vars <- vars[vars!="poutcome"]
vars <- vars[vars!="emp.var.rate"]
num_vars <- length(vars)
var_aucs <- data.frame("vars" = vars)
num_folds <- 10
for (j in 1:num_vars) {
  var <- vars[j]
  print(var)
  folds <- createFolds(train_data$y, k = num_folds)
  auc_scores <- numeric(num_folds)
  for (i in 1:num_folds) {
    train_indices <- unlist(folds[-i])
    test_indices <- unlist(folds[i])
    train <- train_data[train_indices, ]
    test <- train_data[test_indices, ]
    form <- as.formula(paste("y ~ nr.employed + month + poutcome + emp.var.rate + ",var,sep=""))
    model <- glm(form, data = train, family = "binomial")
    predictions <- predict(model, newdata = test, type = "response")
    roc <- roc(response=test$y,predictor=predictions,levels=c("no", "yes"),direction = ">")
    auc_scores[i] <- auc(roc)
  }
  var_aucs$auc[var_aucs$var == var] <- mean(auc_scores)
}
# pdays is best now, but euribor is coming up
# pdays = 0.7862047
# euribor3m = 0.7858530
model <- glm(y ~ nr.employed + month + poutcome + emp.var.rate + pdays, data = train_data, family = "binomial")
summary(model) # p values are low for both pdays and poutcome
vif(model) # wow, apparently pdays and poutcome are very correlated
# For the first like ~10% of the data, pdays increases up until 27 and has success or failure.  
# Then it jumps to 999, and poutcome is nonexistent. So it makes sense these would have a high vif value together.
# I wonder if it would make sense to make pdays and poutcome a product?  I could try that. Or just not include pdays.
model <- glm(y ~ nr.employed + month + poutcome + emp.var.rate + euribor3m, data = train_data, family = "binomial")
summary(model) # p values are low for both pdays and poutcome
vif(model) # vif for nr.employed, emp.var.rate, and euribor3m are all really large now

# Now try nr.employed + month + poutcome*pdays + emp.var.rate
set.seed(7)
vars <- names(train_data)
vars <- vars[vars!="y"]
vars <- vars[vars!="nr.employed"]
vars <- vars[vars!="month"]
vars <- vars[vars!="poutcome"]
vars <- vars[vars!="emp.var.rate"]
vars <- vars[vars!="pdays"]
num_vars <- length(vars)
var_aucs <- data.frame("vars" = vars)
num_folds <- 10
for (j in 1:num_vars) {
  var <- vars[j]
  print(var)
  folds <- createFolds(train_data$y, k = num_folds)
  auc_scores <- numeric(num_folds)
  for (i in 1:num_folds) {
    train_indices <- unlist(folds[-i])
    test_indices <- unlist(folds[i])
    train <- train_data[train_indices, ]
    test <- train_data[test_indices, ]
    form <- as.formula(paste("y ~ nr.employed + month + poutcome*pdays + emp.var.rate + ",var,sep=""))
    model <- glm(form, data = train, family = "binomial")
    predictions <- predict(model, newdata = test, type = "response")
    roc <- roc(response=test$y,predictor=predictions,levels=c("no", "yes"),direction = ">")
    auc_scores[i] <- auc(roc)
  }
  var_aucs$auc[var_aucs$var == var] <- mean(auc_scores)
}
model <- glm(y ~ nr.employed + month + poutcome*pdays + emp.var.rate + marital, data = train_data, family = "binomial")
summary(model) # marital categories have low p values
vif(model) # can't use vif function when there are products

# Now try nr.employed + month + poutcome + emp.var.rate + pdays
set.seed(8)
vars <- names(train_data)
vars <- vars[vars!="y"]
vars <- vars[vars!="nr.employed"]
vars <- vars[vars!="month"]
vars <- vars[vars!="poutcome"]
vars <- vars[vars!="emp.var.rate"]
vars <- vars[vars!="pdays"]
num_vars <- length(vars)
var_aucs <- data.frame("vars" = vars)
num_folds <- 10
for (j in 1:num_vars) {
  var <- vars[j]
  print(var)
  folds <- createFolds(train_data$y, k = num_folds)
  auc_scores <- numeric(num_folds)
  for (i in 1:num_folds) {
    train_indices <- unlist(folds[-i])
    test_indices <- unlist(folds[i])
    train <- train_data[train_indices, ]
    test <- train_data[test_indices, ]
    form <- as.formula(paste("y ~ nr.employed + month + poutcome + emp.var.rate + pdays + ",var,sep=""))
    model <- glm(form, data = train, family = "binomial")
    predictions <- predict(model, newdata = test, type = "response")
    roc <- roc(response=test$y,predictor=predictions,levels=c("no", "yes"),direction = ">")
    auc_scores[i] <- auc(roc)
  }
  var_aucs$auc[var_aucs$var == var] <- mean(auc_scores)
}
# marital is the best
# marital = 0.7868072
# default = 0.7862983
model <- glm(y ~ nr.employed + month + poutcome + emp.var.rate + pdays + marital, data = train_data, family = "binomial")
summary(model) # marital categories have low p values, poutcome and pdays are pretty good though
vif(model)

# Now try nr.employed + month + poutcome + emp.var.rate + euribor3m
set.seed(9)
vars <- names(train_data)
vars <- vars[vars!="y"]
vars <- vars[vars!="nr.employed"]
vars <- vars[vars!="month"]
vars <- vars[vars!="poutcome"]
vars <- vars[vars!="emp.var.rate"]
vars <- vars[vars!="euribor3m"]
num_vars <- length(vars)
var_aucs <- data.frame("vars" = vars)
num_folds <- 10
for (j in 1:num_vars) {
  var <- vars[j]
  print(var)
  folds <- createFolds(train_data$y, k = num_folds)
  auc_scores <- numeric(num_folds)
  for (i in 1:num_folds) {
    train_indices <- unlist(folds[-i])
    test_indices <- unlist(folds[i])
    train <- train_data[train_indices, ]
    test <- train_data[test_indices, ]
    form <- as.formula(paste("y ~ nr.employed + month + poutcome + emp.var.rate + euribor3m + ",var,sep=""))
    model <- glm(form, data = train, family = "binomial")
    predictions <- predict(model, newdata = test, type = "response")
    roc <- roc(response=test$y,predictor=predictions,levels=c("no", "yes"),direction = ">")
    auc_scores[i] <- auc(roc)
  }
  var_aucs$auc[var_aucs$var == var] <- mean(auc_scores)
}
# cons.conf.idx = 0.7863743
# marital = 0.7863397
# pdays = 0.7860666
model <- glm(y ~ nr.employed + month + poutcome + emp.var.rate + pdays + marital, data = train_data, family = "binomial")
summary(model) # marital categories have low p values, poutcome and pdays are pretty good though
vif(model)

# Now try nr.employed + month + poutcome + emp.var.rate + pdays + marital
set.seed(10)
vars <- names(train_data)
vars <- vars[vars!="y"]
vars <- vars[vars!="nr.employed"]
vars <- vars[vars!="month"]
vars <- vars[vars!="poutcome"]
vars <- vars[vars!="emp.var.rate"]
vars <- vars[vars!="pdays"]
vars <- vars[vars!="marital"]
num_vars <- length(vars)
var_aucs <- data.frame("vars" = vars)
num_folds <- 10
for (j in 1:num_vars) {
  var <- vars[j]
  print(var)
  folds <- createFolds(train_data$y, k = num_folds)
  auc_scores <- numeric(num_folds)
  for (i in 1:num_folds) {
    train_indices <- unlist(folds[-i])
    test_indices <- unlist(folds[i])
    train <- train_data[train_indices, ]
    test <- train_data[test_indices, ]
    form <- as.formula(paste("y ~ nr.employed + month + poutcome + emp.var.rate + pdays + marital + ",var,sep=""))
    model <- glm(form, data = train, family = "binomial")
    predictions <- predict(model, newdata = test, type = "response")
    roc <- roc(response=test$y,predictor=predictions,levels=c("no", "yes"),direction = ">")
    auc_scores[i] <- auc(roc)
  }
  var_aucs$auc[var_aucs$var == var] <- mean(auc_scores)
}
# contact = 0.7869090
model <- glm(y ~ nr.employed + month + poutcome + emp.var.rate + pdays + marital + contact, data = train_data, family = "binomial")
summary(model) # marital categories have low p values, poutcome and pdays are pretty good though
vif(model) # contact is low
# The AUC values are going up so slowly.  I feel like this approach isn't amazing for getting up to the .789 that the full model had.

# Let's try one more time, with all variables but job, since job seems to not be good.
set.seed(11)
vars <- c("job")
num_vars <- length(vars)
var_aucs <- data.frame("vars" = vars)
num_folds <- 10
for (j in 1:num_vars) {
  var <- vars[j]
  print(var)
  folds <- createFolds(train_data$y, k = num_folds)
  auc_scores <- numeric(num_folds)
  for (i in 1:num_folds) {
    train_indices <- unlist(folds[-i])
    test_indices <- unlist(folds[i])
    train <- train_data[train_indices, ]
    test <- train_data[test_indices, ]
    form <- as.formula(paste("y ~ . - job + ",var,sep=""))
    model <- glm(form, data = train, family = "binomial")
    predictions <- predict(model, newdata = test, type = "response")
    roc <- roc(response=test$y,predictor=predictions,levels=c("no", "yes"),direction = ">")
    auc_scores[i] <- auc(roc)
  }
  var_aucs$auc[var_aucs$var == var] <- mean(auc_scores)
}
# Yup, still .789.  Try doing the backwards approach.

################################################################################
set.seed(12)
vars <- names(train_data)
vars <- vars[vars!="y"]
num_vars <- length(vars)
var_aucs <- data.frame("vars" = vars)
num_folds <- 10
for (j in 1:num_vars) {
  var <- vars[j]
  print(var)
  folds <- createFolds(train_data$y, k = num_folds)
  auc_scores <- numeric(num_folds)
  for (i in 1:num_folds) {
    train_indices <- unlist(folds[-i])
    test_indices <- unlist(folds[i])
    train <- train_data[train_indices, ]
    test <- train_data[test_indices, ]
    form <- as.formula(paste("y ~ . - ",var,sep=""))
    model <- glm(form, data = train, family = "binomial")
    predictions <- predict(model, newdata = test, type = "response")
    roc <- roc(response=test$y,predictor=predictions,levels=c("no", "yes"),direction = ">")
    auc_scores[i] <- auc(roc)
  }
  var_aucs$auc[var_aucs$var == var] <- mean(auc_scores)
}
# Removing age seems to help the most
# age = 0.7899498
# default = 0.7896749
# job = 0.7895502
# education = 0.7895135
# cons.conf.idx = 0.7894343
# euribor3m = 0.7893457
# nr.employed = 0.7892794
model <- glm(y ~ . - age, data = train_data, family = "binomial")
summary(model)
vif(model) # error

# Remove age
set.seed(13)
vars <- names(train_data)
vars <- vars[vars!="y"]
vars <- vars[vars!="age"]
num_vars <- length(vars)
var_aucs <- data.frame("vars" = vars)
num_folds <- 10
for (j in 1:num_vars) {
  var <- vars[j]
  print(var)
  folds <- createFolds(train_data$y, k = num_folds)
  auc_scores <- numeric(num_folds)
  for (i in 1:num_folds) {
    train_indices <- unlist(folds[-i])
    test_indices <- unlist(folds[i])
    train <- train_data[train_indices, ]
    test <- train_data[test_indices, ]
    form <- as.formula(paste("y ~ . - age - ",var,sep=""))
    model <- glm(form, data = train, family = "binomial")
    predictions <- predict(model, newdata = test, type = "response")
    roc <- roc(response=test$y,predictor=predictions,levels=c("no", "yes"),direction = ">")
    auc_scores[i] <- auc(roc)
  }
  var_aucs$auc[var_aucs$var == var] <- mean(auc_scores)
}
# It seems like removing age is really the only benefit.  Now I'm curious how it gets all the way up to 0.789

# Now try nr.employed + month + poutcome + emp.var.rate + pdays + marital + contact
set.seed(14)
vars <- names(train_data)
vars <- vars[vars!="y"]
vars <- vars[vars!="nr.employed"]
vars <- vars[vars!="month"]
vars <- vars[vars!="poutcome"]
vars <- vars[vars!="emp.var.rate"]
vars <- vars[vars!="pdays"]
vars <- vars[vars!="marital"]
vars <- vars[vars!="contact"]
num_vars <- length(vars)
var_aucs <- data.frame("vars" = vars)
num_folds <- 10
for (j in 1:num_vars) {
  var <- vars[j]
  print(var)
  folds <- createFolds(train_data$y, k = num_folds)
  auc_scores <- numeric(num_folds)
  for (i in 1:num_folds) {
    train_indices <- unlist(folds[-i])
    test_indices <- unlist(folds[i])
    train <- train_data[train_indices, ]
    test <- train_data[test_indices, ]
    form <- as.formula(paste("y ~ nr.employed + month + poutcome + emp.var.rate + pdays + marital + contact + ",var,sep=""))
    model <- glm(form, data = train, family = "binomial")
    predictions <- predict(model, newdata = test, type = "response")
    roc <- roc(response=test$y,predictor=predictions,levels=c("no", "yes"),direction = ">")
    auc_scores[i] <- auc(roc)
  }
  var_aucs$auc[var_aucs$var == var] <- mean(auc_scores)
}
# cons.price.idx = 0.7888909
# There we go.  So adding contact isn't huge, but it makes the next variables huge.
model <- glm(y ~ nr.employed + month + poutcome + emp.var.rate + pdays + marital + contact + cons.price.idx, data = train_data, family = "binomial")
summary(model) 
vif(model) # interesting, month increased
model <- glm(y ~ nr.employed + month + cons.price.idx + emp.var.rate, data = train_data, family = "binomial")
vif(model)

# Now try nr.employed + month + poutcome + emp.var.rate + pdays + marital + contact + cons.price.idx
set.seed(15)
vars <- names(train_data)
vars <- vars[vars!="y"]
vars <- vars[vars!="nr.employed"]
vars <- vars[vars!="month"]
vars <- vars[vars!="poutcome"]
vars <- vars[vars!="emp.var.rate"]
vars <- vars[vars!="pdays"]
vars <- vars[vars!="marital"]
vars <- vars[vars!="contact"]
num_vars <- length(vars)
var_aucs <- data.frame("vars" = vars)
num_folds <- 10
for (j in 1:num_vars) {
  var <- vars[j]
  print(var)
  folds <- createFolds(train_data$y, k = num_folds)
  auc_scores <- numeric(num_folds)
  for (i in 1:num_folds) {
    train_indices <- unlist(folds[-i])
    test_indices <- unlist(folds[i])
    train <- train_data[train_indices, ]
    test <- train_data[test_indices, ]
    form <- as.formula(paste("y ~ nr.employed + month + poutcome + emp.var.rate + pdays + marital + contact + cons.price.idx + ",var,sep=""))
    model <- glm(form, data = train, family = "binomial")
    predictions <- predict(model, newdata = test, type = "response")
    roc <- roc(response=test$y,predictor=predictions,levels=c("no", "yes"),direction = ">")
    auc_scores[i] <- auc(roc)
  }
  var_aucs$auc[var_aucs$var == var] <- mean(auc_scores)
}
# euribor3m = 0.7905232, even better than the 0.789 from removing age
model <- glm(y ~ nr.employed + month + poutcome + emp.var.rate + pdays + marital + contact + cons.price.idx + euribor3m, data = train_data, family = "binomial")
summary(model) # marital categories still aren't good, p values aren't bad though, even for highly correlated variables
vif(model) # adding cons.price.idx and euribor3m shot up the VIF scores though

# Now try nr.employed + month + poutcome + emp.var.rate + pdays + marital + contact + cons.price.idx + euribor3m
set.seed(16)
vars <- names(train_data)
vars <- vars[vars!="y"]
vars <- vars[vars!="nr.employed"]
vars <- vars[vars!="month"]
vars <- vars[vars!="poutcome"]
vars <- vars[vars!="emp.var.rate"]
vars <- vars[vars!="pdays"]
vars <- vars[vars!="marital"]
vars <- vars[vars!="contact"]
vars <- vars[vars!="cons.price.idx"]
vars <- vars[vars!="euribor3m"]
num_vars <- length(vars)
var_aucs <- data.frame("vars" = vars)
num_folds <- 10
for (j in 1:num_vars) {
  var <- vars[j]
  print(var)
  folds <- createFolds(train_data$y, k = num_folds)
  auc_scores <- numeric(num_folds)
  for (i in 1:num_folds) {
    train_indices <- unlist(folds[-i])
    test_indices <- unlist(folds[i])
    train <- train_data[train_indices, ]
    test <- train_data[test_indices, ]
    form <- as.formula(paste("y ~ nr.employed + month + poutcome + emp.var.rate + pdays + marital + contact + cons.price.idx + euribor3m + ",var,sep=""))
    model <- glm(form, data = train, family = "binomial")
    predictions <- predict(model, newdata = test, type = "response")
    roc <- roc(response=test$y,predictor=predictions,levels=c("no", "yes"),direction = ">")
    auc_scores[i] <- auc(roc)
  }
  var_aucs$auc[var_aucs$var == var] <- mean(auc_scores)
}
# campaign = 0.7908145
# previous = 0.7904490

# Now try nr.employed + month + poutcome + emp.var.rate + pdays + marital + contact + cons.price.idx + euribor3m + campaign
set.seed(17)
vars <- names(train_data)
vars <- vars[vars!="y"]
vars <- vars[vars!="nr.employed"]
vars <- vars[vars!="month"]
vars <- vars[vars!="poutcome"]
vars <- vars[vars!="emp.var.rate"]
vars <- vars[vars!="pdays"]
vars <- vars[vars!="marital"]
vars <- vars[vars!="contact"]
vars <- vars[vars!="cons.price.idx"]
vars <- vars[vars!="euribor3m"]
vars <- vars[vars!="campaign"]
num_vars <- length(vars)
var_aucs <- data.frame("vars" = vars)
num_folds <- 10
for (j in 1:num_vars) {
  var <- vars[j]
  print(var)
  folds <- createFolds(train_data$y, k = num_folds)
  auc_scores <- numeric(num_folds)
  for (i in 1:num_folds) {
    train_indices <- unlist(folds[-i])
    test_indices <- unlist(folds[i])
    train <- train_data[train_indices, ]
    test <- train_data[test_indices, ]
    form <- as.formula(paste("y ~ nr.employed + month + poutcome + emp.var.rate + pdays + marital + contact + cons.price.idx + euribor3m + campaign + ",var,sep=""))
    model <- glm(form, data = train, family = "binomial")
    predictions <- predict(model, newdata = test, type = "response")
    roc <- roc(response=test$y,predictor=predictions,levels=c("no", "yes"),direction = ">")
    auc_scores[i] <- auc(roc)
  }
  var_aucs$auc[var_aucs$var == var] <- mean(auc_scores)
}
# housing = 0.7909908
# age = 0.7907559

# Now try nr.employed + month + poutcome + emp.var.rate + pdays + marital + contact + cons.price.idx + euribor3m + campaign + housing
set.seed(18)
vars <- names(train_data)
vars <- vars[vars!="y"]
vars <- vars[vars!="nr.employed"]
vars <- vars[vars!="month"]
vars <- vars[vars!="poutcome"]
vars <- vars[vars!="emp.var.rate"]
vars <- vars[vars!="pdays"]
vars <- vars[vars!="marital"]
vars <- vars[vars!="contact"]
vars <- vars[vars!="cons.price.idx"]
vars <- vars[vars!="euribor3m"]
vars <- vars[vars!="campaign"]
vars <- vars[vars!="housing"]
num_vars <- length(vars)
var_aucs <- data.frame("vars" = vars)
num_folds <- 10
for (j in 1:num_vars) {
  var <- vars[j]
  print(var)
  folds <- createFolds(train_data$y, k = num_folds)
  auc_scores <- numeric(num_folds)
  for (i in 1:num_folds) {
    train_indices <- unlist(folds[-i])
    test_indices <- unlist(folds[i])
    train <- train_data[train_indices, ]
    test <- train_data[test_indices, ]
    form <- as.formula(paste("y ~ nr.employed + month + poutcome + emp.var.rate + pdays + marital + contact + cons.price.idx + euribor3m + campaign + housing + ",var,sep=""))
    model <- glm(form, data = train, family = "binomial")
    predictions <- predict(model, newdata = test, type = "response")
    roc <- roc(response=test$y,predictor=predictions,levels=c("no", "yes"),direction = ">")
    auc_scores[i] <- auc(roc)
  }
  var_aucs$auc[var_aucs$var == var] <- mean(auc_scores)
}
model <- glm(y ~ nr.employed + month + poutcome + emp.var.rate + pdays + marital + contact + cons.price.idx + euribor3m + campaign + housing, data = train_data, family = "binomial")
summary(model) # marital categories still aren't good, p values aren't bad though, even for highly correlated variables
vif(model)
# (nr.employed, emp.var.rate, cons.price.idx, euribor3m)
# (nr.employed, month, emp.var.rate, cons.price.idx)
# (poutcome, pdays)
# marital
# contact
# campaign
# housing
# Hm, what would happen if we tried taking away variables from this?

# Starting with y ~ nr.employed + month + poutcome + emp.var.rate + pdays + marital + contact + cons.price.idx + euribor3m + campaign + housing
set.seed(19)
start_form_str <- 'y ~ nr.employed + month + poutcome + emp.var.rate + pdays + marital + contact + cons.price.idx + euribor3m + campaign + housing'
vars <- c('nr.employed','month','poutcome','emp.var.rate','pdays','marital','contact','cons.price.idx','euribor3m','campaign','housing')
num_vars <- length(vars)
var_aucs <- data.frame("vars" = vars)
num_folds <- 10
for (j in 1:num_vars) {
  var <- vars[j]
  print(var)
  folds <- createFolds(train_data$y, k = num_folds)
  auc_scores <- numeric(num_folds)
  for (i in 1:num_folds) {
    train_indices <- unlist(folds[-i])
    test_indices <- unlist(folds[i])
    train <- train_data[train_indices, ]
    test <- train_data[test_indices, ]
    form <- as.formula(paste(start_form_str," -",var,sep=""))
    model <- glm(form, data = train, family = "binomial")
    predictions <- predict(model, newdata = test, type = "response")
    roc <- roc(response=test$y,predictor=predictions,levels=c("no", "yes"),direction = ">")
    auc_scores[i] <- auc(roc)
  }
  var_aucs$auc[var_aucs$var == var] <- mean(auc_scores)
}
# It looks like removing any of the variables made it worse, so this was a good formula

################################################################################
# Some code to run any model
set.seed(20)
# form <- as.formula('y ~ nr.employed + month + poutcome + housing + cons.price.idx') # 0.7890402
# form <- as.formula('y ~ .') # 0.790865
# form <- as.formula('y ~ month + poutcome + emp.var.rate + contact + cons.price.idx + default') # 0.7913738
form <- as.formula('y ~ month + poutcome + emp.var.rate + contact + cons.price.idx')
num_folds <- 10
folds <- createFolds(train_data$y, k = num_folds)
accuracy_scores <- numeric(num_folds)
auc_scores <- numeric(num_folds)
for (i in 1:num_folds) {
  train_indices <- unlist(folds[-i])
  test_indices <- unlist(folds[i])
  train <- train_data[train_indices, ]
  test <- train_data[test_indices, ]
  model <- glm(form, data = train, family = "binomial")
  predictions <- predict(model, newdata = test, type = "response")
  roc <- roc(response=test$y,predictor=predictions,levels=c("no", "yes"),direction = ">")
  auc_scores[i] <- auc(roc)
}
mean(auc_scores) 

################################################################################
# Stephanie wants me to run her glmnet code
# Prepare the data for glmnet
x <- model.matrix(y ~ . - 1, data = train_data) # The '-1' removes the intercept
y <- train_data$y

# Fit the glmnet model
library(glmnet)
cv.glmnet.model <- cv.glmnet(x, y, family = "binomial", alpha = 1)

# Look at the results to determine the best lambda
plot(cv.glmnet.model)

# Look at the coefficients for the best lambda value
coef(cv.glmnet.model, s = "lambda.min")
