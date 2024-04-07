# Packages
library(olsrr)

# Pull in data
data<-read.csv("C:/Users/aabro/OneDrive/Desktop/SMU Program/Classes/Stats 2/Final Project/bank-additional-full.csv",stringsAsFactors = T, sep=";")
data$y <- relevel(data$y, ref="yes")
train_perc <- .8
set.seed(1234)
train_index <- createDataPartition(data$y, p = train_perc, list = FALSE)
train_data <- data[train_index, ] # 32951
test_data <- data[-train_index, ] # 8237
train_data$duration <- c()

# Backwards Selection
fit_full <- lm(y ~ ., data = train_data) 
backward_ols_model <- ols_step_backward_p(fit_full,penter=0.05,details=TRUE)
# Hm, that didn't feel like working

# Forward Selection
fit_full <- lm(y ~ ., data = train_data) 
forwards_ols_model <- ols_step_forward_p(fit_full,penter=0.05,details=TRUE)

# ChatGPT says that there's a step function in R
final_model <- step(fit_full, direction = "forward")

################################################################################
# It my be kind of fun to try a combination of forward and backward selection

# First variable
set.seed(21)
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
# nr.employed = 0.7495164

# Adding to nr.employed
set.seed(22)
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
# month = 0.7805907

# Adding to nr.employed + month
set.seed(23)
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
# poutcome = 0.7874415

# Try removing vars
set.seed(24)
start_form_str <- 'y ~ nr.employed + month + poutcome'
vars <- c('nr.employed','month','poutcome')
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
# Removing any variable makes the model worse

# Adding to nr.employed + month + poutcome
set.seed(25)
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
# emp.var.rate = 0.7884868

# Try removing vars
set.seed(26)
start_form_str <- 'y ~ nr.employed + month + poutcome + emp.var.rate'
vars <- c('nr.employed','month','poutcome','emp.var.rate')
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
# Removing any variable makes the model worse

# Adding to nr.employed + month + poutcome + emp.var.rate
set.seed(27)
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
# euribor3m = 0.7896542

# Try removing vars
set.seed(28)
start_form_str <- 'y ~ nr.employed + month + poutcome + emp.var.rate + euribor3m'
vars <- c('nr.employed','month','poutcome','emp.var.rate','euribor3m')
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
# Removing any variable makes the model worse

# Adding to nr.employed + month + poutcome + emp.var.rate + euribor3m
set.seed(29)
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
# contact = 0.7909233

# Try removing vars
set.seed(30)
start_form_str <- 'y ~ nr.employed + month + poutcome + emp.var.rate + euribor3m + contact'
vars <- c('nr.employed','month','poutcome','emp.var.rate','euribor3m','contact')
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
# Removing any variable makes the model worse

# Adding to nr.employed + month + poutcome + emp.var.rate + euribor3m + contact
set.seed(31)
vars <- names(train_data)
vars <- vars[vars!="y"]
vars <- vars[vars!="nr.employed"]
vars <- vars[vars!="month"]
vars <- vars[vars!="poutcome"]
vars <- vars[vars!="emp.var.rate"]
vars <- vars[vars!="euribor3m"]
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
    form <- as.formula(paste("y ~ nr.employed + month + poutcome + emp.var.rate + euribor3m + contact + ",var,sep=""))
    model <- glm(form, data = train, family = "binomial")
    predictions <- predict(model, newdata = test, type = "response")
    roc <- roc(response=test$y,predictor=predictions,levels=c("no", "yes"),direction = ">")
    auc_scores[i] <- auc(roc)
  }
  var_aucs$auc[var_aucs$var == var] <- mean(auc_scores)
}
# cons.price.idx = 0.7924448

# Try removing vars
set.seed(32)
start_form_str <- 'y ~ nr.employed + month + poutcome + emp.var.rate + euribor3m + contact + cons.price.idx'
vars <- c('nr.employed','month','poutcome','emp.var.rate','euribor3m','contact','cons.price.idx')
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
# nr.employed = 0.7928806

# Adding to month + poutcome + emp.var.rate + euribor3m + contact + cons.price.idx
set.seed(33)
vars <- names(train_data)
vars <- vars[vars!="y"]
vars <- vars[vars!="month"]
vars <- vars[vars!="poutcome"]
vars <- vars[vars!="emp.var.rate"]
vars <- vars[vars!="euribor3m"]
vars <- vars[vars!="contact"]
vars <- vars[vars!="cons.price.idx"]
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
    form <- as.formula(paste("y ~ month + poutcome + emp.var.rate + euribor3m + contact + cons.price.idx + ",var,sep=""))
    model <- glm(form, data = train, family = "binomial")
    predictions <- predict(model, newdata = test, type = "response")
    roc <- roc(response=test$y,predictor=predictions,levels=c("no", "yes"),direction = ">")
    auc_scores[i] <- auc(roc)
  }
  var_aucs$auc[var_aucs$var == var] <- mean(auc_scores)
}
# default = 0.7930022

# Try removing vars
set.seed(34)
start_form_str <- 'y ~ month + poutcome + emp.var.rate + euribor3m + contact + cons.price.idx + default'
vars <- c('month','poutcome','emp.var.rate','euribor3m','contact','cons.price.idx','default')
num_vars <- length(vars)
var_aucs <- data.frame("vars" = vars)
num_folds <- 9
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
# It first errored when I had num_folds = 10, so I set it to 9
# Removing any variable makes the model worse

# Adding to month + poutcome + emp.var.rate + euribor3m + contact + cons.price.idx + default
set.seed(35)
vars <- names(train_data)
vars <- vars[vars!="y"]
vars <- vars[vars!="month"]
vars <- vars[vars!="poutcome"]
vars <- vars[vars!="emp.var.rate"]
vars <- vars[vars!="euribor3m"]
vars <- vars[vars!="contact"]
vars <- vars[vars!="cons.price.idx"]
vars <- vars[vars!="default"]
num_vars <- length(vars)
var_aucs <- data.frame("vars" = vars)
num_folds <- 7
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
    form <- as.formula(paste("y ~ month + poutcome + emp.var.rate + euribor3m + contact + cons.price.idx + default + ",var,sep=""))
    model <- glm(form, data = train, family = "binomial")
    predictions <- predict(model, newdata = test, type = "response")
    roc <- roc(response=test$y,predictor=predictions,levels=c("no", "yes"),direction = ">")
    auc_scores[i] <- auc(roc)
  }
  var_aucs$auc[var_aucs$var == var] <- mean(auc_scores)
}
# It first errored when I had num_folds = 10, so I set it to 7
# housing = 0.7932561

# Try removing vars
set.seed(36)
start_form_str <- 'y ~ month + poutcome + emp.var.rate + euribor3m + contact + cons.price.idx + default + housing'
vars <- c('month','poutcome','emp.var.rate','euribor3m','contact','cons.price.idx','default','housing')
num_vars <- length(vars)
var_aucs <- data.frame("vars" = vars)
num_folds <- 12
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
# Removing any variable makes the model worse

# Adding to month + poutcome + emp.var.rate + euribor3m + contact + cons.price.idx + default + housing
set.seed(37)
vars <- names(train_data)
vars <- vars[vars!="y"]
vars <- vars[vars!="month"]
vars <- vars[vars!="poutcome"]
vars <- vars[vars!="emp.var.rate"]
vars <- vars[vars!="euribor3m"]
vars <- vars[vars!="contact"]
vars <- vars[vars!="cons.price.idx"]
vars <- vars[vars!="default"]
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
    form <- as.formula(paste("y ~ month + poutcome + emp.var.rate + euribor3m + contact + cons.price.idx + default + housing + ",var,sep=""))
    model <- glm(form, data = train, family = "binomial")
    predictions <- predict(model, newdata = test, type = "response")
    roc <- roc(response=test$y,predictor=predictions,levels=c("no", "yes"),direction = ">")
    auc_scores[i] <- auc(roc)
  }
  var_aucs$auc[var_aucs$var == var] <- mean(auc_scores)
}
# Adding nothing made it better
# So the "best" is y ~ month + poutcome + emp.var.rate + euribor3m + contact + cons.price.idx + default + housing
model <- glm(y ~ month + poutcome + emp.var.rate + euribor3m + contact + cons.price.idx + default + housing, data = train, family = "binomial")
summary(model) # All really low p values, except for housing
vif(model) # All pretty low, except for emp.var.rate, euribor3m, and cons.price.idx

model <- glm(y ~ month + poutcome + emp.var.rate + contact + cons.price.idx + default, data = train, family = "binomial")
summary(model) # All really low p values, except for default = yes
vif(model) # All low

model <- glm(y ~ month + poutcome + emp.var.rate + contact + cons.price.idx, data = train, family = "binomial")
summary(model) # All really low p values
vif(model) # All low, so this could be considered a pretty "lean" model
