###############################################################################
# We have a simple model y ~ month + poutcome + emp.var.rate + contact + cons.price.idx
# We now need to calculate metrics on it: sensitivity, specificity, prevalence, ppv, npv, and auc

# Packages
library(caret) # CreateFolds
library(pROC) # roc
library(car) # VIF

# Pull in data
data<-read.csv("C:/Users/aabro/OneDrive/Desktop/SMU Program/Classes/Stats 2/Final Project/Data/bank-additional-full.csv",stringsAsFactors = T, sep=";")
data$y <- relevel(data$y, ref="yes")
data$month <- factor(data$month, levels=c('mar','apr','may','jun','jul','aug','sep','oct','nov','dec'))
data$day_of_week <- factor(data$day_of_week, levels=c('mon','tue','wed','thu','fri'))
train_perc <- .8
set.seed(1234)
train_indices <- sample(nrow(data), floor(train_perc * nrow(data)))
train_data <- data[train_indices, ] 
test_data <- data[-train_indices, ] 
train_data$duration <- c()
train_data$default <- c()

# Let's calculate the AUC on the training data for a test
form <- as.formula(y ~ month + poutcome + emp.var.rate + contact + cons.price.idx)
model <- glm(form, data = train_data, family = "binomial")
predictions <- predict(model, newdata = train_data, type = "response")
roc <- roc(response=train_data$y,predictor=predictions,levels=c("no", "yes"),direction = ">")
plot(roc,print.thres="best",col="red") # I think the threshold is 0.887?

# So what happens if we use a threshold of 0.887?
predicted <- predict(model, newdata = train_data, type = "response")
predicted_classes <- ifelse(predicted > 0.887, 'no','yes')  # huh, I guess I had that flipped
confusion_matrix <- confusionMatrix(as.factor(predicted_classes), as.factor(train_data$y))
confusion_matrix # sensitivity = 0.622 and specifificity = 0.861 match up, these are not good though

# In order to calculate everything but auc, we'll need a threshold.  
# Let's calculate these values for thresholds in 0:.0001:1
# Also, we can calculate F1.
# Also, it makes sense that a yes would be a lot of money for the bank, where a no doesn't matter as much.
# So maybe assume some formula, like 1000*#yes - #no, and try maximizing that.
# While we're at it, why not do 100*yes and 10,000*yes, since they're kinda arbitrary to begin with.
metrics = data.frame(thresh=seq(0, 1, by = 0.0001))
num_thresh <- nrow(metrics)
metrics$sensitivity <- 0
metrics$specificity <- 0
metrics$ppv <- 0
metrics$npv <- 0
metrics$accuracy <- 0
metrics$f1 <- 0
metrics$sensPlusSpec <- 0
metrics$ppvPlusNpv <- 0
metrics$yes <- 0
metrics$tenYes <- 0
predicted <- predict(model, newdata = train_data, type = "response")
for (i in 1:num_thresh){
  if(i %% 100 == 0) {
    print(paste(i,'/',num_thresh,sep=''))
  }
  
  # Confusion Matrix
  predicted_classes <- ifelse(predicted > metrics$thresh[i], 'no','yes')
  confusion_matrix <- confusionMatrix(as.factor(predicted_classes), as.factor(train_data$y))
  
  # Metrics
  metrics$sensitivity[i] <- as.numeric(confusion_matrix$byClass['Sensitivity'])
  metrics$specificity[i] <- as.numeric(confusion_matrix$byClass['Specificity'])
  metrics$ppv[i] <- as.numeric(confusion_matrix$byClass['Pos Pred Value'])
  metrics$npv[i] <- as.numeric(confusion_matrix$byClass['Neg Pred Value'])
  metrics$accuracy[i] <- as.numeric(confusion_matrix$overall['Accuracy'])
  metrics$f1[i] <- as.numeric(confusion_matrix$byClass['F1'])
  metrics$sensPlusSpec[i] <- metrics$sensitivity[i] + metrics$specificity[i]
  metrics$ppvPlusNpv[i] <- metrics$ppv[i] + metrics$npv[i]
  metrics$yes[i] <- confusion_matrix$table['yes','yes'] - confusion_matrix$table['yes','no']
  metrics$tenYes[i] <- 10 * confusion_matrix$table['yes','yes'] - confusion_matrix$table['yes','no']
}

# Make some plots
plot(metrics$sensitivity) # no max
plot(metrics$specificity) # no max
plot(metrics$ppv) # no max
plot(metrics$npv) # no max
plot(metrics$accuracy)
maxAccuracy <- max(metrics$accuracy, na.rm = TRUE)
theshAccuracy <- which.max(metrics$accuracy[!is.na(metrics$accuracy)])
plot(metrics$f1) # 0.6198
maxF1 <- max(metrics$f1, na.rm = TRUE)
theshF1 <- which.max(metrics$f1[!is.na(metrics$f1)])
plot(metrics$sensPlusSpec) # 0.8849
maxSensPlusSpec <- max(metrics$sensPlusSpec, na.rm = TRUE)
theshSensPlusSpec <- which.max(metrics$sensPlusSpec[!is.na(metrics$sensPlusSpec)])
plot(metrics$ppvPlusNpv) # 0.0027
maxPpvPlusNpv <- max(metrics$ppvPlusNpv, na.rm = TRUE)
theshPpvPlusNpv <- which.max(metrics$ppvPlusNpv[!is.na(metrics$ppvPlusNpv)])
plot(metrics$yes) # 0.5197
maxYes <- max(metrics$yes, na.rm = TRUE)
theshYes <- which.max(metrics$yes[!is.na(metrics$yes)])
plot(metrics$tenYes) # 0.9222
maxTenYes <- max(metrics$tenYes, na.rm = TRUE)
theshTenYes <- which.max(metrics$tenYes[!is.na(metrics$tenYes)])

predicted_classes <- ifelse(predicted > 0.6198, 'no','yes')
confusion_matrix <- confusionMatrix(as.factor(predicted_classes), as.factor(train_data$y))

# We decided on using F1 as our metric, so let's use 0.6198 as the threshold
form <- as.formula(y ~ month + poutcome + emp.var.rate + contact + cons.price.idx)
model <- glm(form, data = train_data, family = "binomial")
predicted <- predict(model, newdata = test_data, type = "response")
predicted_classes <- ifelse(predicted > 0.6198, 'no','yes')
confusion_matrix <- confusionMatrix(as.factor(predicted_classes), as.factor(test_data$y))
confusion_matrix # Sensitivity = 0.32272, Specificity = 0.96451, PPV = 0.53069, NPV = 0.91970, Prevalence = 0.11059
confusion_matrix$byClass['F1'] # 0.4013652 

# AUC
roc <- roc(response=test_data$y,predictor=predicted,levels=c("no", "yes"),direction = ">")
auc(roc) # 0.7868
