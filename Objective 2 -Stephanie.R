# Packages
library(caret) # CreateFolds
library(pROC) # roc
library(car) # VIF
library(tidyverse)

# Pull in data
data <- read_csv("C:/Users/Steph/OneDrive/Documents/MSDS_6372/Project 2/bank-additional-full.csv")

# Split into training and test
data$y <- factor(data$y, levels = c("no", "yes"))
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



#LDA Model

# Convert the binary outcome to a factor
train_data$y <- as.factor(train_data$y)


fitControl<-trainControl(method="repeatedcv",number=5,repeats=1,classProbs=TRUE, summaryFunction=mnLogLoss)
set.seed(1234)


lda.fit<-train(y~ month + poutcome + emp.var.rate + contact + cons.price.idx,
               data=train_data,
               method="lda",
               trControl=fitControl,
               metric="logLoss")

# Get threshold
metrics = data.frame(thresh=seq(0, 1, by = 0.0001))
num_thresh <- nrow(metrics)
metrics$sensitivity <- 1
metrics$specificity <- 1
metrics$ppv <- 1
metrics$npv <- 1
metrics$accuracy <- 1
metrics$f1 <- 1
predicted <- predict(lda.fit, newdata = train_data, type = "prob")
for (i in 1:num_thresh){
  if(i %% 100 == 0) {
    print(paste(i,'/',num_thresh,sep=''))
  }
  
  # Confusion Matrix
  predicted_classes <- ifelse(predicted[, "yes"] > metrics$thresh[i], 'yes', 'no')
  predicted_classes_factor <- factor(predicted_classes, levels = levels(train_data$y))
  confusion_matrix <- confusionMatrix(predicted_classes_factor, train_data$y)
  
  
  # Metrics
  metrics$sensitivity[i] <- as.numeric(confusion_matrix$byClass['Sensitivity'])
  metrics$specificity[i] <- as.numeric(confusion_matrix$byClass['Specificity'])
  metrics$ppv[i] <- as.numeric(confusion_matrix$byClass['Pos Pred Value'])
  metrics$npv[i] <- as.numeric(confusion_matrix$byClass['Neg Pred Value'])
  metrics$accuracy[i] <- as.numeric(confusion_matrix$overall['Accuracy'])
  metrics$f1[i] <- as.numeric(confusion_matrix$byClass['F1'])
}

# Get threshold value that maximizes F1
# Get F1 thresholds
maxF1 <- max(metrics$f1, na.rm = TRUE) # 
maxF1 
theshF1 <- metrics$thresh[which.max(metrics$f1)] 
theshF1 # 




# Test data
predicted <- predict(lda.fit, newdata = test_data, type = "prob")
predicted_classes <- ifelse(predicted[, "yes"] >  theshF1, 'yes', 'no')
confusion_matrix <- confusionMatrix(as.factor(predicted_classes), as.factor(test_data$y))
confusion_matrix 
confusion_matrix$byClass['F1'] # 0.47426842 


#AUC
plot(roc,print.thres="best",col="red")



library(pROC)

# Assuming `predicted` contains the predicted probabilities for the 'yes' class
roc <- roc(response = test_data$y, 
                 predictor = as.numeric(as.character(predicted[, "yes"])),
                 levels = rev(levels(test_data$y)))  # Ensure correct ordering of levels if needed

# Print the AUROC
auc(roc)

plot(roc,print.thres="best",col="red")

