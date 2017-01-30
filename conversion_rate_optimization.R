rm(list=ls())
cat('\014')

library(dplyr)
library(ggplot2)
library(GGally)
library(ROCR)

setwd("~/berkeley/spring2017/DataScience/Assignment1")

raw <- read.csv('conversion_data.csv')
str(raw)
summary(raw)

#correlation matrix
# ggscatmat(raw)

# checking the split of the dependent variable before splitting to test and train sets
table(raw$converted)
# 0      1 
# 306000  10200

# splitting into training and test sets
raw$id <- 1:nrow(raw)
train <- sample_frac(raw, size=0.7)
test <- anti_join(raw, train, by="id")

# checking split of dependent variable in the test and train test
table(test$converted)
# 0     1 
# 91765  3095  
table(train$converted)
# 0      1 
# 214194   7146 

train$id <- NULL
test$id <- NULL


# logistic regression models
model <- glm(converted ~ country + age + new_user + source + total_pages_visited, 
             data=train, family=binomial)

summary(model)
# log(p/1-p) = b0 + b1x1 + b2x2 + ..

#predictions on the training data
predict_train <- predict(model, type="response")
#confusion matrix
table(train$converted, predict_train > 0.5)
# FALSE   TRUE
# 0 213337    857
# 1   2234   4912

# accuracy
(213337 + 4912)/221340
# 0.9860351

#roc - receiver operating characteristic
# fpr = fp/fp + tn
# tpr = tp/fn + tp

ROCpred <- prediction(predict_train, train$converted)
ROCcurve <- performance(ROCpred, "tpr", "fpr")
plot(ROCcurve, colorize=TRUE, print.cutoffs.at=seq(0,1,0.1), text.adj=c(-0.2,0.7))

#auc  - Area Under the curve 
as.numeric(performance(ROCpred, "auc")@y.values)
# 0.9859186

#predictions on the test set
predict_test <- predict(model, type="response", newdata=test)
table(test$converted, predict_test > 0.5)
# FALSE  TRUE
# 0 91449   357
# 1   924  2130

#accuracy 
(91449 + 2130)/94860
# 0.9864959

