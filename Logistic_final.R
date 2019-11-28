library(rpart)
library(rpart.plot)
library(caret)
library(dplyr)

credit.df <- read.csv("/Users/tapas/Downloads/UCI_Credit_Card.csv")
credit.df <- credit.df[,-1]  #Drop ID

train.index <- sample(c(1:dim(credit.df)[1]),0.7 * dim(credit.df)[1])
train.df <- credit.df[train.index, ]
valid.df <- credit.df[-train.index, ]

##############Logistic Regression#################
logit.reg <- glm(default.payment.next.month ~ ., data = train.df, family = 'binomial')

logit.pred.train <- predict(logit.reg, train.df[, !(colnames(train.df) %in% c('default.payment.next.month'))], type = 'response')
confusionMatrix(as.factor(ifelse(logit.pred.train > 0.5, 1, 0)), as.factor(train.df$default.payment.next.month))
#Accuracy : 0.8104
logit.pred.valid <- predict(logit.reg, valid.df[, !(colnames(valid.df) %in% c('default.payment.next.month'))], type = 'response')
confusionMatrix(as.factor(ifelse(logit.pred.valid > 0.5, 1, 0)), as.factor(valid.df$default.payment.next.month))
roc.curve(valid.df$default.payment.next.month,logit.pred.valid)
plot(roc(valid.df$default.payment.next.month, logit.pred.valid), main= "Logistic Regression")
roc_val <- roc(valid.df$default.payment.next.month, logit.pred.valid)
roc_val$auc
#Accuracy : 0.8104


#backward elimination
full.logit.reg <- glm(default.payment.next.month ~ ., data = train.df, family = "binomial") 
empty.logit.reg  <- glm(default.payment.next.month ~ 1,data = train.df, family= "binomial")
summary(empty.logit.reg)


backwards = step(full.logit.reg)
summary(backwards)

logit.reg.back <- glm(formula(backwards), data = train.df, family = 'binomial')

logit.pred.train.back <- predict(logit.reg.back, train.df[, !(colnames(train.df) %in% c('default.payment.next.month'))], type = 'response')
confusionMatrix(as.factor(ifelse(logit.pred.train > 0.5, 1, 0)), as.factor(train.df$default.payment.next.month))
##Accuracy : 0.8104
logit.pred.valid.back <- predict(logit.reg.back, valid.df[, !(colnames(valid.df) %in% c('default.payment.next.month'))], type = 'response')
confusionMatrix(as.factor(ifelse(logit.pred.valid > 0.5, 1, 0)), as.factor(valid.df$default.payment.next.month))
## Accuracy : 0.8104

#ROC curve
library(pROC)
library(ggplot2)
library(plotROC)
plot(roc(valid.df$default.payment.next.month, logit.pred.valid.back), main= "Logistic Regression")
roc_val <- roc(valid.df$default.payment.next.month, logit.pred.valid.back)
roc_val$auc
