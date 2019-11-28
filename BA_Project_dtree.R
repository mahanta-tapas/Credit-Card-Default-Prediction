library(rpart)
library(rpart.plot)
library(caret)
library(dplyr)

credit.df <- read.csv("CreditCardDefault_dataset.csv")
credit.df <- credit.df[,-1]  #Drop ID

#Unii-Variate Analysis
credit.df %>% group_by(SEX) %>% dplyr::summarise(count=n())
credit.df %>% group_by(MARRIAGE) %>% summarise(count=n()) %>% mutate(pct=count/sum(count))%>%ggplot(aes(x = MARRIAGE, y = pct)) + geom_bar(stat = "identity", fill ="purple", aes(color = I('black')), size = 0.1)+xlab("MARRIAGE") + 
  ylab("Percent")+ theme_few()
credit.df %>% group_by(SEX) %>% summarise(count=n()) %>% mutate(pct=count/sum(count))%>%ggplot(aes(x = SEX, y = pct)) + geom_bar(stat = "identity", fill ="purple", aes(color = I('black')), size = 0.1)+xlab("SEX") + 
  ylab("Percent")+ theme_few()
credit.df %>% group_by(PAY_2) %>% summarise(count=n()) %>% mutate(pct=count/sum(count))%>%ggplot(aes(x = PAY_2, y = pct)) + geom_bar(stat = "identity", fill ="purple", aes(color = I('black')), size = 0.1)+xlab("PAY_2") + 
  ylab("Percent")+ theme_few()

train.index <- sample(c(1:dim(credit.df)[1]),0.7 * dim(credit.df)[1])
train.df <- credit.df[train.index, ]
valid.df <- credit.df[-train.index, ]
#credit.ct <- rpart(default.payment.next.month ~ ., data = train.df,method = "class")
varImp(credit.ct)
prp(credit.ct, type = 1, extra = 2, under = TRUE, split.font = 1, varlen = -10)
credit.ct$cptable
plotcp(credit.ct)

credit.ct <- rpart(default.payment.next.month ~ ., data = train.df,method = "class",parms=list(split='gini'),cp=0.01)
length(credit.ct$frame$var[credit.ct$frame$var == "<leaf>"])
credit.ct.pred.train <- predict(credit.ct,train.df,type = "class")
confusionMatrix(credit.ct.pred.train, as.factor(train.df$default.payment.next.month))

credit.ct.pred.valid <- predict(credit.ct,valid.df,type = "class")
confusionMatrix(credit.ct.pred.valid, as.factor(valid.df$default.payment.next.month))


credit.ct <- rpart(default.payment.next.month ~ ., data = train.df,method = "class",parms=list(split='gini'),cp=0.001)
length(credit.ct$frame$var[credit.ct$frame$var == "<leaf>"])
credit.ct.pred.train <- predict(credit.ct,train.df,type = "class")
confusionMatrix(credit.ct.pred.train, as.factor(train.df$default.payment.next.month))

credit.ct.pred.valid <- predict(credit.ct,valid.df,type = "class")
confusionMatrix(credit.ct.pred.valid,as.factor(valid.df$default.payment.next.month))


credit.ct <- rpart(default.payment.next.month ~ ., data = train.df,method = "class",parms=list(split='information'),cp=0.001)
length(credit.ct$frame$var[credit.ct$frame$var == "<leaf>"])
credit.ct.pred.train <- predict(credit.ct,train.df,type = "class")
confusionMatrix(credit.ct.pred.train, as.factor(train.df$default.payment.next.month))

credit.ct.pred.valid <- predict(credit.ct,valid.df,type = "class")
confusionMatrix(credit.ct.pred.valid,as.factor(valid.df$default.payment.next.month))


##############################################################################################
##Logistic Regression
logit.reg <- glm(default.payment.next.month ~ ., data = train.df, family = 'binomial')

logit.pred.train <- predict(logit.reg, train.df[, !(colnames(train.df) %in% c('default.payment.next.month'))], type = 'response')
confusionMatrix(as.factor(ifelse(logit.pred.train > 0.5, 1, 0)), as.factor(train.df$default.payment.next.month))
#Accuracy : 0.8121
logit.pred.valid <- predict(logit.reg, valid.df[, !(colnames(valid.df) %in% c('default.payment.next.month'))], type = 'response')
confusionMatrix(as.factor(ifelse(logit.pred.valid > 0.5, 1, 0)), as.factor(valid.df$default.payment.next.month))
#Accuracy : 0.8112


#backward elimination
full.logit.reg <- glm(default.payment.next.month ~ ., data = train.df, family = "binomial") 
empty.logit.reg  <- glm(default.payment.next.month ~ 1,data = train.df, family= "binomial")
summary(empty.logit.reg)


backwards = step(full.logit.reg)
summary(backwards)

logit.reg.back <- glm(formula(backwards), data = train.df, family = 'binomial')

logit.pred.train.back <- predict(logit.reg.back, train.df[, !(colnames(train.df) %in% c('default.payment.next.month'))], type = 'response')
confusionMatrix(as.factor(ifelse(logit.pred.train > 0.5, 1, 0)), as.factor(train.df$default.payment.next.month))
##Accuracy : 0.8121
logit.pred.valid.back <- predict(logit.reg.back, valid.df[, !(colnames(valid.df) %in% c('default.payment.next.month'))], type = 'response')
confusionMatrix(as.factor(ifelse(logit.pred.valid > 0.5, 1, 0)), as.factor(valid.df$default.payment.next.month))
## Accuracy : 0.8112
library('ggplot2')
library(pROC)

predict.df = predict(logit.reg.back, newdata = valid.df, type = "response")
#confusionMatrix(predict.df, valid.df$default.payment.next.month)
plot.roc(valid.df$default.payment.next.month,predict.df)

