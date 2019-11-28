
library('ggplot2')
library('ggthemes')
library('dplyr')

credit.df <- read.csv("/Users/tapas/Downloads/UCI_Credit_Card.csv")

credit.df %>% group_by(SEX) %>% dplyr::summarise(count=n())
# SEX    count
# <fct>  <int>
#   1 Male   11888
# 2 Female 18112
credit.df %>% group_by(SEX) %>% dplyr::summarise(count=n()) %>% mutate(pct=count/sum(count))%>%ggplot(aes(x = SEX, y = pct)) + geom_bar(stat = "identity", fill='#FF5555',aes(color = I('black')), size = 0.1)+xlab("Sex") +
  ylab("Percent")+ theme_few()

credit.df %>% group_by(MARRIAGE) %>% dplyr::summarise(count=n())
# MARRIAGE count
# <fct>    <int>
#   1 Other      377
# 2 Married  13659
# 3 Single   15964

credit.df %>% group_by(MARRIAGE) %>% dplyr::summarise(count=n()) %>% mutate(pct=count/sum(count))%>%ggplot(aes(x = MARRIAGE, y = pct)) + geom_bar(stat = "identity", fill ="#FF5555", aes(color = I('black')), size = 0.1)+xlab("Marriage") +
  ylab("Percent")+ theme_few()



summary(credit.df$EDUCATION)
unique(credit.df$EDUCATION)
#Factoring Categorical variables

# Combining Unkown and Other categories as Other.
credit.df$EDUCATION <- as.factor(credit.df$EDUCATION)
levels(credit.df$EDUCATION) <- c("Other","Graduate","University","High School","Other","Other","Other")

# Combining Unkown and Other categories as Other.
credit.df$SEX <- as.factor(credit.df$SEX)
levels(credit.df$SEX) <- c("Male","Female")

credit.df$MARRIAGE <- as.factor(credit.df$MARRIAGE)
levels(credit.df$MARRIAGE) <- c("Other","Married","Single","Other")

credit.df$PAY_0 <- as.factor(credit.df$PAY_0)
credit.df$PAY_2 <- as.factor(credit.df$PAY_2)
credit.df$PAY_3 <- as.factor(credit.df$PAY_3)
credit.df$PAY_4 <- as.factor(credit.df$PAY_4)
credit.df$PAY_5 <- as.factor(credit.df$PAY_5)
credit.df$PAY_6 <- as.factor(credit.df$PAY_6)
credit.df
credit.df %>% group_by(PAY_0) %>% dplyr::summarise(count=n()) %>% mutate(pct=count/sum(count))%>%ggplot(aes(x = PAY_0, y = pct)) + geom_bar(stat = "identity", fill ="#FF6666", aes(color = I('black')), size = 0.1)+xlab("PAY_0") +
  ylab("Percent")+ theme_few()
credit.df %>% group_by(PAY_2) %>% dplyr::summarise(count=n()) %>% mutate(pct=count/sum(count))%>%ggplot(aes(x = PAY_2, y = pct)) + geom_bar(stat = "identity", fill ="#FF6666", aes(color = I('black')), size = 0.1)+xlab("PAY_2") +
  ylab("Percent")+ theme_few()
credit.df %>% group_by(PAY_3) %>% dplyr::summarise(count=n()) %>% mutate(pct=count/sum(count))%>%ggplot(aes(x = PAY_3, y = pct)) + geom_bar(stat = "identity", fill ="#FF6666", aes(color = I('black')), size = 0.1)+xlab("PAY_3") +
  ylab("Percent")+ theme_few()
credit.df %>% group_by(PAY_4) %>% dplyr::summarise(count=n()) %>% mutate(pct=count/sum(count))%>%ggplot(aes(x = PAY_4, y = pct)) + geom_bar(stat = "identity", fill ="#FF6666", aes(color = I('black')), size = 0.1)+xlab("PAY_4") +
  ylab("Percent")+ theme_few()
credit.df %>% group_by(PAY_5) %>% dplyr::summarise(count=n()) %>% mutate(pct=count/sum(count))%>%ggplot(aes(x = PAY_5, y = pct)) + geom_bar(stat = "identity", fill ="#FF6666", aes(color = I('black')), size = 0.1)+xlab("PAY_5") +
  ylab("Percent")+ theme_few()

#Combining -2,-1,0 as 0 i.e. dully paid
levels(credit.df$PAY_0) <- c(0,0,0,1,2,3,4,5,6,7,8)
levels(credit.df$PAY_2) <- c(0,0,0,1,2,3,4,5,6,7,8)
levels(credit.df$PAY_3) <- c(0,0,0,1,2,3,4,5,6,7,8)
levels(credit.df$PAY_4) <- c(0,0,0,1,2,3,4,5,6,7,8)
levels(credit.df$PAY_5) <- c(0,0,0,1,2,3,4,5,6,7,8)
levels(credit.df$PAY_6) <- c(0,0,0,1,2,3,4,5,6,7,8)

#binning ages
breaks <- c(18,25,30,40,50,60,100)
tags <- c("18-24","25-29","30-39","40-49","50-59","60-100")
credit.df$age_group <- cut(credit.df$AGE,breaks = breaks,include.lowest=TRUE, right=FALSE, labels=tags)
summary(credit.df$age_group)

#Univariate Analysis
credit.df %>% group_by(MARRIAGE) %>% dplyr::summarise(count=n()) %>% mutate(pct=count/sum(count))%>%ggplot(aes(x = MARRIAGE, y = pct)) + geom_bar(stat = "identity", fill ="purple", aes(color = I('black')), size = 0.1)+xlab("MARRIAGE") +
  ylab("Percent")+ theme_few()

credit.df <- credit.df %>% dplyr::rename("default"="default.payment.next.month")
#Renaming "default.payment.next.month" to "default"
credit.df$default <- as.factor(credit.df$default)

credit.df %>% group_by(default) %>% dplyr::summarise(count=n()) %>% mutate(pct=count/sum(count))%>%ggplot(aes(x = default, y = pct)) + geom_bar(stat = "identity", fill ="#FF6666", aes(color = I('black')), size = 0.1)+xlab("default") +
  ylab("Percent")+ theme_few()


#Bi-Variate Analysis
ggplot(credit.df,aes(x = SEX, fill = default)) + geom_bar(position='fill') 
#Default rate is higer in males


ggplot(credit.df, aes(x =EDUCATION, fill = default)) + geom_bar(stat='count', position='fill') + labs(x = 'EDUCATION', y= 'Percent of default Vs No default')
#As Default rate decreses with increase in level of education, education could be an important variable


ggplot(credit.df, aes(x =PAY_5, fill = default)) + geom_bar(position='fill') + labs(x = 'PAY_5',y = "Percent of default Vs No default")
#As expected default rate increses with delay in payment

ggplot(credit.df, aes(x =PAY_0, fill = default)) + geom_bar(position='fill') + labs(x = 'PAY_0',y = "Percent of default Vs No default")

#Exploring Payment and Bill varibales
#Calculating outstanding amount by Payment in month 1 - Statement of month 2
options(scipen=999)
explore.df <- credit.df
explore.df$bal_diff <- explore.df$PAY_AMT1 - explore.df$BILL_AMT2
hist(explore.df$bal_diff)

length(explore.df$bal_diff[explore.df$bal_diff == 0])

explore.df %>% filter(explore.df$default == 'Default' & bal_diff >= 0) %>% nrow()
explore.df %>% filter(explore.df$default == 'Default' & bal_diff >= 0) %>% nrow() / dim(explore.df)[1] # As percentage of total 
#There is a sizable population for which which defaults even with a non-negative bal difference.
#We need to include payment history as well along with recent payment. Checking previous months bill amount for these customers.
explore.df %>% filter(default == 'Default' & bal_diff >= 0) %>% select(BILL_AMT3 > 0) %>% ggplot(aes(BILL_AMT3)) + geom_histogram(bins=30)
hist(explore.df[explore.df$bal_diff == 0 & explore.df$default == 'Non-Default',]$BILL_AMT3)
# BILL_AMT3 is higher but for a better understanding we should normalize it using credit limit 

#Creating variables for payment history
explore.df$bal_diff1 <- explore.df$PAY_AMT1 - explore.df$BILL_AMT2
explore.df$bal_diff2 <- explore.df$PAY_AMT2 - explore.df$BILL_AMT3
explore.df$bal_diff3 <- explore.df$PAY_AMT3 - explore.df$BILL_AMT4
explore.df$bal_diff4 <- explore.df$PAY_AMT4 - explore.df$BILL_AMT5
explore.df$bal_diff5 <- explore.df$PAY_AMT5 - explore.df$BILL_AMT6


summary(explore.df[explore.df$bal_diff1 >= 0 & explore.df$bal_diff2 >= 0 & explore.df$default == 'Default',]$BILL_AMT4)
dim(explore.df[explore.df$bal_diff1 >= 0 & explore.df$bal_diff2 >= 0 & explore.df$default == 'Default',])[1]
dim(explore.df[explore.df$bal_diff1 >= 0 & explore.df$bal_diff2 >= 0 & explore.df$bal_diff3 >= 0 & explore.df$default == 'Default',])[1]
#Still there is a sizable population that defaults even after paying the bills recently
#Is there nay other variable that explains this 

ggplot(explore.df[explore.df$bal_diff1 >= 0 & explore.df$bal_diff2 >= 0,],aes(x = MARRIAGE, fill = default)) + geom_bar(position='fill')
ggplot(explore.df[explore.df$bal_diff1 >= 0 & explore.df$bal_diff2 >= 0,],aes(x = SEX, fill = default)) + geom_bar(position='fill')
ggplot(explore.df[explore.df$bal_diff1 >= 0 & explore.df$bal_diff2 >= 0,],aes(x = EDUCATION, fill = default)) + geom_bar(position='fill')
#The distribution of defaulter and non defaulter is same in all the demographic variables, so they are not very useful in explaining this.

#Create new variable bal_diff by combining all balance difference

# May be we should not use BILL_AMT and PAY_AMT as is.

explore.df$bal_diff <- (explore.df$bal_diff1 + explore.df$bal_diff2 + explore.df$bal_diff3 + explore.df$bal_diff4 + explore.df$bal_diff5)
dim(explore.df[explore.df$bal_diff >= 0 & explore.df$default == 'Default',])[1]
#1252
dim(explore.df[explore.df$bal_diff >= 0 & explore.df$default == 'Non-Default',])[1]
#5928
dim(explore.df[explore.df$bal_diff < 0 & explore.df$default == 'Default',])[1]
#5384
hist(explore.df$bal_diff)
summary(explore.df$bal_diff)

# Though bal_diff is indicator for defaukt to some extent, it has a large range
#    Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
# -3201825  -244882   -81090  -192202     -396  2666322
# It's a good idea to see their credit card expenses based on their credit card limit

explore.df$exp5 <- (explore.df$BILL_AMT5 - explore.df$BILL_AMT6 - explore.df$PAY_AMT5)/ explore.df$LIMIT_BAL
explore.df$exp4 <- ((explore.df$BILL_AMT5 - explore.df$BILL_AMT6 - explore.df$PAY_AMT5) + (explore.df$BILL_AMT4 - explore.df$BILL_AMT5 - explore.df$PAY_AMT4)) / (2 * explore.df$LIMIT_BAL)
explore.df$exp3 <- ((explore.df$BILL_AMT5 - explore.df$BILL_AMT6 - explore.df$PAY_AMT6) +
                      (explore.df$BILL_AMT4 - explore.df$BILL_AMT5 - explore.df$PAY_AMT4) +
                      (explore.df$BILL_AMT3 - explore.df$BILL_AMT4 - explore.df$PAY_AMT3) ) / (3*explore.df$LIMIT_BAL)

explore.df$exp2 <- ((explore.df$BILL_AMT5 - explore.df$BILL_AMT6 - explore.df$PAY_AMT5) +
                      (explore.df$BILL_AMT4 - explore.df$BILL_AMT5 - explore.df$PAY_AMT4) +
                      (explore.df$BILL_AMT3 - explore.df$BILL_AMT4 - explore.df$PAY_AMT3) +
                      (explore.df$BILL_AMT2 - explore.df$BILL_AMT3 - explore.df$PAY_AMT2)) / (4*explore.df$LIMIT_BAL)

explore.df$exp1 <- ((explore.df$BILL_AMT5 - explore.df$BILL_AMT6 - explore.df$PAY_AMT5) +
                      (explore.df$BILL_AMT4 - explore.df$BILL_AMT5 - explore.df$PAY_AMT4) +
                      (explore.df$BILL_AMT3 - explore.df$BILL_AMT4 - explore.df$PAY_AMT3) +
                      (explore.df$BILL_AMT2 - explore.df$BILL_AMT3 - explore.df$PAY_AMT2) +
                      (explore.df$BILL_AMT1 - explore.df$BILL_AMT2 - explore.df$PAY_AMT1)) / (5*explore.df$LIMIT_BAL)


boxplot(explore.df[explore.df$default == '1',]$exp1)
# -0.5 to 0.5 
boxplot(explore.df[explore.df$default == '1',]$exp2)
# -0.4 to 0.4
boxplot(explore.df[explore.df$default == '1',]$exp3)
# -0.7 to 0.5
boxplot(explore.df[explore.df$default == '1',]$exp4)
# -1 to 0.8
boxplot(explore.df[explore.df$default == '1',]$exp5)
# -2 to 1

boxplot(explore.df[explore.df$default == 'Non-Default',]$exp1)
#-0.8 to 0.8
boxplot(explore.df[explore.df$default == 'Non-Default',]$exp2)
#-0.9 to 0.9
boxplot(explore.df[explore.df$default == 'Non-Default',]$exp3)
#-1.2 to 0.8
boxplot(explore.df[explore.df$default == 'Non-Default',]$exp4)
#-2 to 2
boxplot(explore.df[explore.df$default == 'Non-Default',]$exp5)
#-2.2 to 2

dim(explore.df[explore.df$exp1 > 0 & explore.df$exp2 > 0 & explore.df$exp3 > 0 & explore.df$exp4 > 0 & explore.df$exp5 > 0 &explore.df$default == 'Default',])[1]

dim(explore.df[explore.df$exp1 > 0 | explore.df$exp2 > 0 | explore.df$exp3 > 0 | explore.df$exp4 > 0 | explore.df$exp5 > 0 | explore.df$default == 'Default',])[1]

temp.df <- explore.df[explore.df$exp1 > 0 | explore.df$exp2 > 0 | explore.df$exp3 > 0 | explore.df$exp4 > 0 | explore.df$exp5 > 0 | explore.df$default == 'Default',]

temp.df %>% group_by(default) %>% dplyr::summarise(count=n()) %>% mutate(pct=count/sum(count))%>%ggplot(aes(x = default, y = pct)) + geom_bar(stat = "identity", fill ="purple", aes(color = I('black')), size = 0.1)+xlab("default") +
  ylab("Percent")+ theme_few()


temp.df <- explore.df[
  (explore.df$exp1 > 0 & explore.df$exp1 < 0.5) | 
    (explore.df$exp2 > 0 & explore.df$exp2 < 0.5) | 
    (explore.df$exp3 > 0 & explore.df$exp3 < 0.5) | 
    (explore.df$exp4 > 0 & explore.df$exp4 < 0.5) | 
    (explore.df$exp5 > 0 & explore.df$exp5 < 0.5) | 
    explore.df$default == 'Default',]

#Prediction Using Random forest
# We recreated all the new columns we created for exploration

credit.df$exp5 <- (credit.df$BILL_AMT5 - credit.df$BILL_AMT6 - credit.df$PAY_AMT5)/ credit.df$LIMIT_BAL
credit.df$exp4 <- ((credit.df$BILL_AMT5 - credit.df$BILL_AMT6 - credit.df$PAY_AMT5) + (credit.df$BILL_AMT4 - credit.df$BILL_AMT5 - credit.df$PAY_AMT4)) / (2 * credit.df$LIMIT_BAL)
credit.df$exp3 <- ((credit.df$BILL_AMT5 - credit.df$BILL_AMT6 - credit.df$PAY_AMT6) +
                     (credit.df$BILL_AMT4 - credit.df$BILL_AMT5 - credit.df$PAY_AMT4) +
                     (credit.df$BILL_AMT3 - credit.df$BILL_AMT4 - credit.df$PAY_AMT3) ) / (3*credit.df$LIMIT_BAL)

credit.df$exp2 <- ((credit.df$BILL_AMT5 - credit.df$BILL_AMT6 - credit.df$PAY_AMT5) +
                     (credit.df$BILL_AMT4 - credit.df$BILL_AMT5 - credit.df$PAY_AMT4) +
                     (credit.df$BILL_AMT3 - credit.df$BILL_AMT4 - credit.df$PAY_AMT3) +
                     (credit.df$BILL_AMT2 - credit.df$BILL_AMT3 - credit.df$PAY_AMT2)) / (4*credit.df$LIMIT_BAL)

credit.df$exp1 <- ((credit.df$BILL_AMT5 - credit.df$BILL_AMT6 - credit.df$PAY_AMT5) +
                     (credit.df$BILL_AMT4 - credit.df$BILL_AMT5 - credit.df$PAY_AMT4) +
                     (credit.df$BILL_AMT3 - credit.df$BILL_AMT4 - credit.df$PAY_AMT3) +
                     (credit.df$BILL_AMT2 - credit.df$BILL_AMT3 - credit.df$PAY_AMT2) +
                     (credit.df$BILL_AMT1 - credit.df$BILL_AMT2 - credit.df$PAY_AMT1)) / (5*credit.df$LIMIT_BAL)

credit.df$bal_diff1 <- credit.df$PAY_AMT1 - credit.df$BILL_AMT2
credit.df$bal_diff2 <- credit.df$PAY_AMT2 - credit.df$BILL_AMT3
credit.df$bal_diff3 <- credit.df$PAY_AMT3 - credit.df$BILL_AMT4
credit.df$bal_diff4 <- credit.df$PAY_AMT4 - credit.df$BILL_AMT5
credit.df$bal_diff5 <- credit.df$PAY_AMT5 - credit.df$BILL_AMT6

#removing ID and AGE
credit.df <- credit.df[,-c(1,6)]
train.index <- sample(c(1:dim(credit.df)[1]),0.7 * dim(credit.df)[1])
train.df <- credit.df[train.index, ]
valid.df <- credit.df[-train.index, ]
install.packages("randomForest")
library(randomForest) 
forest.df = randomForest(formula = default~., data = train.df, ntree = 100)
predict.df = predict(forest.df, newdata = valid.df, type = "class")
library(caret)
confusionMatrix(predict.df, valid.df$default)
roc.curve(valid.df$default,predict.df)

# Confusion Matrix and Statistics
# 
# Reference
# Prediction    0    1
# 0 6625 1246
# 1  389  740
# 
# Accuracy : 0.8183               
# 95% CI : (0.8102, 0.8263)     
# No Information Rate : 0.7793               
# P-Value [Acc > NIR] : < 0.00000000000000022
# 
# Kappa : 0.3752               
# 
# Mcnemar's Test P-Value : < 0.00000000000000022
#                                                
#             Sensitivity : 0.9445               
#             Specificity : 0.3726               
#          Pos Pred Value : 0.8417               
#          Neg Pred Value : 0.6554               
#              Prevalence : 0.7793               
#          Detection Rate : 0.7361               
#    Detection Prevalence : 0.8746               
#       Balanced Accuracy : 0.6586               
#                                                
#        'Positive' Class : 0                    
#                                                
# Area under the curve (AUC): 0.659


#Removing bal_diff
remove_feature = remove_feature = c(30:34)
credit_resized <- credit.df[,-remove_feature]
train.df <- credit_resized[train.index, ]
valid.df <- credit_resized[-train.index, ]
forest.df = randomForest(formula = default~., data = train.df, ntree = 100)
predict.df = predict(forest.df, newdata = valid.df, type = "class")
confusionMatrix(predict.df, valid.df$default)
roc.curve(valid.df$default,predict.df)

# Confusion Matrix and Statistics
# 
# Reference
# Prediction    0    1
# 0 6644 1239
# 1  370  747
# 
# Accuracy : 0.8212               
# 95% CI : (0.8131, 0.8291)     
# No Information Rate : 0.7793               
# P-Value [Acc > NIR] : < 0.00000000000000022
# 
# Kappa : 0.3835               
# 
# Mcnemar's Test P-Value : < 0.00000000000000022
#                                                
#             Sensitivity : 0.9472               
#             Specificity : 0.3761               
#          Pos Pred Value : 0.8428               
#          Neg Pred Value : 0.6688               
#              Prevalence : 0.7793               
#          Detection Rate : 0.7382               
#    Detection Prevalence : 0.8759               
#       Balanced Accuracy : 0.6617               
#                                                
#        'Positive' Class : 0                    
#                                                
# Area under the curve (AUC): 0.662

#Only original features
remove_feature = c(25:34)
credit_resized <- credit.df[,-remove_feature]
train.df <- credit_resized[train.index, ]
valid.df <- credit_resized[-train.index, ]
forest.df = randomForest(formula = default~., data = train.df, ntree = 100)
predict.df = predict(forest.df, newdata = valid.df, type = "class")
confusionMatrix(predict.df, valid.df$default)
roc.curve(valid.df$default,predict.df)

# Confusion Matrix and Statistics
# 
# Reference
# Prediction    0    1
# 0 6632 1246
# 1  382  740
# 
# Accuracy : 0.8191               
# 95% CI : (0.811, 0.827)       
# No Information Rate : 0.7793               
# P-Value [Acc > NIR] : < 0.00000000000000022
# 
# Kappa : 0.3769               
# 
# Mcnemar's Test P-Value : < 0.00000000000000022
#                                                
#             Sensitivity : 0.9455               
#             Specificity : 0.3726               
#          Pos Pred Value : 0.8418               
#          Neg Pred Value : 0.6595               
#              Prevalence : 0.7793               
#          Detection Rate : 0.7369               
#    Detection Prevalence : 0.8753               
#       Balanced Accuracy : 0.6591               
#                                                
#        'Positive' Class : 0                    
#                                                
# Area under the curve (AUC): 0.659

#removing EXP_N
remove_feature = remove_feature = c(25:29)
credit_resized <- credit.df[,-remove_feature]
train.df <- credit_resized[train.index, ]
valid.df <- credit_resized[-train.index, ]
forest.df = randomForest(formula = default~., data = train.df, ntree = 100)
predict.df = predict(forest.df, newdata = valid.df, type = "class")
confusionMatrix(predict.df, valid.df$default)
roc.curve(valid.df$default,predict.df)

# Confusion Matrix and Statistics
# 
# Reference
# Prediction    0    1
# 0 6643 1256
# 1  371  730
# 
# Accuracy : 0.8192               
# 95% CI : (0.8111, 0.8271)     
# No Information Rate : 0.7793               
# P-Value [Acc > NIR] : < 0.00000000000000022
# 
# Kappa : 0.3745               
# 
# Mcnemar's Test P-Value : < 0.00000000000000022
#                                                
#             Sensitivity : 0.9471               
#             Specificity : 0.3676               
#          Pos Pred Value : 0.8410               
#          Neg Pred Value : 0.6630               
#              Prevalence : 0.7793               
#          Detection Rate : 0.7381               
#    Detection Prevalence : 0.8777               
#       Balanced Accuracy : 0.6573               
#                                                
#        'Positive' Class : 0                    
#                                                
# Area under the curve (AUC): 0.657


#Over Sampling
remove_feature = remove_feature = c(30:34)
credit.df1 <- credit.df[,-remove_feature]
train.df <- credit.df1[train.index, ]
valid.df <- credit.df1[-train.index, ]
train_balanced <- ovun.sample(default ~ ., data = train.df, method = "over",N = 40000)$data
table(train_balanced$default)
forest.df = randomForest(formula = default~., data = train_balanced, ntree = 30)
predict.df = predict(forest.df, newdata = valid.df, type = "class")
confusionMatrix(predict.df, valid.df$default)
roc.curve(valid.df$default,predict.df)

# Confusion Matrix and Statistics
# 
# Reference
# Prediction    0    1
# 0 6351 1099
# 1  663  887
# 
# Accuracy : 0.8042               
# 95% CI : (0.7959, 0.8124)     
# No Information Rate : 0.7793               
# P-Value [Acc > NIR] : 0.000000004218       
# 
# Kappa : 0.3822               
# 
# Mcnemar's Test P-Value : < 0.00000000000000022
#                                                
#             Sensitivity : 0.9055               
#             Specificity : 0.4466               
#          Pos Pred Value : 0.8525               
#          Neg Pred Value : 0.5723               
#              Prevalence : 0.7793               
#          Detection Rate : 0.7057               
#    Detection Prevalence : 0.8278               
#       Balanced Accuracy : 0.6761               
#                                                
#        'Positive' Class : 0                    
#                                                
# Area under the curve (AUC): 0.676

#UnderSampling
remove_feature = remove_feature = c(30:34)
credit.df1 <- credit.df[,-remove_feature]
train.df <- credit.df1[train.index, ]
valid.df <- credit.df1[-train.index, ]
train_balanced <- ovun.sample(default ~ ., data = train.df, method = "under",N = 12000)$data
table(train_balanced$default)
forest.df = randomForest(formula = default~., data = train_balanced, ntree = 100)
predict.df = predict(forest.df, newdata = valid.df, type = "class")
confusionMatrix(predict.df, valid.df$default)
roc.curve(valid.df$default,predict.df)
# Confusion Matrix and Statistics
# 
# Reference
# Prediction    0    1
# 0 6052  932
# 1  962 1054
# 
# Accuracy : 0.7896         
# 95% CI : (0.781, 0.7979)
# No Information Rate : 0.7793         
# P-Value [Acc > NIR] : 0.009733       
# 
# Kappa : 0.3914         
# 
# Mcnemar's Test P-Value : 0.505182       
#                                          
#             Sensitivity : 0.8628         
#             Specificity : 0.5307         
#          Pos Pred Value : 0.8666         
#          Neg Pred Value : 0.5228         
#              Prevalence : 0.7793         
#          Detection Rate : 0.6724         
#    Detection Prevalence : 0.7760         
#       Balanced Accuracy : 0.6968         
#                                          
#        'Positive' Class : 0              
#                                          
# > roc.curve(valid.df$default,predict.df)
# Area under the curve (AUC): 0.697

#Both Over and Under Sampling
remove_feature = remove_feature = c(30:34)
credit.df1 <- credit.df[,-remove_feature]
train.df <- credit.df1[train.index, ]
valid.df <- credit.df1[-train.index, ]
train_balanced <- ovun.sample(default ~ ., data = train.df, method = "both",p=0.5,N = 40000)$data
forest.df = randomForest(formula = default~., data = train_balanced, ntree = 100)
predict.df = predict(forest.df, newdata = valid.df, type = "class")
confusionMatrix(predict.df, valid.df$default)
roc.curve(valid.df$default,predict.df)

# Confusion Matrix and Statistics
# 
# Reference
# Prediction    0    1
# 0 6239 1022
# 1  775  964
# 
# Accuracy : 0.8003          
# 95% CI : (0.7919, 0.8085)
# No Information Rate : 0.7793          
# P-Value [Acc > NIR] : 0.000000629990  
# 
# Kappa : 0.3924          
# 
# Mcnemar's Test P-Value : 0.000000006509  
#                                           
#             Sensitivity : 0.8895          
#             Specificity : 0.4854          
#          Pos Pred Value : 0.8592          
#          Neg Pred Value : 0.5543          
#              Prevalence : 0.7793          
#          Detection Rate : 0.6932          
#    Detection Prevalence : 0.8068          
#       Balanced Accuracy : 0.6875          
#                                           
#        'Positive' Class : 0               
#                                           
# Area under the curve (AUC): 0.687

#ROSE Method
remove_feature = remove_feature = c(30:34)
credit.df1 <- credit.df[,-remove_feature]
train.df <- credit.df1[train.index, ]
valid.df <- credit.df1[-train.index, ]
train_balanced <- ROSE(default ~ ., data = train.df)$data
forest.df = randomForest(formula = default~., data = train_balanced, ntree = 100)
predict.df = predict(forest.df, newdata = valid.df, type = "class")
confusionMatrix(predict.df, valid.df$default)
roc.curve(valid.df$default,predict.df)

# Confusion Matrix and Statistics
# 
# Reference
# Prediction    0    1
# 0 1370  119
# 1 5644 1867
# 
# Accuracy : 0.3597             
# 95% CI : (0.3497, 0.3697)   
# No Information Rate : 0.7793             
# P-Value [Acc > NIR] : 1                  
# 
# Kappa : 0.0678             
# 
# Mcnemar's Test P-Value : <0.0000000000000002
#                                              
#             Sensitivity : 0.1953             
#             Specificity : 0.9401             
#          Pos Pred Value : 0.9201             
#          Neg Pred Value : 0.2486             
#              Prevalence : 0.7793             
#          Detection Rate : 0.1522             
#    Detection Prevalence : 0.1654             
#       Balanced Accuracy : 0.5677             
#                                              
#        'Positive' Class : 0                  
#                                              
# > roc.curve(valid.df$default,predict.df)
# Area under the curve (AUC): 0.568
#Though this has very low accuracy false negatives are also the lowest false negative





test.df <- read.csv("/Users/tapas/Downloads/Credit_card_data.csv")
#Splitting data into monthly cross sections
sep.df <- test.df[,c("Customer_ID","LIMIT_BAL","SEX","EDUCATION","MARRIAGE","age_group","PAY_0","BILL_AMT1","PAY_AMT1","default")]
sep.df <- sep.df %>% dplyr::rename("Payment_Status"="PAY_0","bill_amt"="BILL_AMT1","pay_amt"="PAY_AMT1")

aug.df <- test.df[,c("Customer_ID","LIMIT_BAL","SEX","EDUCATION","MARRIAGE","age_group","PAY_2","BILL_AMT2","PAY_AMT2","default")]
aug.df <- aug.df %>% dplyr::rename("Payment_Status"="PAY_2","bill_amt"="BILL_AMT2","pay_amt"="PAY_AMT2")

july.df <- test.df[,c("Customer_ID","LIMIT_BAL","SEX","EDUCATION","MARRIAGE","age_group","PAY_3","BILL_AMT3","PAY_AMT3","default")]
july.df <- july.df %>% dplyr::rename("Payment_Status"="PAY_3","bill_amt"="BILL_AMT3","pay_amt"="PAY_AMT3")

june.df <- test.df[,c("Customer_ID","LIMIT_BAL","SEX","EDUCATION","MARRIAGE","age_group","PAY_4","BILL_AMT4","PAY_AMT4","default")]
june.df <- june.df %>% dplyr::rename("Payment_Status"="PAY_4","bill_amt"="BILL_AMT4","pay_amt"="PAY_AMT4")

may.df <- test.df[,c("Customer_ID","LIMIT_BAL","SEX","EDUCATION","MARRIAGE","age_group","PAY_5","BILL_AMT5","PAY_AMT5","default")]
may.df <- may.df %>% dplyr::rename("Payment_Status"="PAY_5","bill_amt"="BILL_AMT5","pay_amt"="PAY_AMT5")

apr.df <- test.df[,c("Customer_ID","LIMIT_BAL","SEX","EDUCATION","MARRIAGE","age_group","PAY_6","BILL_AMT6","PAY_AMT6","default")]
apr.df <- apr.df %>% dplyr::rename("Payment_Status"="PAY_6","bill_amt"="BILL_AMT6","pay_amt"="PAY_AMT6")

#Adding month column to each cross-section
apr.df$month = "apr"
may.df$month = "may"
june.df$month = "june"
july.df$month = "july"
aug.df$month = "aug"
sep.df$month = "sep"
panel_train$month <- as.factor(panel_train$month)


#Appending all cross-section to form panel data
panel_df <- rbind(apr.df,may.df,june.df,july.df,aug.df,sep.df)
dim(panel_df)

#Creating train test split based on customer id
panel_train <- panel_df[panel_df$Customer_ID<21000,-1]
dim(panel_train)

panel_valid <- panel_df[panel_df$Customer_ID>=21000,-1]
dim(panel_valid)

library(stats)

#Replacing month labels with numbers
levels(panel_train$month) <- c(1,2,3,4,5,6)

panel_train$month <- as.numeric(panel_train$month)
panel_train$month

linear_model <- lm(default~ LIMIT_BAL + SEX + EDUCATION + MARRIAGE + age_group + Payment_Status + bill_amt + pay_amt + month + I((Payment_Status * month)),data = panel_train)
linear_model <- lm(default~ LIMIT_BAL + SEX + EDUCATION + MARRIAGE + age_group + Payment_Status + bill_amt + pay_amt + month + I((Payment_Status * month)) + I((bill_amt*month)) + I((pay_amt*month)),data = panel_train)
summary(linear_model)

panel_train$month
panel_train$month <- as.factor(panel_train$month)
linear_model <- lm(default~ LIMIT_BAL + SEX + EDUCATION + MARRIAGE + age_group + Payment_Status + bill_amt + pay_amt + month +bill_amt*month ,data = panel_train)
summary(linear_model)

linear_model <- lm(default~ LIMIT_BAL + SEX + EDUCATION + MARRIAGE + age_group + Payment_Status + bill_amt + pay_amt + month + Payment_Status * month + bill_amt*month + pay_amt*month,data = panel_train)
panel_valid$month <- as.factor(panel_valid$month)

levels(panel_valid$month) <- c(1,2,3,4,5,6)
predictions <- predict(linear_model,panel_valid)
actuals_preds <- data.frame(cbind(actuals=panel_valid$default, predicteds=predictions))
correlation_accuracy <- cor(actuals_preds) 
head(actuals_preds)

summary(linear_model)
logistic_model <- glm(default~ LIMIT_BAL + SEX + EDUCATION + MARRIAGE + age_group + Payment_Status + bill_amt + pay_amt + month + Payment_Status * month + bill_amt*month + pay_amt*month,data = panel_train)
summary(logistic_model)

plot(logistic_model$residuals)
plot(linear_model$residuals)
fitted.results <- predict(logistic_model,newdata=panel_valid,type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)
misClasificError <- mean(fitted.results != panel_valid$default)
print(paste('Accuracy',1-misClasificError))

install.packages("ROCR")
library(ROCR)
p <- predict(logistic_model, newdata=panel_valid, type="response")
pr <- prediction(p, panel_valid$default)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc

#TimeSeries Model
panel_train <- panel_df[panel_df$Customer_ID<21000,]
dim(panel_train)

panel_valid <- panel_df[panel_df$Customer_ID>=21000,]
dim(panel_valid)

panel_train$month <- as.factor(panel_train$month)
panel_valid$month <- as.factor(panel_valid$month)

timeseries_logistic_model <- glm(default~ LIMIT_BAL + SEX + EDUCATION + MARRIAGE + age_group + Payment_Status + bill_amt + pay_amt + month ,data = panel_train)
summary(timeseries_logistic_model)
timeseries_logistic_predictions <- predict(timeseries_logistic_model,newdata=panel_valid,type='response')
timeseries_logistic_predictions <- ifelse(timeseries_logistic_predictions > 0.8,1,0)
misClasificError <- mean(timeseries_logistic_predictions != panel_valid$default)
print(paste('Accuracy',1-misClasificError))

     