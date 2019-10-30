loan <- read.csv('/home/tapas/data/loan.csv')
library(dplyr)
loan1 <- loan %>% select(addr_state,all_util,annual_inc,annual_inc_joint,application_type,avg_cur_bal,bc_open_to_buy,delinq_amnt,dti,dti_joint,emp_length,funded_amnt,funded_amnt_inv,grade,home_ownership,id,il_util,initial_list_status,inq_fi,inq_last_12m,inq_last_6mths,installment,int_rate,last_pymnt_amnt,last_pymnt_d,loan_amnt,loan_status,max_bal_bc,mort_acc,mths_since_last_delinq,mths_since_recent_bc_dlq,mths_since_recent_inq,num_actv_bc_tl,num_tl_120dpd_2m,out_prncp,out_prncp_inv,pub_rec,pub_rec_bankruptcies,purpose,pymnt_plan,sub_grade,tax_liens,term,title,tot_cur_bal,tot_hi_cred_lim,total_acc,total_bal_ex_mort,total_bal_il,zip_code,hardship_flag,hardship_status)
loan1 <- filter(loan1, loan1$loan_status == "Fully Paid" | loan1$loan_status == "Charged Off")
loan1 <- mutate(loan1,binary_status = as.numeric(ifelse(loan1$loan_status == "Fully Paid", 1, 0)))
set.seed(101)
library(caTools)
temp = sample.split(loan1$binary_status,SplitRatio = 0.8)
loan_train = subset(loan1,temp==TRUE)
loan_test = subset(loan1,temp==FALSE)
loan_train <- loan_train [,-16] #Remove id
loan_train <- loan_train [,-26] #Remove status
library(rpart)
tree = rpart(binary_status~addr_state+annual_inc_joint+bc_open_to_buy+dti_joint+funded_amnt_inv+il_util+inq_last_12m+int_rate+loan_amnt+mort_acc+mths_since_recent_inq+out_prncp+pub_rec_bankruptcies+sub_grade+title+total_acc+zip_code+binary_status+all_util+application_type+delinq_amnt+emp_length+grade+initial_list_status+inq_last_6mths+last_pymnt_amnt+mths_since_last_delinq+num_actv_bc_tl+out_prncp_inv+purpose+tax_liens+tot_cur_bal+total_bal_ex_mort+hardship_flag+annual_inc+avg_cur_bal+dti+funded_amnt+home_ownership+inq_fi+installment+last_pymnt_d+max_bal_bc+mths_since_recent_bc_dlq+num_tl_120dpd_2m+pub_rec+pymnt_plan+term+tot_hi_cred_lim+total_bal_il+hardship_status, data = loan_train,method="class",minsplit=2,minbucket = 1)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
fancyRpartPlot(tree, caption = NULL)
tree$variable.importance
library(ggplot2)
library(lattice)
library(caret)
varImp(tree)

# application_type            561.2210
# funded_amnt                6797.6930
# funded_amnt_inv            6805.9084
# grade                     52534.9270
# hardship_status             403.9324
# initial_list_status        2648.0009
# installment                6243.3326
# int_rate                  51379.4782
# last_pymnt_amnt          108763.8242
# last_pymnt_d              36219.9488
# loan_amnt                  2060.7005
# sub_grade                 54338.5614
# term                      45347.0249
# addr_state                    0.0000
# annual_inc_joint              0.0000
# bc_open_to_buy                0.0000
# dti_joint                     0.0000
# il_util                       0.0000
# inq_last_12m                  0.0000
# mort_acc                      0.0000
# mths_since_recent_inq         0.0000
# out_prncp                     0.0000
# pub_rec_bankruptcies          0.0000
# total_acc                     0.0000
# zip_code                      0.0000
# binary_status                 0.0000
# all_util                      0.0000
# delinq_amnt                   0.0000
# emp_length                    0.0000
# inq_last_6mths                0.0000
# mths_since_last_delinq        0.0000
# num_actv_bc_tl                0.0000
# out_prncp_inv                 0.0000
# purpose                       0.0000
# tax_liens                     0.0000
# tot_cur_bal                   0.0000
# total_bal_ex_mort             0.0000
# hardship_flag                 0.0000
# annual_inc                    0.0000
# avg_cur_bal                   0.0000
# dti                           0.0000
# home_ownership                0.0000
# inq_fi                        0.0000
# max_bal_bc                    0.0000
# mths_since_recent_bc_dlq      0.0000
# num_tl_120dpd_2m              0.0000
# pub_rec                       0.0000
# pymnt_plan                    0.0000
# tot_hi_cred_lim               0.0000
# total_bal_il                  0.0000



