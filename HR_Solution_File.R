library(googleVis)
library(ggplot2)
library(caret)
library(gbm)
library(MASS)
setwd("G:\\Downloads\\New folder")#setting working directory
#Loading train dataset
R_train = read.csv("hr_train.csv",   stringsAsFactors = FALSE)
#Looking at the data set
str(R_train)
#Checking if we have any NA values in the dataset
apply(R_train, 2, function(x) any(is.na(x)))
#Loading test dataset
R_test = read.csv("hr_test.csv",   stringsAsFactors = FALSE)
str(R_test)
#Adding 0 values in the left column of test data
R_test$left=rep(0,4500)
#Looking at new dataset
str(R_test)
#Checking if we have any NA values in the dataset
apply(R_test, 2, function(x) any(is.na(x)))

library(dplyr) #Loading library dplyr
#Joining both datasets

R = bind_rows(R_train,R_test)
str(R)
#Checking if we have any NA values in the dataset
apply(R, 2, function(x) any(is.na(x)))

#Data Cleaing
library(dplyr) #calling library
table(R$salary)
round(prop.table(table(R$salary,R$left),1),2)
#Assigning categories
#1 : high 0.90
#2 : medium 0.81
R=R %>% mutate (sal_1 = as.numeric(salary %in% c("high")),
                sal_2 = as.numeric(salary %in% c("medium"))) %>% 
  select(-salary)




#Assigning categories
#1 : accounting, hr 0.76
#2 : product_mng, support 0.80
#3 :sales, technical 0.79
#4 : IT 0.81
#5 : Management 0.84
#6 : RandD

table(R$sales)
round(prop.table(table(R$sales,R$left),1),2)
R = R %>%  mutate(s_1 = as.numeric(sales %in% c("management")),
                  s_2 = as.numeric(sales %in% c("RandD")),
                  s_3 = as.numeric(sales %in% c("IT")),
                  s_4 = as.numeric(sales %in% c("product_mng","support")),
                  s_5 = as.numeric(sales %in% c("sales","technical")),
                  s_6 = as.numeric(sales %in% c("accounting","hr")))  %>% 
  select(-sales)

str(R)              

#Splitting the dataset back 
R_train <- R[1:10499,]
R_test <- R[10500:14999,]
R_test$left = NULL

str(R_train)
str(R_test)
R_train_1 <- R_train[1:8000,]
R_train_2 <- R_train[8001:10499,]

str(R_train_1)
str(R_train_2)

library(car) #This is use for checking up VIF 
R_fit <- lm(left ~ ., data=R_train_1)
summary(R_fit)

R_fit <- lm(left ~.-s_6, data=R_train_1)
summary(R_fit)


R_fit <- lm(left ~.-s_6 -s_5, data=R_train_1)
summary(R_fit)


R_fit <- lm(left ~.-s_6 -s_5 -s_2, data=R_train_1)
summary(R_fit)

R_fit <- lm(left ~.-s_6 -s_5 -s_2 -s_4, data=R_train_1)
summary(R_fit)

R_fit <- lm(left ~.-s_6 -s_5 -s_2 -s_4 -promotion_last_5years, data=R_train_1)
summary(R_fit)

R_fit <- lm(left ~.-s_6 -s_5 -s_2 -s_4 -promotion_last_5years -s_3, data=R_train_1)
summary(R_fit)

mod_LR_exp <- coef(summary(R_fit))
mod_LR_exp[, "Estimate"] <- exp(coef(R_fit))
mod_LR_exp
str(R_test)
str(R_train_1)
str(R_train_2)

library(randomForest)


mod_RF <- randomForest(left ~.-s_6 -s_5 -s_2 -s_4 -promotion_last_5years -s_3, R_train_1)
mod_RF

pred_RF <- predict(mod_RF, R_train_2, type="class")
View(pred_RF)

pred_RF_new <- predict(mod_RF, R_test, type="class")
pred_RF_new


R_test$left = pred_RF_new

View(R_test$left)

#Submitting the file 
submit = data.frame (left= R_test$left)
#Giving name to the file
write.csv(submit, file = "Atul_Kumar_P4_part2.csv", row.names = FALSE)
