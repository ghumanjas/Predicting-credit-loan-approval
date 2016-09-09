drive <- "D:/R/Analytics vidhya/Hackathon/Loan Prediction"
setwd(drive)
train <- read.csv("TrainD.csv", header = TRUE, stringsAsFactors = FALSE)
test <- read.csv("TestD.csv", header = TRUE, stringsAsFactors = FALSE)

head(train)

######USing all parameters for prediction#######
logistic <- glm(Loan_Status ~ ., data=train[,!colnames(train) %in% c("Loan_ID")],
                family='binomial')
summary(logistic)
pred = predict(logistic, newdata=train,type="response")
pred <- as.integer(ifelse(pred>"0.499",1,0))
str(pred)
confusionMatrix(data=pred, train$Loan_Status)



######USing relevant selected parameters for prediction#######
logistic1 <- glm(Loan_Status ~ Credit_History+Property_AreaSemiurban+DebtRatio_TotalIncome+
                Loan_Amount_Term1120.240.mnths+Loan_Amount_Term1240.360.mnths  , data=train[,!colnames(train) %in% c("Loan_ID")],
                family='binomial')
summary(logistic1)
pred1 = predict(logistic1, newdata=train,type="response")
pred1 <- as.integer(ifelse(pred>"0.499",1,0))
str(pred1)
confusionMatrix(data=pred1, train$Loan_Status)

####The accuracy doesnt improve######
##Accuracy at 0.8192##
##LB Accuracy is 0.76##

#Predict Output
predicted= predict(logistic,test,type = "response")
head(predicted)
predicted <- as.integer(ifelse(predicted>"0.499",1,0))
head(predicted)
library(dplyr)
test1 <- test %>% select (Loan_ID)
comb <- data.frame(test1,predicted)
write.csv(comb,"12345.csv",row.names = FALSE)
