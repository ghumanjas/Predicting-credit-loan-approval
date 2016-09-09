##clean all objects from memory
rm(list=ls())

#Running the packages#
library(dplyr)
library(VIM)
library(mice)
library(ggplot2)

#Setting up the working drive#
getwd()
setwd("D:/R/Analytics vidhya/Hackathon/Loan Prediction")

#Checking what files are there in the working drive#
dir("D:/R/Analytics vidhya/Hackathon/Loan Prediction")

#Reading the train and test dataset#
train <- read.csv('train_home loan.csv',stringsAsFactors = FALSE,na.strings=c("","NA"))
test <- read.csv('test_home loan.csv',stringsAsFactors = FALSE,na.strings=c("","NA"))

#Checking train dataset#
str(train)

#Combining Test and Train dataset using tags for missing value imputations#
test$Loan_Status<- 2
test$Tag<- "test"
train$Tag<- "train"
train$Loan_Status <- ifelse(train$Loan_Status=="Y",1,0)
data<- rbind(train,test)

head(data)
summary(data)


#Converting characters to factors#
data$Gender <- as.factor(data$Gender)
data$Married <- as.factor(data$Married)
data$Education <- as.factor(data$Education)
data$Self_Employed <- as.factor(data$Self_Employed)
data$Property_Area <- as.factor(data$Property_Area)
data$Loan_Status <- as.factor(data$Loan_Status)
data$Dependents <- as.factor(data$Dependents)
str(data)

#Missing values#
table(is.na(data))
colSums(is.na(data))
summary(data)

#Visual represenatation of missing data#
aggr_plot <- aggr(data, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(data), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))

#Missing value imputation with MICE#
data1 <- mice(data = data[,c("Gender","Married","Dependents","Education","Self_Employed","Loan_Amount_Term","Credit_History","Property_Area","Tag")], m=7,maxit=100,seed = 500)
summary(data1)

#Check which method was used for missing data imputation#
data1$meth

#Combining imputed data with the actual combined sheet#
data_imputed<- complete(data1,1)
imp_data<- cbind(data,data_imputed$Gender,data_imputed$Married,data_imputed$Dependents, data_imputed$Education,data_imputed$Self_Employed,data_imputed$Loan_Amount_Term,data_imputed$Credit_History,data_imputed$Property_Area)
write.csv(imp_data,"impdata.csv",row.names = FALSE)

#Dropping earlier columns from Data#
names(imp_data)
df<- subset(imp_data, select= -c(Gender,Married,Dependents,Education,Self_Employed,Loan_Amount_Term,Credit_History,Property_Area))
summary(df)
write.csv(df,"df.csv",row.names = FALSE)

#Checking for pending missing values and imputing loan amount missing values with mean#
colSums(is.na(df))
df$LoanAmount[is.na(df$LoanAmount)]<- mean(df$LoanAmount, na.rm = TRUE)
summary(df$LoanAmount)

#Changing lable names for the imputed labels#
names(df)
names(df)[7:14]= c("Gender","Married","Dependents","Education","Self_Employed","Loan_Amount_Term","Credit_History","Property_Area")
names(df)

#Feature engineering - Label encoding, one hot encoding, New features#
str(df)
#Label Encoding# 
df$Gender <- ifelse(df$Gender=="Male",1,0)
df$Married <- ifelse(df$Married=="Yes",1,0)
df$Self_Employed <- ifelse(df$Self_Employed=="Yes",1,0)
df$Education <- ifelse(df$Education=="Graduate",1,0)

#One hot Encoding#
library(dummies)
names(df)
df <- dummy.data.frame(df, names = c('Dependents', 'Property_Area',  sep='_'))
str(df)

#Total income = Applicant+Co-applicant#, Debt ratio = loan amount/income#, Loan amount term binning#
# Creating new features#
df$TotalIncome <- df$ApplicantIncome + df$CoapplicantIncome
df$ApplicantIncome_Zero <- ifelse(df$ApplicantIncome==0,1,0)
df$DebtRatio_Mainapplicant <- df$LoanAmount/df$ApplicantIncome
df$DebtRatio_TotalIncome <- df$LoanAmount/df$TotalIncome
df$CoappIncGApplinc <- ifelse(df$CoapplicantIncome-df$ApplicantIncome>0,1,0)

df$Loan_Amount_Term1 <- ifelse(df$Loan_Amount_Term< 13,"0-12 mnths",ifelse(df$Loan_Amount_Term<121,"12-120 mnths",ifelse(df$Loan_Amount_Term<241,"120-240 mnths",ifelse(df$Loan_Amount_Term<361,"240-360 mnths","360+ Mnths"))))
df <- dummy.data.frame(df, names = c('Loan_Amount_Term1',  sep='_'))
str(df)

df$Gender_Married <- df$Gender*df$Married  #Male married increases your chances of loan approval(74%)

#Generating interaction between married and dependents#
df$Married_Dep0 <- df$Married*df$Dependents0
df$Married_Dep1 <- df$Married*df$Dependents1
df$Married_Dep2 <- df$Married*df$Dependents2
df$Married_Dep3 <- df$Married*df$`Dependents3+`

#Treating outliers in Loan amount and Incomes by log transformation#
ggplot(data=df, aes(df$LoanAmount)) + geom_histogram()
df$LoanAmount=log(df$LoanAmount)
ggplot(data=df, aes(df$LoanAmount)) + geom_histogram()
ggplot(data=df, aes(df$ApplicantIncome)) + geom_histogram()
ggplot(data=df, aes(df$CoapplicantIncome)) + geom_histogram()
ggplot(data=df, aes(df$TotalIncome)) + geom_histogram()
df$TotalIncome=log(df$TotalIncome) #Transforming only the total income, Keeping individual incomes intact#
ggplot(data=df, aes(df$TotalIncome)) + geom_histogram()

#dropping variables which are not required#
#Loan amount term#
df <- select(df,-c(Loan_Amount_Term))
names(df)
str(df)

#####Changing the data type######
df$Loan_Status <- as.factor(df$Loan_Status)
df$Gender <- as.factor(df$Gender)
df$Married <- as.factor(df$Married)
df$Dependents0 <- as.factor(df$Dependents0)
df$Dependents1 <- as.factor(df$Dependents1)
df$Dependents2 <- as.factor(df$Dependents2)
df$`Dependents3+`<- as.factor(df$`Dependents3+`)
df$Education <- as.factor(df$Education)
df$Self_Employed <- as.factor(df$Self_Employed)
df$Credit_History <- as.factor(df$Credit_History)
df$Property_AreaRural <- as.factor(df$Property_AreaRural)
df$Property_AreaSemiurban <- as.factor(df$Property_AreaSemiurban)
df$Property_AreaUrban <- as.factor(df$Property_AreaUrban)
df$ApplicantIncome_Zero <- as.factor(df$ApplicantIncome_Zero)
df$CoappIncGApplinc <- as.factor(df$CoappIncGApplinc)
df$`Loan_Amount_Term10-12 mnths`<- as.factor(df$`Loan_Amount_Term10-12 mnths`)
df$`Loan_Amount_Term1360+ Mnths`<- as.factor(df$`Loan_Amount_Term1360+ Mnths`)
df$`Loan_Amount_Term112-120 mnths`<- as.factor(df$`Loan_Amount_Term112-120 mnths`)
df$`Loan_Amount_Term1120-240 mnths`<- as.factor(df$`Loan_Amount_Term1120-240 mnths`)
df$`Loan_Amount_Term1240-360 mnths`<- as.factor(df$`Loan_Amount_Term1240-360 mnths`)
df$Gender_Married <- as.factor(df$Gender_Married)
df$Married_Dep0 <- as.factor(df$Married_Dep0)
df$Married_Dep1 <- as.factor(df$Married_Dep1)
df$Married_Dep2 <- as.factor(df$Married_Dep2)
df$Married_Dep3 <- as.factor(df$Married_Dep3)
str(df)

#Splitting Train and test data-set#
Train <- df[df$Tag=="train",]
Test  <- df[df$Tag=="test",]
Train$Tag <- NULL
Test$Tag  <- NULL
Test$Loan_Status  <- NULL
Train$Loan_Status <- factor(Train$Loan_Status) ## This is done to remove original factor levels which are retained even after manipulation##

write.csv(Train,"TrainD.csv",row.names=FALSE)
write.csv(Test,"TestD.csv",row.names=FALSE)




