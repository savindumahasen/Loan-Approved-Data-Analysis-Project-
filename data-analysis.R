
## Import the dataset

data <- read.csv("Loan.csv")


## View the dataset
View(data)

## check the data types of the dataset

str(data)
## 
head(data)

## summary

summary(data)

## attach the dataset with environment

attach(data)

## View the dataset again

View(data)

## check the null values of the dataset

data[!complete.cases(data),]

## Preparing the dataset (Preprocessing)

## install the plyr package 
install.packages('plyr')

## call the plyr library
library('plyr')

## convert the catgerical text values into the numerical values
data$EmploymentStatus <- revalue(EmploymentStatus,c(Employed=1,SelfEmployed=3,Unemployed=3))
data$EmploymentStatus
data$EducationLevel <- revalue(EducationLevel, c(Master=1, Associate=2, Bachelor=3,HighSchool=4,Doctorate=5))
data$EducationLevel
data$MaritalStatus <- revalue(MaritalStatus, c(Married=1,Single=2,Divorced=3,Widowed=4))
data$MaritalStatus
data$HomeOwnershipStatus <- revalue(HomeOwnershipStatus, c(Own=1,Mortgage=2,Rent=3, Other=4))
data$HomeOwnershipStatus
data$LoanPurpose <- revalue(LoanPurpose, c(Home=1,DebtConsolidation=2, Education=3, Other=4,Auto=5))
data$LoanPurpose

## writing the modified dataset into the new csv file
write.csv(data,"data_new.csv")

## Load the new dataset
data_new <- read.csv("data_new.csv")

## View the  new datset
View(data_new)

## check the data types  and number of objects
str(data_new)

## Normality testing
## install the nortest package
install.packages("nortest")
## call the library
library("nortest")
## Loan Approved
ad.test(LoanApproved)
lillie.test(LoanApproved)
shapiro.test(LoanApproved)

## Age
ad.test(Age)
lillie.test(Age)
shapiro.test(Age)

## Annual Income
ad.test(AnnualIncome)
lillie.test(AnnualIncome)
shapiro.test(AnnualIncome)

## Credit Sore
ad.test(CreditScore)
lillie.test(CreditScore)
shapiro.test(CreditScore)

## Employment Status
ad.test(data$EmploymentStatus)
lillie.test(EmploymentStatus)
