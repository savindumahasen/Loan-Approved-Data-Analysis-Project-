
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
