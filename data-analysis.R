
## Import the dataset

data <- read.csv("Loan.csv")


## View the dataset
View(data)

## check the data types of the dataset

str(data)

## summary

summary(data)

## attach the dataset with environment

attach(data)

## View the dataset again

View(data)

## check the null values of the dataset

data[!complete.cases(data),]
