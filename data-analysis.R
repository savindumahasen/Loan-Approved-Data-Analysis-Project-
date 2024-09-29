
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

## Summary statistics of the dataset
summary(data_new)

## detach existing dataset
detach(data)

## Attach the updated dataset with enviornment
attach(data_new)

## View the updated dataset
View(data_new)

## Normality testing
## install the nortest package
install.packages("nortest")
## call the library
library("nortest")
## Loan Approved
ad.test(LoanApproved)
lillie.test(LoanApproved)
shapiro.test(LoanApproved)

## Histogram and bell curve for LoanApproved
hist(LoanApproved, main="Loan Approved Distrubution", xlab="LanAprroved Category", 
     ylab = "LoanApproved", prob=TRUE)
curve(dnorm(x, mean=mean(LoanApproved,na.rm=TRUE), sd=sd(LoanApproved, na.rm =TRUE)), add=TRUE )

## Age
ad.test(Age)
lillie.test(Age)
shapiro.test(Age)

## Histogram and bell curve for Age
hist(Age, main="Age Distriution", xlab="Age category", ylab = "Age", prob=TRUE)
curve(dnorm(x, mean=mean(Age, na.rm=TRUE), sd=sd(Age, na.rm=TRUE)), add=TRUE)

## Annual Income
ad.test(AnnualIncome)
lillie.test(AnnualIncome)
shapiro.test(AnnualIncome)

## Histogram and bell curve for Annual Income
hist(AnnualIncome, main="Annual Income Distribution", xlab= "Annual Income Category",
     ylab= "Annual Income", prob=TRUE)
curve(dnorm(x, mean=mean(AnnualIncome, na.rm =TRUE), sd=sd(AnnualIncome, na.rm=TRUE)), add=TRUE)

## Credit Sore
ad.test(CreditScore)
lillie.test(CreditScore)
shapiro.test(CreditScore)

## Histograms and bell curve for CreditScore
hist(CreditScore, main="CreditScore Distribution", xlab = "CreditScore Category",
     ylab="CreditScore", prob=TRUE)
curve(dnorm(x, mean=mean(CreditScore,na.rm = TRUE), sd=sd(CreditScore, na.rm = TRUE)),add=TRUE)

## Employment Status
ad.test(EmploymentStatus)
lillie.test(EmploymentStatus)
shapiro.test(EmploymentStatus)

## Histograms and bell curve for Employment Status
hist(EmploymentStatus, main="EmploymentStatus distribution", xlab="EmploymentStatus Category",
     ylab="EmploymentStatus", prob=TRUE)
curve(dnorm(x, mean=mean(EmploymentStatus, na.rm=TRUE), sd=sd(EmploymentStatus, na.rm = TRUE)),
            add=TRUE)
## Education Level
ad.test(EducationLevel)
lillie.test(EducationLevel)
shapiro.test(EducationLevel)

## Histograms and bell curve for Education Level
hist(EducationLevel, main="EducationLevel distribution", xlab="EducationLevel Category", ylab="EducationLevel", prob=TRUE)
curve(dnorm(x, mean=mean(EducationLevel, na.rm=TRUE), sd=sd(EducationLevel, na.rm = TRUE)), add=TRUE)

## Experience 
ad.test(Experience)
lillie.test(Experience)
shapiro.test(Experience)

## Histograms and bell curve for Experience
hist(Experience, main="Experience distribution", xlab = "Experience Category", ylab="Experience", prob=TRUE)
curve(dnorm(x, mean=mean(Experience, na.rm = TRUE), sd=sd(Experience, na.rm = TRUE)), add=TRUE)

## Loan Amount
ad.test(LoanAmount)
lillie.test(LoanAmount)
shapiro.test(LoanAmount)

## Histograms and bell curve for Loan Amount
hist(LoanAmount, main = "LoanAmount distribution", xlab = "LoanAmount Category", ylab="LoanAmount", prob=TRUE)
curve(dnorm(x, mean=mean(LoanAmount, na.rm = TRUE), sd=sd(LoanAmount,na.rm = TRUE)), add=TRUE)

## Loan Duration
ad.test(LoanDuration)
lillie.test(LoanDuration)
shapiro.test(LoanDuration)

## Histograms and bell curve for Loan Duration
hist(LoanDuration, main = "LoanDuration distribution", xlab = "LoanDuration Category", ylab = "LoanDuration", prob=TRUE)
curve(dnorm(x, mean=mean(LoanDuration, na.rm = TRUE), sd=sd(LoanDuration, na.rm = TRUE)), add=TRUE)

## Marital Status
ad.test(MaritalStatus)
lillie.test(MaritalStatus)
shapiro.test(MaritalStatus)

## Histograms and bell curve for MaritalStatus
hist(MaritalStatus, main = "MaritalStatus distribution", xlab = "MaritalStatus Category", ylab="MaritalStatus", prob=TRUE)
curve(dnorm(x, mean=mean(MaritalStatus,na.rm=TRUE), sd=sd(MaritalStatus, na.rm=TRUE)), add=TRUE)

## NumberOfDependents
ad.test(NumberOfDependents)
lillie.test(NumberOfDependents)
shapiro.test(NumberOfDependents)

## Histograms and bell curve for NumberOfDependents
hist(NumberOfDependents, main="NumberOfDependents distribution", xlab = "NumberOfDependents distribution", ylab="NumberOfDependents",
     prob=TRUE)
curve(dnorm(x, mean=mean(NumberOfDependents, na.rm = TRUE), sd=sd(NumberOfDependents, na.rm = TRUE)), prob=TRUE)

## HomeOwnershipStatus
ad.test(HomeOwnershipStatus)
lillie.test(HomeOwnershipStatus)
shapiro.test(HomeOwnershipStatus)

## Histograms and bell curve for HomeOwnershipStatus
hist(HomeOwnershipStatus, main="HomeOwnershipStatus distribution", xlab="HomeOwnershipStatus Category", ylab="HomeOwnership", prob=TRUE)
curve(dnorm(x, mean=mean(HomeOwnershipStatus, na.rm = TRUE), sd=sd(HomeOwnershipStatus, na.rm = TRUE)), add=TRUE)

## MonthlyDebtPayments
ad.test(MonthlyDebtPayments)
lillie.test(MonthlyDebtPayments)
shapiro.test(MonthlyDebtPayments)

## Histogram and bell curve for MonthlyDebtPayments
hist(MonthlyDebtPayments, main="MonthlyDebtPayments distribution", xlab = "MonthlyDebtPayments Category", ylab="MonthlyDebtPayments", prob=TRUE)
curve(dnorm(x, mean=mean(MonthlyDebtPayments, na.rm=TRUE), sd=sd(MonthlyDebtPayments, na.rm = TRUE)), add=TRUE)

## CreditCardUtilizationRate
ad.test(CreditCardUtilizationRate)
lillie.test(CreditCardUtilizationRate)
shapiro.test(CreditCardUtilizationRate)

## Histogram and bell curve for CreditCardUtilizationRate
hist(CreditCardUtilizationRate, main="CreditCardUtilizationRate distribution", xlab="CreditCardUtilizationRate Category", 
     ylab="CreditCardUtilizationRate", prob=TRUE)
curve(dnorm(x, mean=mean(CreditCardUtilizationRate, na.rm=TRUE), sd=sd(CreditCardUtilizationRate, na.rm=TRUE)), add=TRUE)

## NumberOfOpenCreditLines
ad.test(NumberOfOpenCreditLines)
lillie.test(NumberOfOpenCreditLines)
shapiro.test(NumberOfOpenCreditLines)

## Histogram and bell curve for NumberOpenCreditLines
hist(NumberOfOpenCreditLines, main = "NumberOfCreditLines distribution", xlab="NumberOfOpenCreditLines Category",
     ylab="NumberOfOpenCreditLines", prob=TRUE)
curve(dnorm(x, mean=mean(NumberOfOpenCreditLines, na.rm=TRUE), sd=sd(NumberOfOpenCreditLines, na.rm=TRUE)), add=TRUE)

## NumberOfCreditInquiries
ad.test(NumberOfCreditInquiries)
lillie.test(NumberOfCreditInquiries)
shapiro.test(NumberOfCreditInquiries)

## Histogram and bell curve for NumberOfCreditInquiries
hist(NumberOfCreditInquiries, main = "NumberOfInquiries distribution", xlab="NumberOfInquiries distribution", ylab="NumberOfInquiries", prob=TRUE)
curve(dnorm(x, mean=mean(NumberOfCreditInquiries, na.rm=TRUE), sd=sd(NumberOfCreditInquiries, na.rm = TRUE)), add=TRUE)

## DebtToIncomeRatio

ad.test(DebtToIncomeRatio)
lillie.test(DebtToIncomeRatio)
shapiro.test(DebtToIncomeRatio)

## Histogram and bell curve for DebtToIncomeRatio
hist(DebtToIncomeRatio, main="BebtToIncomeRation distribution", xlab="DebtToIncomeRatio Category", ylab="DebtToIncomeRatio", prob=TRUE)

## Histogram and bell curve for DebtToIncomeRatio
curve(dnorm(x, mean=mean(DebtToIncomeRatio, na.rm = TRUE), sd=sd(DebtToIncomeRatio, na.rm = TRUE)), add=TRUE)


## Bankruptcy History
ad.test(BankruptcyHistory)
lillie.test(BankruptcyHistory)
shapiro.test(BankruptcyHistory)

## Histogram and bell curve for BankruptcyHistory
hist(BankruptcyHistory, main="BankruptcyHistory distribution", xlab="BankruptcyHistory Category", ylab="BankruptcyHistory", prob=TRUE)
curve(dnorm(x, mean=mean(BankruptcyHistory, na.rm = TRUE), sd=sd(BankruptcyHistory, na.rm = TRUE)), add=TRUE)

## LoanPurpose
ad.test(LoanPurpose)
lillie.test(LoanPurpose)
shapiro.test(LoanPurpose)

## Histogram and bell curve for LoanPurpose
hist(LoanPurpose, main="LoanPurpose distribution", xlab="LoanPurpose distribution", ylab="LoanPurpose", prob=TRUE)
curve(dnorm(x, mean=mean(LoanPurpose, na.rm = TRUE), sd=sd(LoanPurpose, na.rm = TRUE)), add=TRUE)

## PreviousLoanDefault
ad.test(PreviousLoanDefaults)
lillie.test(PreviousLoanDefaults)
shapiro.test(PreviousLoanDefaults)

## Histogram and bell curve for PreviousLoanDefault
hist(PreviousLoanDefaults, main = "PreviousLoanDefault distribution", xlab="PreviousLoanDefault Category", ylab="PreviousLoanDefault", prob=TRUE)
curve(dnorm(x, mean=mean(PreviousLoanDefaults, na.rm = TRUE), sd=sd(PreviousLoanDefaults, na.rm = TRUE)), add=TRUE)

## PaymentHistory
ad.test(PaymentHistory)
lillie.test(PaymentHistory)
shapiro.test(PaymentHistory)

## Histogram and bell curve for PaymentHistory
hist(PaymentHistory, main = "PaymentHistory distribution", xlab="PaymentHistory Category", ylab="PaymentHistory", prob=TRUE)
curve(dnorm(x, mean=mean(PaymentHistory, na.rm = TRUE), sd=sd(PaymentHistory, na.rm = TRUE)), add=TRUE)

## LengthOfCreditHistory
ad.test(LengthOfCreditHistory)
lillie.test(LengthOfCreditHistory)
shapiro.test(LengthOfCreditHistory)

## Histogram and bell curve for LengthOfCreditHistory
hist(LengthOfCreditHistory, main="LengthOfCreditHistorydistribution", xlab="LengthOfCreditHistory Category", ylab = "LengthOfCreditHistory", prob=TRUE)
curve(dnorm(x, mean=mean(LengthOfCreditHistory, na.rm = TRUE), sd=sd(LengthOfCreditHistory, na.rm = TRUE)), add=TRUE)

## SavingsAccountBalance
ad.test(SavingsAccountBalance)
lillie.test(SavingsAccountBalance)
shapiro.test(SavingsAccountBalance)

## Histogram and bell curve for SavingsAccountBalance
hist(SavingsAccountBalance, main="SavingsAccountBalance distribution", xlab="SavingsAccountBalance Category", ylab="SavingsAccountBalance", prob=TRUE)
curve(dnorm(x, mean=mean(SavingsAccountBalance, na.rm=TRUE), sd=sd(SavingsAccountBalance, na.rm=TRUE)), add=TRUE)

## CheckingAccountBalance
ad.test(CheckingAccountBalance)
lillie.test(CheckingAccountBalance)
shapiro.test(CheckingAccountBalance)

## Histogram and bell curve for CheckingAccountBalance
hist(CheckingAccountBalance, main="CheckingAccountBalance distribution", xlab="CheckingAccountBalance Category", ylab="CheckingAccountBalance", prob=TRUE)
curve(dnorm(x, mean=mean(CheckingAccountBalance, na.rm=TRUE), sd=sd(CheckingAccountBalance, na.rm=TRUE)), add=TRUE)

## TotalAssets
ad.test(TotalAssets)
lillie.test(TotalAssets)
shapiro.test(TotalAssets)

## Histogram and bell curve for TotalAssets
hist(TotalAssets, main="TotalAssets distribution", xlab="TotalAssets Category", ylab="TotalAssets", prob=TRUE)
curve(dnorm(x, mean=mean(TotalAssets, na.rm=TRUE), sd=sd(TotalAssets, na.rm=TRUE)), add=TRUE)

## TotalLiabilities
ad.test(TotalLiabilities)
lillie.test(TotalLiabilities)
shapiro.test(TotalLiabilities)

## Histogram and bell curve for TotalLiabilities
hist(TotalLiabilities, main = "TotalLiabilities distribution", xlab="TotalLiabilities Category", ylab="TotalLiabilities", prob=TRUE)
curve(dnorm(x, mean=mean(TotalLiabilities, na.rm = TRUE), sd=sd(TotalLiabilities, na.rm=TRUE)), add=TRUE)

## MontlyIncome
ad.test(MonthlyIncome)
lillie.test(MonthlyIncome)
shapiro.test(MonthlyIncome)

## Histogram and bell curve for MonthlyIncome
hist(MonthlyIncome, main="MonthlyIncome distribution", xlab = "MonthlyIncome Category", ylab="MonthlyIncome", prob=TRUE)
curve(dnorm(x, mean=mean(MonthlyIncome, na.rm=TRUE), sd=sd(MonthlyIncome, na.rm = TRUE)), add=TRUE)

## UtilityBillsPaymentHistory

ad.test(UtilityBillsPaymentHistory)
lillie.test(UtilityBillsPaymentHistory)
shapiro.test(UtilityBillsPaymentHistory)

## Histogram and bell curve for UtilityBillsPaymentHistory
hist(UtilityBillsPaymentHistory, main="UtilityBillsPaymentHistory", xlab="UtilityBillsPaymentHistory Category", ylab="UtilityBillsPaymentHistory", prob=TRUE)
curve(dnorm(x, mean=mean(UtilityBillsPaymentHistory, na.rm = TRUE), sd=sd(UtilityBillsPaymentHistory)), add=TRUE)

## JobTenure

ad.test(JobTenure)
lillie.test(JobTenure)
shapiro.test(JobTenure)

## Histogram and bell curve for JobTenture
hist(JobTenure, main="JobTenture distribution", xlab="JobTenture Category", ylab="JobTenture", prob=TRUE)
curve(dnorm(x, mean=mean(JobTenure, na.rm=TRUE), sd=sd(JobTenure, na.rm=TRUE)), add=TRUE)


##  NetWorth

ad.test(NetWorth)
lillie.test(NetWorth)
shapiro.test(NetWorth)

## Histogram and bell curve for Networth
hist(NetWorth, main="Networth distribution", xlab = "Networth Category", ylab="Networth", prob=TRUE)
curve(dnorm(x, mean=mean(NetWorth, na.rm=TRUE), sd=sd(NetWorth, na.rm=TRUE)), add=TRUE)

## BaseInterestRate
ad.test(BaseInterestRate)
lillie.test(BaseInterestRate)
shapiro.test(BaseInterestRate)

## Histogram and bell curve for BaseInterestRate
hist(BaseInterestRate, name="BaseInterestRate distribution", xlab="BaseInterestRate Category", ylab="BaseInterestRate", prob=TRUE)
curve(dnorm(x, mean=mean(BaseInterestRate, na.rm=TRUE), sd=sd(BaseInterestRate, na.rm = TRUE)), add =TRUE)

## InterestRate
ad.test(InterestRate)
lillie.test(InterestRate)
shapiro.test(InterestRate)

## Histogram and bell curve for InterestRate
hist(InterestRate, name="InterestRate distribution", xlab="InterestRate Category", ylab="InterestRate", prob=TRUE)
curve(dnorm(x, mean=mean(InterestRate, na.rm = TRUE),sd=sd(InterestRate, na.rm=TRUE)), add=TRUE)

## MonthlyLoanPayment
ad.test(MonthlyLoanPayment)
lillie.test(MonthlyLoanPayment)
shapiro.test(MonthlyLoanPayment)

## Histogram and bell curve for MonthlyLoanPayment
hist(MonthlyLoanPayment, main="MonthlyLoanPayment distribution", xlab="MonthlyLoanPayment Category", ylab=
       "MonthlyLoanPayment", prob=TRUE)
curve(dnorm(x, mean=mean(MonthlyLoanPayment, na.rm = TRUE), sd=sd(MonthlyLoanPayment, na.rm=TRUE)), add=TRUE)

## TotalDebtToIncomeRatio
ad.test(TotalDebtToIncomeRatio)
lillie.test(TotalDebtToIncomeRatio)
shapiro.test(TotalDebtToIncomeRatio)

## Histogram and bell curve for TotalDebtToIncomeRatio
hist(TotalDebtToIncomeRatio, main = "TotalDebtToIncomeRatio distribution", xlab="TotalDebtToIncomeRatio", ylab="TotalDebtToIncomeRatio", prob=TRUE)
curve(dnorm(x, mean=mean(TotalDebtToIncomeRatio, na.rm = TRUE), sd=sd(TotalDebtToIncomeRatio, na.rm = TRUE)), add=TRUE)


## RiskScore

ad.test(RiskScore)
lillie.test(RiskScore)
shapiro.test(RiskScore)

## Histogram and bell curve for RiskScore
hist(RiskScore, main="RiskScore distribution", xlab="RiskScore Category", ylab="RiskScore",prob=TRUE)
curve(dnorm(x, mean=mean(RiskScore, na.rm = TRUE), sd=sd(RiskScore, na.rm = TRUE)), add=TRUE)

## Install the Rcmdr
install.packages('Rcmdr')

## Call the library
library('Rcmdr')

## Correlational Analysis
cor.test(LoanApproved,Age, method="spearman", alternative="two.sided")

## Scatterplot for LoanApproved vs Age

scatterplot(LoanApproved~Age, regLine=TRUE, smooth=FALSE, boxplots=FALSE, 
           xlab="Age", ylab="LoanApproved", main="LoanApproved vs Age Scatterplot", 
          data=data_new)

cor.test(LoanApproved, AnnualIncome, method = "spearman", alternative="two.sided")

## scatteplot for LoanApproved VS AnnualIncome

scatterplot(LoanApproved~AnnualIncome, regLine=TRUE, smooth=FALSE, 
            boxplots=FALSE, xlab="AnnualIncome", ylab="LoanApproved", 
            main="LoanApproved vs AnnualIncome  Scatterplot", data=data_new)

## Scatterplot for LoanApproved Vs CreditScore
cor.test(LoanApproved, CreditScore, method="spearman", alternative="two.sided")

## scatterplot for LoanApproved Vs CreditScore

scatterplot(LoanApproved~CreditScore, regLine=TRUE, smooth=FALSE, 
            boxplots=FALSE, xlab="CreditScore", ylab="LoanApproved", 
            main="LoanApproved vs CreditScore  Scatterplot", data=data_new)

cor.test(LoanApproved, EmploymentStatus, method="spearman", alternative="two.sided")

## scatterplot for LoanApproved Vs EmploymentStatus

scatterplot(LoanApproved~EmploymentStatus, regLine=TRUE, smooth=FALSE, 
            boxplots=FALSE, xlab="EmploymentStatus", ylab="LoanApproved", 
            main="LoanApproved vs EmploymentStatus  Scatterplot", data=data_new)

cor.test(LoanApproved, EducationLevel, method="spearman", alternative="two.sided")


## scatterplot for LoanApproved Vs EducationLevel

scatterplot(LoanApproved~EducationLevel, regLine=TRUE, smooth=FALSE, 
            boxplots=FALSE, xlab="EducationLevel", ylab="LoanAprroved", 
            main="EducationLevel Vs LoanAprroved Scatterplot", data=data_new)

cor.test(LoanApproved, Experience, method="spearman", alternative="two.sided")

## scatterplot for LoanApproved Vs Experience

scatterplot(LoanApproved~Experience, regLine=TRUE, smooth=FALSE, 
            boxplots=FALSE, xlab="Experience", ylab="LoanApproved", 
            main="LoanApproved Vs Experience Scatterplot", data=data_new)

cor.test(LoanApproved, LoanAmount, method = "spearman", alternative = "two.sided")

## scatterplot for LoanApproved Vs LoanAmount

scatterplot(LoanApproved~LoanAmount, regLine=TRUE, smooth=FALSE, 
            boxplots=FALSE, xlab="LoanAmount", ylab="LoanApproved", 
            main="LoanApproved Vs LoanAmount Scatterplot", data=data_new)

cor.test(LoanApproved, LoanDuration, method="spearman", alternative = "two.sided")

## scatterplot for LoanApproved Vs LoanDuration

scatterplot(LoanApproved~LoanDuration, regLine=TRUE, smooth=FALSE, 
            boxplots=FALSE, xlab="LoanDuration", ylab="LoanApproved", 
            main="LoanApproved Vs LoanDuration Scatterplot", data=data_new)

cor.test(LoanApproved, MaritalStatus, method="spearman", alternative ="two.sided")

## scatterplot for LoanApproved Vs MaritalStatus
scatterplot(LoanApproved~MaritalStatus, regLine=TRUE, smooth=FALSE, 
            boxplots=FALSE, xlab="MaritalStatus", ylab="LoanApproved", 
            main="LoanApproved Vs MaritalStatus Scatterplot", data=data_new)

cor.test(LoanApproved, NumberOfDependents, method="spearman", alternative = "two.sided")

## scatterplot for LoanApproved Vs NumberOfDependents
scatterplot(LoanApproved~NumberOfDependents, regLine=TRUE, smooth=FALSE, 
            boxplots=FALSE, xlab="NumberOfDependents", ylab="LoanApproved", 
            main="LoanApproved Vs NumberOfDependents Scatterplot", data=data_new)

cor.test(LoanApproved,HomeOwnershipStatus, method="spearman", alternative="two.sided")

## scatterplot for LoanApproved Vs HomeOwnershipStatus

scatterplot(LoanApproved~HomeOwnershipStatus, regLine=TRUE, smooth=FALSE, 
            boxplots=FALSE, xlab="HomeOwnershipStatus", ylab="LoanApproved", 
            main="LoanApproved Vs HomeOwnershipStatus Scatterplot", data=data_new)

cor.test(LoanApproved,MonthlyDebtPayments, method="spearman", alternative = "two.sided")

## scatterplot for LoanApproved Vs MonthlyDebtPayments

scatterplot(LoanApproved~MonthlyDebtPayments, regLine=TRUE, smooth=FALSE, 
            boxplots=FALSE, xlab="MonthlyDebtPayments", ylab="LoanApproved", 
            main="LoanApproved Vs MonthlyDebtPayments Scatterplot", data=data_new)

cor.test(LoanApproved, CreditCardUtilizationRate, method="spearman", alternative = "two.sided")

## scatterplot for LoanApproved Vs CreditCardUtilizationRate

scatterplot(LoanApproved~CreditCardUtilizationRate, regLine=TRUE, 
            smooth=FALSE, boxplots=FALSE, xlab="CreditCardUtilizationRate", 
            ylab="LoanApproved", 
            main="LoanApproved Vs CreditCardUtilizationRate Scatterplot", data=data_new)

cor.test(LoanApproved,NumberOfOpenCreditLines, method="spearman", alternative = "two.sided")

## scatterplot for LoanApproved Vs NumberOfOpenCreditLines

scatterplot(LoanApproved~NumberOfOpenCreditLines, regLine=TRUE, 
            smooth=FALSE, boxplots=FALSE, xlab="NumberOfOpenCreditLines", 
            ylab="LoanApproved", main="LoanApproved Vs NumberOfCreditLines Scatterplot",
            data=data_new)

cor.test(LoanApproved, NumberOfCreditInquiries, method="spearman", alternative = "two.sided")

## scatterplot for LoanApproved Vs NumberOfCreditInquiries

scatterplot(LoanApproved~NumberOfCreditInquiries, regLine=TRUE, 
            smooth=FALSE, boxplots=FALSE, xlab="NumberOfCreditInquiries", 
            ylab="LoanApproved", 
            main="LoanApproved Vs NumberOfCreditInquiries Scatterplot", data=data_new)

cor.test(LoanApproved, DebtToIncomeRatio, method="spearman", alternative = "two.sided")

## scatterplot for LoanApproved Vs DebtToIncomeRatio

scatterplot(LoanApproved~DebtToIncomeRatio, regLine=TRUE, smooth=FALSE, 
            boxplots=FALSE, xlab="DebtToIncomeRatio", ylab="LoanApproved", 
            main="LoanApproved Vs DebtToIncomeRatio Scatterplot", data=data_new)

cor.test(LoanApproved,BankruptcyHistory, method="spearman", alternative = "two.sided")

## scatterplot for LoanApproved Vs BankruptcyHistory

scatterplot(LoanApproved~BankruptcyHistory, regLine=TRUE, smooth=FALSE, 
            boxplots=FALSE, xlab="BankruptcyHistory", ylab="LoanApproved", 
            main="LoanApproved Vs BankruptcyHistory Scatterplot", data=data_new)

cor.test(LoanApproved, LoanPurpose, method = "spearman", alternative = "two.sided")

## scatterplot for LoanApproved Vs LoanPurpose

scatterplot(LoanApproved~LoanPurpose, regLine=TRUE, smooth=FALSE, 
            boxplots=FALSE, xlab="LoanPurpose", ylab="LoanApproved", 
            main="LoanApproved Vs LoanPurpose Scatterplot", data=data_new)

cor.test(LoanApproved, PreviousLoanDefaults, method="spearman", alternative = "two.sided")

## scatterplot for LoanApproved Vs PreviousLoanDefaults

scatterplot(LoanApproved~PreviousLoanDefaults, regLine=TRUE, smooth=FALSE, 
            boxplots=FALSE, xlab="PreviousLoanDefaults", ylab="LoanApproved", 
            main="LoanApproved Vs PreviousLoanDefaults Scatterplot", data=data_new)

cor.test(LoanApproved, PaymentHistory, method="spearman", alternative = "two.sided")

## scatterplot for LoanApproved Vs PaymentHistory

scatterplot(LoanApproved~PaymentHistory, regLine=TRUE, smooth=FALSE, 
            boxplots=FALSE, xlab="PaymentHistory", ylab="LoanApproved", 
            main="LoanApproved Vs PaymentHistory Scatterplot", data=data_new)

cor.test(LoanApproved,LengthOfCreditHistory, method="spearman", alternative = "two.sided")

## scatterplot for LoanApproved Vs LengthOfCreditHistory

scatterplot(LoanApproved~LengthOfCreditHistory, regLine=TRUE, smooth=FALSE, 
            boxplots=FALSE, xlab="LengthOfCreditHistory", ylab="LoanApproved", 
            main="LoanApproved Vs LengthOfCreditHistory Scatterplot", data=data_new)

cor.test(LoanApproved,SavingsAccountBalance, method="spearman", alternative = "two.sided")

## scatterplot for LoanApproved Vs SavingsAccountBalance

scatterplot(LoanApproved~SavingsAccountBalance, regLine=TRUE, smooth=FALSE, 
            boxplots=FALSE, xlab="SavingsAccountBalance", ylab="LoanApproved", 
            main="LoanApproved Vs SavingsAccountBalance Scatterplot", data=data_new)

cor.test(LoanApproved, CheckingAccountBalance, method="spearman", alternative = "two.sided")

## scatterplot for LoanApproved Vs CheckingAccountBalance

scatterplot(LoanApproved~CheckingAccountBalance, regLine=TRUE, smooth=FALSE,
            boxplots=FALSE, xlab="CheckingAccountBalance", ylab="LoanApproved", 
            main="LoanApproved Vs CheckingAccountBalance Scatterplot", data=data_new)

cor.test(LoanApproved, TotalAssets, method="spearman", alternative = "two.sided")

## scatterplot for LoanApproved Vs TotalAssets

scatterplot(LoanApproved~TotalAssets, regLine=TRUE, smooth=FALSE, 
            boxplots=FALSE, xlab="Total Assets", ylab="LoanApproved", 
            main="LoanApproved Vs TotalAssets Scatterplot", data=data_new)

cor.test(LoanApproved,TotalLiabilities, method="spearman", alternative = "two.sided")

## scatterplot for LoanApproved Vs TotalLiabilities

scatterplot(LoanApproved~TotalLiabilities, regLine=TRUE, smooth=FALSE, 
            boxplots=FALSE, xlab="TotalLiabilities", ylab="LoanApproved", 
            main="LoanApproved Vs TotalLiabilities Scatterplot", data=data_new)

cor.test(LoanApproved, MonthlyIncome, method = "spearman", alternative="two.sided")

## scatterplot for LoanApproved Vs MonthlyIncome

scatterplot(LoanApproved~MonthlyIncome, regLine=TRUE, smooth=FALSE, 
            boxplots=FALSE, xlab="MonthlyIncome", ylab="LoanApproved", 
            main="LoanApproved Vs MonthlyIncome Scatterplot", data=data_new)

cor.test(LoanApproved, UtilityBillsPaymentHistory, method="spearman", alternative = "two.sided")

## scatterplot for LoanApproved Vs UtilitiyBillsPaymentHistory

scatterplot(LoanApproved~UtilityBillsPaymentHistory, regLine=TRUE, 
            smooth=FALSE, boxplots=FALSE, xlab="UtilityBillsPaymentHistory", 
            ylab="LoanApproved", 
            main="LoanApproved Vs UtilityBillsPaymentHistory Scatterplot", 
            data=data_new)

cor.test(LoanApproved, JobTenure, method="spearman", alternative="two.sided")

## scatterplot for LoanApproved Vs JobTenture

scatterplot(LoanApproved~JobTenure, regLine=TRUE, smooth=FALSE, 
            boxplots=FALSE, xlab="JonTenture", ylab="LoanApproved", 
            main="LoanApproved Vs JonTenture Scatterplot", data=data_new)

cor.test(LoanApproved, NetWorth, method="spearman", alternative = "two.sided")

## scatterplot for LoanApproved Vs Networth

scatterplot(LoanApproved~NetWorth, regLine=TRUE, smooth=FALSE, 
            boxplots=FALSE, xlab="Networth", ylab="LoanApproved", 
            main="LoanApproved Vs Networth Scatterplot", data=data_new)

cor.test(LoanApproved, BaseInterestRate, method="spearman", alternative = "two.sided")

## scatterplot for LoanApproved Vs BaseInterestRate

scatterplot(LoanApproved~BaseInterestRate, regLine=TRUE, smooth=FALSE, 
            boxplots=FALSE, xlab="BaseInterestRate", ylab="LoanApproved", 
            main="LoanApproved Vs BaseInterestRate Scatterplot", data=data_new)

cor.test(LoanApproved, InterestRate, method="spearman", alternative="two.sided")

## scatterplot for LoanApproved Vs InterestRate

scatterplot(LoanApproved~InterestRate, regLine=TRUE, smooth=FALSE, 
            boxplots=FALSE, xlab="InterestRate", ylab="LoanApproved", 
            main="LoanApproved Vs InterestRate Scatterplot", data=data_new)

cor.test(LoanApproved, MonthlyLoanPayment, method="spearman", alternative="two.sided")

## scatterplot for LoanAprroved Vs MonthlyLoadPayment

scatterplot(LoanApproved~MonthlyLoanPayment, regLine=TRUE, smooth=FALSE, 
            boxplots=FALSE, xlab="MonthlyLoanPayment", ylab="LoanApproved", 
            main="LoanApproved Vs MonthlyLoanPayment  Scatterplot", data=data_new)

cor.test(LoanApproved,TotalDebtToIncomeRatio, method="spearman", alternative = "two.sided")

## scatterplot for LoanApproved Vs TotalDebtToIncomeRatio

scatterplot(LoanApproved~TotalDebtToIncomeRatio, regLine=TRUE, smooth=FALSE,
            boxplots=FALSE, xlab="TotalDebtToIncomeRatio", ylab="LoanApproved", 
            main="LoanApproved Vs TotalDebtToIncomeRatio  Scatterplot", data=data_new)

cor.test(LoanApproved, RiskScore, method="spearman", alternative = "two.sided")

## scatterplot for LoanApproved Vs RiskScore

scatterplot(LoanApproved~RiskScore, regLine=TRUE, smooth=FALSE, 
            boxplots=FALSE, xlab="RiskScore", ylab="LoanApproved", 
            main="LoanApproved Vs RiskScore  Scatterplot", data=data_new)


## RandomForest Classifier Model

install.packages('randomForest')

## call the randomForest library

# Load necessary library
library(randomForest)

# Assuming your dataset 'loan_data' has been loaded and the Random Forest model is trained
# and includes the target variable 'ApprovalStatus'

# Ensure the target variable 'ApprovalStatus' is binary and a factor
data_new$LoanApproved <- as.factor(data_new$LoanApproved)

# Build the Random Forest model using the full dataset
rf_model <- randomForest(LoanApproved ~ LoanAmount + MonthlyIncome + InterestRate + 
                           MonthlyLoanPayment + TotalDebtToIncomeRatio + RiskScore, 
                         data=data_new, ntree=100)

## summary statistics
summary(rf_model)

# Now let's predict with sample data

# Create a sample data frame for prediction (without the target column)
sample_data <- data.frame(
  LoanAmount = 9184,        # Example loan amounts
  MonthlyIncome =8605.333,      # Example monthly incomes
  InterestRate = 0.1759902,          # Example interest rates
  MonthlyLoanPayment = 330.1791,    # Example monthly loan payments
  TotalDebtToIncomeRatio =0.0702098,  # Example debt-to-income ratios
  RiskScore = 36.0              # Example risk scores
)

# Make predictions on the sample data
sample_predictions <- predict(rf_model, newdata=sample_data)
if (sample_predictions[1]==1) {
  print("1")
} else {
  print("0")
}



