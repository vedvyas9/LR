
install.packages("stringr")
install.packages("tidyverse")

library(stringr)
library(tidyverse)
#Descriptive Stats
#loading the data and making '.' as NA
data<-read.csv("loans.csv",na.strings=".")
View(data)


#dimension of the data
dim(data)

#short summary of all columns
str(data)


#top 5 rows to get an idea of the data
head(data)

#we do not require ID column
data=data[,-1]
dim(data)


#converting Amount.Requested to numeric 
data$Amount.Requested<-as.numeric(data$Amount.Requested)

#Amount funded by investors will have no effect in interest rates (from Domain Knowledge)
data<-data[-2]

#FICO.Range is not effective. Hence, we replace all the enties with the mean of the range of that entry.
#The advantage of this method is that we can easily combine multiple strings into a matrix or a data frame.
SplitFICO <- data.frame(str_split_fixed(data$FICO.Range, "-", 2))
SplitFICO$X1 <- as.numeric(as.character(SplitFICO$X1))
SplitFICO$X2 <- as.numeric(as.character(SplitFICO$X2))
FICO.Mean <- rowMeans(SplitFICO)
data<- data.frame(data, FICO.Mean)

#now we can drop FICO.Range
data<-data[-9]

#we can see '%' symbol in Interest.Rate column and Debt.To.Income.Ratio column
#we remove the '%' symbol so that the computer can interpret the data better
data$Interest.Rate<-gsub("%","",data$Interest.Rate)
data$Interest.Rate<-as.numeric(data$Interest.Rate)
data$Debt.To.Income.Ratio<-gsub("%","",data$Debt.To.Income.Ratio)
data$Debt.To.Income.Ratio<-as.numeric(data$Debt.To.Income.Ratio)

#to confirm the removal of % symbol
head(data)

#removal of whitespace and 'months' word from Loan.Length
data$Loan.Length<-gsub("months","",data$Loan.Length)
data$Loan.Length<-trimws(data$Loan.Length,which=c("both"))
data$Loan.Length<-as.numeric(data$Loan.Length)

#we can see 'year','years','<','+' in the column Employment.Length which is making it a factor datatype column
#we remove those symbols and strings and convert it to numeric
data$Employment.Length<-gsub("years|year","", data$Employment.Length)
data$Employment.Length<-gsub("< ","", data$Employment.Length)


#'<-'function can't handle missing values in its extraction indices hence we need to add '!is.na()'
data[data[,12]=="10+ " & !is.na(data$Employment.Length),12 ]<-10

#as <1 year is 0 years
data[data[,12]=="1 " & !is.na(data$Employment.Length),12 ]<-0
data$Employment.Length

#we see that the column is chr and not numeric even after removing the symbols. It is because of whitespace and '.'
data$Employment.Length<-trimws(data$Employment.Length,which=c("both"))

#converting Employment.Length to numeric
data$Employment.Length<-as.numeric(data$Employment.Length)

#we can clearly see that the number of missing values have increased to 80 in this column due to the removal of '.'
sum(is.na(data))
sapply(data, function(x) sum(is.na(x)))

#summary of datatypes
sapply(data[1,], class)

#converting Interest.rate and Debt.To.Income to numeric
data$Interest.Rate<-as.numeric(data$Interest.Rate)
data$Debt.To.Income.Ratio<-as.numeric(data$Debt.To.Income.Ratio)

#converting Monthly.Income to numeric
data$Monthly.Income<-as.numeric(data$Monthly.Income)

#converting Open.Credit.Lines, Revolving.CREDIT.Balance,Inquiries.in.the.last.6.Months to numeric
data$Open.CREDIT.Lines<-as.numeric(data$Open.CREDIT.Lines)
data$Revolving.CREDIT.Balance<-as.numeric(data$Revolving.CREDIT.Balance)
data$Inquiries.in.the.Last.6.Months<-as.numeric(data$Inquiries.in.the.Last.6.Months)

#check if converted
sapply(data[1,],class)

#check for missing values
sum(is.na(data))
sapply(data, function(x) sum(is.na(x)))

#checking the datatypes again
str(data)

#number of missing values
sum(is.na(data))
sapply(data, function(x) sum(is.na(x)))

#filling missing values with mdian for numerical columns as the number of missing values is very less
#'na.rm=TRUE' indicates: calculate the mean of non missing values
data$Amount.Requested[is.na(data$Amount.Requested)]<-mean(data$Amount.Requested,na.rm=TRUE)
data$Debt.To.Income.Ratio[is.na(data$Debt.To.Income.Ratio)]<-mean(data$Debt.To.Income.Ratio,na.rm=TRUE)
data$Loan.Length[is.na(data$Loan.Length)]<-mean(data$Loan.Length,na.rm=TRUE)
data$Open.CREDIT.Lines[is.na(data$Open.CREDIT.Lines)]<-mean(data$Open.CREDIT.Lines,na.rm=TRUE)
data$Revolving.CREDIT.Balance[is.na(data$Revolving.CREDIT.Balance)]<-mean(data$Revolving.CREDIT.Balance,na.rm=TRUE)

#filling the missing value of state by the most occuring
table(data$State)
#we can see that NY is the most occuring
data$State[is.na(data$State)]<-"NY"

#Employment.length ha 80 missing values.
#First we will fill with mean in a decoy column and check sd before and after
sd(data$Employment.Length,na.rm=TRUE)
x=data$Employment.Length
x[is.na(x)]<-mean(data$Employment.Length,na.rm=TRUE)
sd(x)

#we see that there is not much difference in the sd, hence we can apply this to the main dataset
data$Employment.Length[is.na(data$Employment.Length)]<-mean(data$Employment.Length,na.rm=TRUE)

#check whether number of missing are zero or not
sum(is.na(data))


#Exploratory Analysis

#checking the correlations for numeric columns with interest rate 
cor(data[, sapply(data, class) != "factor"])

#we can see that FICO.mean and Amount.Requested have a significant negative correlation and Loan.Length has a significant positive Correlation

#now we will see the correlation with factor variables through boxplots
par(mfrow=c(2,2))
plot(data$Interest.Rate~data$State)
plot(data$Interest.Rate~data$Home.Ownership)
plot(data$Interest.Rate~data$Loan.Purpose)

#we can see that in all plots , the means all groups vary significantly and hence won't add much value to our analysis

#Therefore we take FICO.mean, Amount.Requested and Loan.Length to apply to our model
#After reading about FICO we come to know that the other factors are included in FICO and hence gives us another reason to exclude them

#Multilinear Regression
model<-lm(data$Interest.Rate~data$FICO.Mean+data$Amount.Requested+data$Loan.Length)

#Summary of the model
summary(model)

#To see which predictor variables are significant, we can examine the coefficients table,
summary(model)$coefficient

#The confidence interval of the model coefficient
confint(model)

#Multiple R-squared: 0.6896


#Conclusion
#The first step in interpreting the multiple regression analysis is to examine the F-statistic and the associated p-value, at the bottom of model summary.
#In our example, it can be seen that p-value of the F-statistic is <2e-16, which is highly significant. This means that, at least, one of the predictor variables is significantly related to the outcome variable.
#For a given the predictor, the t-statistic evaluates whether or not there is significant association between the predictor and the outcome variable, that is whether the beta coefficient of the predictor is significantly different from zero.
#It can be seen that, FICO.Mean and Loan.Length are significantly effecting Interest.Rate while changes in Amount.Requested is not significantly associated with Interest.Rate.
#We found that Amount.Requested is not significant in the multiple regression model. This means that, for a fixed amount of FICO.mean and Loan.Length, changes in Amount.Requested will not significantly affect Interest.Rate.

#finally we can write our model equation as:Interest.Rate=65.97-0.08516*FICO.mean-0.0006*Amount.Requested+0.181*Loan.Length

#model without Amount.Requested
model1<-lm(data$Interest.Rate~data$FICO.Mean+data$Loan.Length)
summary(model1)
summary(model1)$coefficient
confint(model1)

#Multiple R-squared: 0.6894

#finally we can write our model equation as:Interest.Rate=65.87-0.08519*FICO.mean+0.18*Loan.Length

