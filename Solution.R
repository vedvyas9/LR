install.packages("lsr")
install.packages("stringr")
library(lsr)
library(stringr)


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


#as from 2500 rows there are maximum 4 missing values in a single column we can use mean to fill them
