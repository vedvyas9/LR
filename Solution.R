install.packages("lsr")
library(lsr)


#loading the data
data<-read.csv("loans.csv")

#dimension of the data
dim(data)

#short summary of all columns
str(data)

#we do not require ID column
data=data[,-1]
dim(data)

#top 5 rows to get an idea of the data
head(data)

#we can see % symbol in Interest.Rate column and Debt.To.Income.Ratio column
#we remove the % symbol so that the computer can interpret the data better
data$Interest.Rate<-gsub("%","",data$Interest.Rate)
data$Debt.To.Income.Ratio<-gsub("%","",data$Debt.To.Income.Ratio)
#to confirm the removal of % symbol
head(data)

#summary of datatypes
sapply(data[1,], class)

#converting Interest.rate and Debt.To.Income to numeric
data$Interest.Rate<-as.numeric(data$Interest.Rate)
data$Debt.To.Income.Ratio<-as.numeric(data$Debt.To.Income.Ratio)

#check if converted
sapply(data[1,],class)

