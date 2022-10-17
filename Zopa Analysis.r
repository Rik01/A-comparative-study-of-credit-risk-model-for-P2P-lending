title: "A Comparative Analysis Of Credit Rating Model Used In Peer to Peer Lending"
author: "Rik Choudhury"
date: "January 23, 2021"
#Zopa loan book analysis

library(ggplot2)
library(tidyr)
library(dplyr)
library(ggthemes)
library(gridExtra)
library(RColorBrewer)
library(knitr)
library(GGally)
library(psych)
library('corrplot')
library(tidyverse)
library(scales)
library(RColorBrewer)
library(tidyverse)
library(grid)
library(ggthemes)
library(pander)
library(lubridate)
library(readxl)
Zopa_Loan_book <- read_excel("/Volumes/MacSD/Documents/Thesis/Credit Rating Modeling/Loan Book/Zopa Loan book.xlsm")
View(Zopa_Loan_book)



Zopa_Loan_book<-na.omit(Zopa_Loan_book)

test=as.character(c(1,2,3,4,'M'))
v=as.numeric(test)
ix.na=is.na(v)
which(ix.na)
test[ix.na]

ix.na=is.na(Zopa_Loan_book$PostCode)
which(ix.na)
dim(Zopa_Loan_book)
class(Zopa_Loan_book$PostCode)
Zopa_Loan_book$PostCode[is.na(Zopa_Loan_book$PostCode)]<-00
Zopa_Loan_book$PostCode=as.numeric(Zopa_Loan_book$PostCode)


str(Zopa_Loan_book)
summary(Zopa_Loan_book)
head(Zopa_Loan_book$`Date of Default`)
!is.na(Zopa_Loan_book$`Date of Default`)
class(Zopa_Loan_book$PostCode)
ix.na = is.na(Zopa_Loan_book$PostCode)
which(ix.na)
Zopa_Loan_book[ix.na]
Zopa_Loan_book$PostCode<-as.numeric(Zopa_Loan_book$PostCode)
glimpse(Zopa_Loan_book)
loan <- Zopa_Loan_book
dim(loan)
table(loan$`Latest Status`)
loan$`Latest Status` <- as.character(loan$`Latest Status`)
Completed <- c("Completed")
Late <- c("Late")
Default <- c("Default")
loan$`Latest Status`[loan$`Latest Status` %in% Completed] <- "Completed"
loan$`Latest Status`[loan$`Latest Status` %in% Late] <- "Late"
loan$`Latest Status`[loan$`Latest Status` %in% Default] <- "Default"
loan$`Latest Status` <- factor(loan$`Latest Status`, 
                          levels = c("Completed",  "Active", 
                                     "Late", "Default"), 
                          ordered = T)
table(loan$`Latest Status`)

# plot LoanStatus bar chart(Fig10)
ggplot(aes(x= `Latest Status`, y = ..count../sum(..count..)), data = loan) + 
  geom_bar(color="blue", fill="violet") +
  geom_text(stat='count', 
            aes(label= paste(round(..count../sum(..count..)*100, 2),"%"))
            , vjust=-0.5)+ggtitle("Distribution of loans")+
  ylab("Percentage of loans")+xlab("Loan Status")
# plot Terms bar chart(Fig11)
ggplot(aes(x= Term, y = ..count../sum(..count..)), data = loan) + 
  geom_bar(color="yellow", fill="green") +
  geom_text(stat='count', 
            aes(label= paste(round(..count../sum(..count..)*100, 2),"%"))
            , vjust=-0.5)+ggtitle("Distribution of terms")+
  ylab("Percentage of no. of terms")+xlab("No. of Terms")+
  theme(plot.title = element_text(hjust = 0.5),axis.text.x = element_text(angle = 290,hjust =0))
#post code summary
sort(table(loan$PostCode), decreasing=TRUE)[1:20]
# create CompletedOrRisk column
loan$CompletedOrRisk<- ifelse(
  loan$`Latest Status` %in% c("Default","Late"), 
  "HighRisk", as.character(loan$`Latest Status`))
# subset the data only contain Completed and HighRisk loan
sub_loan<- subset(loan, CompletedOrRisk!= "Active")
dim(sub_loan)
table(sub_loan$`Latest Status`)
# plot bar chart for CompletedOrRisk(Fig12)
ggplot(aes(x= CompletedOrRisk), data = sub_loan) + 
  geom_bar(color="blue", fill="light blue") +
  geom_text(stat='count', aes(label=..count..), vjust=-0.5)+ggtitle("Distribution of loans")+
  ylab("Total number")+xlab("Completed or Risky")
# plot proportion for each loan status(Fig13)
ggplot(aes(x= CompletedOrRisk, y = ..count../sum(..count..)), data = sub_loan) + 
  geom_bar(color="blue", fill="light blue") +
  geom_text(stat='count', 
            aes(label= paste(round(..count../sum(..count..)*100, 2),"%")), 
            vjust=-0.5) + ggtitle("Proportion for each loan status")
  labs(x = "Loan Status", y = "Percentage")
 # plot proportion for each loan status(Fig14)
ggplot(aes(x= `Latest Status`, y = ..count../sum(..count..)), data = loan) + 
    geom_bar(color="blue", fill="light blue") +
    geom_text(stat='count', 
              aes(label= paste(round(..count../sum(..count..)*100, 2),"%")), 
              vjust=-0.5) + ggtitle("Proportion for each loan status")+
  labs(x = "Loan Status", y = "Percentage")
# make LendingRate summary table
loan$`Lending rate`<-as.numeric(loan$`Lending rate`)
summary(loan$`Lending rate`)
describe(loan$`Lending rate`)
sort(table(loan$`Lending rate`), decreasing=TRUE)[1:5]

# plot LendingRate histogram(Fig15)
ggplot(aes(x = `Lending rate`), data = loan) +
  geom_histogram(stat="count",binwidth = 0.0005)+ggtitle("Distribution of Lending Rates")+
  labs(x = "Lending Rate")
sub_loan$`Lending rate`<-as.numeric(sub_loan$`Lending rate`)
describe(sub_loan$`Lending rate`)
sort(table(sub_loan$`Lending rate`), decreasing=TRUE)[1:5]
# plot plot LendingRate histogram and facet with loan status(Fig16)
ggplot(aes(x = `Lending rate`), data = sub_loan) + 
  geom_histogram(stat="count",binwidth = 0.005) +
  facet_wrap(~CompletedOrRisk, ncol = 2)+ggtitle("Distribution of Lending Rates for Completed or Risky loans")+
  labs(x = "Lending Rate")
glimpse(sub_loan)
# create a summary table of CompletedOrRisk column clasified by it's value
by(sub_loan$`Lending rate`, sub_loan$CompletedOrRisk, summary)
sub_loan$`Original Loan Amount`<-as.numeric(sub_loan$`Original Loan Amount`)
# creating summary table for Original Loan Amount
by(loan$`Original Loan Amount`, loan$CompletedOrRisk, summary)
describe(loan$`Original Loan Amount`)
sort(table(loan$`Original Loan Amount`), decreasing=TRUE)[1:5]



# creating summary table for Original Loan Amount for CompleteOrRisk
by(sub_loan$`Original Loan Amount`, sub_loan$CompletedOrRisk, summary)
describe(sub_loan$`Original Loan Amount`)
sort(table(sub_loan$`Original Loan Amount`), decreasing=TRUE)[1:5]
# creating summary table for Principal Collected for CompleteOrRisk
sub_loan$`Principal Collected`<-as.numeric(sub_loan$`Principal Collected`)
by(sub_loan$`Principal Collected`, sub_loan$CompletedOrRisk, summary)
sort(table(sub_loan$`Principal Collected`), decreasing=TRUE)[1:5]
table(sub_loan$`Principal Collected`)
# creating summary table for Interest Collected for CompleteOrRisk
table(sub_loan$`Interest Collected`)
sub_loan$`Interest Collected`<-as.numeric(sub_loan$`Interest Collected`)
by(sub_loan$`Interest Collected`, sub_loan$CompletedOrRisk, summary)
sort(table(sub_loan$`Interest Collected`), decreasing=TRUE)[1:5]
# creating summary table for Total number of payments for CompleteOrRisk
sub_loan$`Total number of payments`<-as.numeric(sub_loan$`Total number of payments`)
by(sub_loan$`Total number of payments`, sub_loan$CompletedOrRisk, summary)
sort(table(sub_loan$`Total number of payments`), decreasing=TRUE)[1:5]
summary(loan$`Total number of payments`)
# finding the most frequent interest rate and it's frequency
sort(table(loan$`Lending rate`), decreasing=TRUE)[1:5]
# create a year column
loan$`Disbursal date` <- as.Date(loan$`Disbursal date`)
# transfer it to numeric
loan$year <- as.numeric(format(loan$`Disbursal date`, format = "%Y"))
sub_loan$year <- as.numeric(format(sub_loan$`Disbursal date`, format = "%Y"))
# make year column as factor
loan$year <- factor(loan$year)
sub_loan$year <- factor(sub_loan$year)
table(loan$year)
by(sub_loan$`Lending rate`, sub_loan$year, summary)
# plot LendingRate by year(Fig7)
ggplot(aes(x = `Lending rate`), data = loan) +
  geom_bar(color="blue",binwidth = 0.005) +
  facet_wrap(~year) + ggtitle("Lending Rate by year")+
  labs(x = "Lending rate")
# plot LendingRate by year category sub_loan(Fig8)
ggplot(aes(x = `Lending rate`), data = sub_loan) +
  geom_bar(color="green",binwidth = 0.005) +
  facet_wrap(~year) + ggtitle("Lending Rate by year for CompleteOrRisk loans")+
  labs(x = "Lending rate")
#Date of Default summary
loan$`Date of Default` <- as.Date(loan$`Date of Default`)
table(loan$`Date of Default`)

sub_loan$`Date of Default`<- sapply(sub_loan$`Date of Default`,function(x){x=1})
loan1<-loan%>%select(`Lending rate`,`Date of Default`)
zopa_all$Date.of.Default <- sapply(zopa_all$Date.of.Default,function(x){if (x != ""){x=1} else if (x == ""){x=0}})

#Lending rate with trendline(Fig9)
ggplot(loan, aes(x=`Lending rate`)) +
  geom_histogram(stat="count", binwidth=.005, position="dodge") +
  geom_density(alpha=.3)
#Lending rate of sub_loan with trendline(Fig10)
ggplot(sub_loan, aes(x=`Lending rate`)) +
  geom_histogram(stat="count",binwidth=.005, position="dodge") +
  geom_density(alpha=.3)
describe(loan)
describe(sub_loan)

# make Original loan amount frequency table
summary(loan$`Original Loan Amount`)
# plot Original Loan Amount histogram(Fig11)
ggplot(aes(x = `Original Loan Amount`), data = loan) +
  geom_histogram(stat="count", color = "blue")+ ggtitle("Loan Amount")+
  labs(x = "Loan amount")
# plot Original Loan Amount for Completed or Risk histogram(Fig12)
ggplot(aes(x = `Original Loan Amount`), data = sub_loan) +
  geom_histogram(stat="count", color = "green")+ ggtitle("Loan Amount for Completed or Risky loans")+
  labs(x = "Loan amount")
# make PrincipalCollected frequency table
summary(loan$`Principal Collected`)
# plot PrincipalCollected histogram(Fig13)
ggplot(aes(x = `Principal Collected`), data = loan) +
  geom_histogram(stat="count", color = "blue")+ ggtitle("Principal Collected")+
  labs(x = "Principal amount")
# plot PrincipalCollected for Completed or Risk histogram(Fig14)
ggplot(aes(x = `Principal Collected`), data = sub_loan) +
  geom_histogram(stat="count", color = "green")+ ggtitle("Principal Collected for Completed or Risky loans")+
  labs(x = "Principal amount")

# make InterestCollected frequency table
summary(loan$`Interest Collected`)
# plot Interest Collected histogram(Fig15)
ggplot(aes(x = `Interest Collected`), data = loan) +
  geom_histogram(stat="count", color = "blue")+ ggtitle("Interest Collected")+
  labs(x = "Interest amount")
# plot Interest Collected for Completed or Risk histogram(Fig16)
ggplot(aes(x = `Interest Collected`), data = sub_loan) +
  geom_histogram(stat="count", color = "green")+ ggtitle("Interest Collected for Completed or Risky loans")+
  labs(x = "Interest amount")

# make Total no. of payments frequency table
summary(loan$`Total number of payments`)
# plot Interest Collected histogram(Fig17)
ggplot(aes(x = `Total number of payments`), data = loan) +
  geom_histogram(stat="count", color = "blue")+ ggtitle("Total number of payments")+
  labs(x = "`Total number of payments`")
# plot Interest Collected for Completed or Risk histogram(Fig18)
ggplot(aes(x = `Total number of payments`), data = sub_loan) +
  geom_histogram(stat="count", color = "green")+ ggtitle("Total number of payments for Completed or Risky loans")+
  labs(x = "Total number of payments")


ds$`Lending rate` <- as.numeric(ds$`Lending rate`)
ds$`Original Loan Amount` <- as.numeric(ds$`Original Loan Amount`)
ds$`Principal Collected` <- as.numeric(ds$`Principal Collected`)
ds$`Interest Collected` <- as.numeric(ds$`Interest Collected`)



#Logistic Regression Model


glimpse(sub_loan)
sub_loan$Status <- ifelse(
  sub_loan$CompletedOrRisk == "HighRisk", 1, 0)
ds <- sub_loan[, c("Status", "Lending rate","Original Loan Amount", "Principal Collected", "Interest Collected")]
sapply(ds, function(x) sum(is.na(x)))
model <- glm(Status ~.,family=binomial(link='logit'),data=ds)
summary(model)
print(model)



coefs <- coef(model)
exp(coefs)
(exp(coefs)-1)*100



ds$`Lending rate` <- as.numeric(ds$`Lending rate`)
d$`Original Loan Amount` <- as.numeric(d$`Original Loan Amount`)
d$`Principal Collected` <- as.numeric(d$`Principal Collected`)
d$`Interest Collected` <- as.numeric(d$`Interest Collected`)
d$`Total number of payments` <- as.numeric(d$`Total number of payments`)
ds$`Lending rate` <- ifelse(
  is.na(ds$`Lending rate`), 0, ds$`Lending rate`)
ds$`Original Loan Amount` <- ifelse(
  is.na(ds$`Original Loan Amount`), 0, ds$`Original Loan Amount`)
ds$`Principal Collected` <- ifelse(
  is.na(ds$`Principal Collected`), 0, ds$`Principal Collected`)
ds$`Interest Collected` <- ifelse(
  is.na(ds$`Principal Collected`), 0, ds$`Interest Collected`)
ds$`Principal Collected` <- ifelse(
  is.na(ds$`Principal Collected`), 0, ds$`Principal Collected`)





#Correlation Matrix
sub_loan$`Original Loan Amount`<-as.numeric(sub_loan$`Original Loan Amount`)
sub_loan$`Principal Collected`<-as.numeric(sub_loan$`Principal Collected`)
sub_loan$`Interest Collected`<-as.numeric(sub_loan$`Interest Collected`)
sub_loan$`Lending rate`<-as.numeric(sub_loan$`Lending rate`)
sub_loan$`Principal Collected`<-as.numeric(sub_loan$`Principal Collected`)
sub_loan$`Interest Collected`<-as.numeric(sub_loan$`Interest Collected`)
#Corr with loan(Fig19)
corr_loan = cor(loan[,c(5,6,7,8,10,11)])
plot(corr_loan)
corrplot(corr_loan, type="upper", order="hclust",col = brewer.pal(n=8,name = "RdYlBu"))
#Corr with sub_loan(Fig20)
corr_sub_loan = cor(sub_loan[,c(5,6,7,8,10,11)])
corrplot(corr_sub_loan, type="upper", order="hclust",col = brewer.pal(n=8,name = "RdYlBu"))
corrplot(corr_sub_loan, method="number")
glimpse(sub_loan)





sub_loan_new<-subset(sub_loan,select = c(11,13))
sub_loan_new<-subset(sub_loan_new,subset =sub_loan_new$`Date of Default`!="NA" )  
sub_loan_new$`Date of Default`<-sapply(sub_loan_new$`Date of Default`,function(x){x=1})  
sub_loan1<-subset(loan,select = c(11,13))  
#Log of lending rate vs frequency of default.(Fig21)
hist(log(sub_loan_new$`Lending rate`,2), xlab = "Log_2(Lending Rate)", ylab = "Frequency of Default", main = "Logarithm of Frequency of Defaults per Lending Rate")

#Mean disbursal(Amount vs Date of Disbursal)
zopa_date_amount <- subset(loan,select = c(5,4))
zopa_date_amount <- zopa_date_amount[order(zopa_date_amount$`Disbursal date`),] 
zopa_date_amount$`Disbursal date`<-lubridate::ymd(zopa_date_amount$`Disbursal date`)

zopa_date_amount$month <- lubridate::floor_date(zopa_date_amount$`Disbursal date`,"month")


zopa_date_amount$`Original Loan Amount`<-as.numeric(zopa_date_amount$`Original Loan Amount`)
table(zopa_date_amount$`Original Loan Amount`)
zopa_aggregate <- aggregate(`Original Loan Amount`~ month, zopa_date_amount,mean)
names(zopa_aggregate) <- c("DisbursalDate", "OriginalLoanAmount")


#plotting mean disbursal value(Fig22)
ggplot(data = zopa_aggregate, aes(x=DisbursalDate, y = OriginalLoanAmount)) +
  geom_line(color="blue") +
  geom_point(color="red") +
  expand_limits(y=0) +
  scale_x_date(expand = c(0,200),breaks = pretty(zopa_aggregate$`Disbursal date`, n = 10)) + scale_y_continuous(expand=c(0,0),limits = c(0,8000)) +
  xlab("Date of Disbursal") + ylab("Disbursal Amount")+
  ggtitle("Mean disbursal value per month") +
  theme(plot.title = element_text(hjust = 0.5),axis.text.x = element_text(angle = 290,hjust =0))
#plotting mean disbursal value(smooth)(Fig23)
ggplot(data = zopa_aggregate, aes(x=DisbursalDate, y = OriginalLoanAmount)) + 
  stat_smooth(method = "lm", formula = y ~ poly(x, 9), size = 1,se=FALSE)+
  expand_limits(y=0) +
  scale_x_date(expand = c(0,200),breaks = pretty(zopa_aggregate$DisbursalDate, n = 10)) + scale_y_continuous(expand=c(0,0),limits = c(0,8000)) +
  xlab("Date of Disbursal") + ylab("Disbursal Amount")+
  ggtitle("Mean disbursal value per month") +
  theme(plot.title = element_text(hjust = 0.5),axis.text.x = element_text(angle = 290,hjust =0))



