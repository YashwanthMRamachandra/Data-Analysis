#clearing current R workspace

rm(list=ls())

#setting R working directory and path for files----

setwd("/media/sf_mita_ubuntu/Sears/Cookware/Data/")
myPath<-"/media/sf_mita_ubuntu/Sears/Cookware/Data/"


source(file = "AllSourceCode.R")

#please clear RetailerInfo.csv file in Input folder

fn1_SetDateFormat()

readline(prompt = "fill the date format inRetailer info file, press enter when done:")
#fill retailer info
fn2_readAllData()

Review_Data<- All_Review
#All_Review<-Review_Data

#checking retailers
table(All_Review$Retailer)
#changing retailer names if required
All_Review$Retailer[All_Review$Retailer==unique(All_Review$Retailer)[5]]<-"dickssportinggoods"

#removing all records containg NAs
fn3_removeNAs()


#create duplicate check file----
fn4_dupChk()

# take max review date as 12th month period end----
#NotDSG_MMS <- All_Review[All_Review$Retailer!="dickssportinggoods",]
#max(NotDSG_MMS$Review.Creation_Date)
Epoch_Date <- as.Date(max(All_Review$Review.Creation_Date))
write.csv(All_Review,"All_Review2.csv")
# to check which retailer is having wrong dates
sapply(unique(All_Review$Retailer),function(x){print(as.Date(max(All_Review$Review.Creation_Date[All_Review$Retailer==x])))})

#check number of reviews for which dates are wrong
sum(All_Review$Review.Creation_Date>Sys.Date())

#if dates are wrong for very few reviews
All_Review<-All_Review[!All_Review$Review.Creation_Date>Sys.Date(),]
Epoch_Date <- Sys.Date()

#running function to get only 1yr review----
fn5_1yrReview()

#calculating retailer Normalization----
fn6_retailerNorm()

#calculating review score----
fn7_calcRevScore()
