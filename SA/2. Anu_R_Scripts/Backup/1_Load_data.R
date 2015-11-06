#--- Library
library("plyr")
library("sqldf")

#--- System notifications


Retailer_Info <- read.delim(file="/home/mono/Documents/Assortment/Top 25/Data/Inputs/Retailer_Info.csv",sep = ",",header = T)
for(i in c(1:ncol(Retailer_Info))){
  Retailer_Info[,i] <- as.character(Retailer_Info[,i])
}

#--- Read all the retailer review files mention in the Retailer_Info
for(retailerIndex in c(1:nrow(Retailer_Info))){  # retailerIndex <- 1
  #--- System notifications
  sysCommand <- paste('echo "', (15/nrow(Retailer_Info)),'"; echo "# Reading file ', Retailer_Info[retailerIndex,1],'..."',sep="",collapse = "")
  system(sysCommand)
  
  #--- Read reviews into tempDf
  tempDf <- read.delim(file=Retailer_Info$File_Path[retailerIndex],sep = ",",header = T)
  
  #--- Remove unwanted columns
  tempDf <- tempDf[,c(1:32)]
  
  #--- Save colnames of the 1st file which will be applied to the remaining datasets
  if(retailerIndex==1){
    usedColNames <- colnames(tempDf)
  } else{
    colnames(tempDf) <- usedColNames
  }
  
  #--- Format Data
  for(i in c(1:(ncol(tempDf)))){
    if(class(tempDf[,i]) == "factor"){
      tempDf[,i] <- as.character(tempDf[,i])
    }
    tempDf[grepl(x = tempDf[,i],pattern = "n/a"),i] <- NA
  }
  tempDf$Review.Creation_Date <- as.Date(tempDf$Review.Creation_Date, format=Retailer_Info$Date_Format[retailerIndex])
  tempDf$Retailer <- tolower(gsub(x = tempDf$Retailer, pattern = "\\s", replacement = ""))

  
  #--- Merge all data in one df
  if(retailerIndex==1){
    All_Review <- tempDf
  } else{
    All_Review <- rbind(All_Review, tempDf)
  }
}
Retailer_Info <- 1
#--- Remove duplicates
# System command
system('echo "# Removing duplicates..."; echo "15"')
#---------
All_Review <- All_Review[!duplicated(All_Review[c("Retailer","Unique_Product_Identifier_Mainproduct","Review.Header","Review.Creation_Date","Review.By")]),]

length(All_Review$Unique_Product_Identifier_Mainproduct[All_Review$Unique_Product_Identifier_Mainproduct=="B00KD5SEPK"])
temp <- count(All_Review, c("Unique_Product_Identifier_Mainproduct"))
temp <- sqldf("SELECT a.*,b.Product_reviews FROM temp AS a INNER JOIN All_Review AS b ON a.Unique_Product_Identifier_Mainproduct=b.Unique_Product_Identifier_Mainproduct")
temp <- unique(temp)
temp$Product_reviews <- gsub(x = temp$Product_reviews,pattern = ",",replacement = "")
temp$Product_reviews <- as.numeric(as.character(temp$Product_reviews))
temp$ratio <- temp$freq / temp$Product_reviews

#--- Remove rows which have Review.Creation_Date & Review.Rating_Score as NAs
colIndex <- grep(x=colnames(All_Review),pattern = "Review.Creation_Date",ignore.case = T)
All_Review <- All_Review[complete.cases(All_Review[,colIndex]),]
colIndex <- grep(x=colnames(All_Review),pattern = "Review.Rating_Score",ignore.case = T)
All_Review <- All_Review[complete.cases(All_Review[,colIndex]),]
rm(colIndex)

#--- Save data
# System command
system('echo "# Saving data..."; echo "17"')
#---------
save(All_Review,file="/home/mono/Documents/Assortment/Top 25/Data/ProcessedData/1_All_Review_Processed.RData")
#write.csv(All_Review, file="/home/mono/Documents/Assortment/Top 25/Data/ProcessedData/1_All_Review_Processed.csv", row.names=F)

rm(list=ls())
