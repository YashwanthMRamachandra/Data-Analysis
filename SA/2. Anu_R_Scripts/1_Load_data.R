#--- Library
library("plyr")
library("sqldf")

#--- System notifications


Retailer_Info <- read.delim(file=paste(myPath,"Inputs/Retailer_Info.csv",sep=""),sep = ",",header = T)
Retailer_Info <- Retailer_Info[,c(1,2,3)]
for(i in c(1:ncol(Retailer_Info))){
  Retailer_Info[,i] <- as.character(Retailer_Info[,i])
}

#--- Get data template (column names)
colNames <- colnames(read.csv(file=paste(myPath,"Inputs/DataTemplate.csv",sep="")))

#--- Read all the retailer review files mention in the Retailer_Info
for(retailerIndex in c(1:nrow(Retailer_Info))){  # retailerIndex <- 1
  #--- System notifications
  #sysCommand <- paste('echo "', (15/nrow(Retailer_Info)),'"; echo "# Reading file ', Retailer_Info[retailerIndex,1],'..."',sep="",collapse = "")
  #system(sysCommand)
  
  #--- Read reviews into tempDf
  tempDf <- read.delim(file=Retailer_Info$File_Path[retailerIndex],sep = ",",header = T)
  
  #--- Remove unwanted columns
  tempDf <- tempDf[,c(1:32)]
  
  #--- Save colnames of the 1st file which will be applied to the remaining datasets
  ##---colnames taken directly from data template csv rather than the first read csv file
  colnames(tempDf) <- colNames
  
  #--- Format Data
  for(i in c(1:(ncol(tempDf)))){
    #anurag
    #if(class(tempDf[,i]) == "factor"){
    #  tempDf[,i] <- as.character(tempDf[,i])
    #}
    tempDf[,i] <- as.character(tempDf[,i])
    
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

#checking retailers
table(All_Review$Retailer)
#changing retailer names if required
All_Review$Retailer[All_Review$Retailer==unique(All_Review$Retailer)[2]]<-"dickssportinggoods"

#--- Remove rows which have Review.Creation_Date & Review.Rating_Score $ProdId  as NAs
x<-table(All_Review$Retailer)
colIndex <- grep(x=colnames(All_Review),pattern = "Review.Creation_Date",ignore.case = T)
All_Review <- All_Review[complete.cases(All_Review[,colIndex]),]
#checking retailers
table(All_Review$Retailer)

colIndex <- grep(x=colnames(All_Review),pattern = "Review.Rating_Score",ignore.case = T)
All_Review <- All_Review[complete.cases(All_Review[,colIndex]),]
#checking retailers
table(All_Review$Retailer)

colIndex <- grep(x=colnames(All_Review),pattern = "Unique_Product_Identifier_Mainproduct",ignore.case = T)
All_Review <- All_Review[complete.cases(All_Review[,colIndex]),]
#checking retailers
table(All_Review$Retailer)


#changing IDs to 'RETAILER-ID':
#All_Review$Unique_Product_Identifier_Mainproduct[All_Review$Retailer=="sportsauthority"]<-paste(toupper(All_Review$Retailer[All_Review$Retailer=="sportsauthority"]),All_Review$Unique_Product_Identifier_Mainproduct[All_Review$Retailer=="sportsauthority"],sep = "-")

#changing IDs to 'RETAILER-ID':

All_Review$Unique_Product_Identifier_Mainproduct<-paste(toupper(All_Review$Retailer),All_Review$Unique_Product_Identifier_Mainproduct,sep = "-")


#--- Remove duplicates
# System command
#system('echo "# Removing duplicates..."; echo "15"')

All_Review <<- All_Review[!duplicated(All_Review[c("Retailer","Unique_Product_Identifier_Mainproduct","Review.Header","Review.Creation_Date","Review.By","Review.Text")]),]

#checking what got removed
print("old")
x
print("new")
table(All_Review$Retailer)

#--- Just checking if the no of reviews found in data is equal to the no of reviews mentioned in the total. This doesnt affect the process.
temp <- count(All_Review, c("Unique_Product_Identifier_Mainproduct"))
temp <- sqldf("SELECT a.*,b.Product_reviews FROM temp AS a INNER JOIN All_Review AS b ON a.Unique_Product_Identifier_Mainproduct=b.Unique_Product_Identifier_Mainproduct")
temp <- unique(temp)
temp$Product_reviews <- gsub(x = temp$Product_reviews,pattern = ",",replacement = "")
temp$Product_reviews <- as.numeric(as.character(temp$Product_reviews))
temp$ratio <- temp$freq / temp$Product_reviews
write.csv(temp,file = "temp.csv")

rm(temp,tempDf,colIndex)



#--- Save data
# System command
#system('echo "# Saving data..."; echo "17"')
#---------
save(All_Review,file=paste(myPath,"ProcessedData/1_All_Review_Processed.RData",sep=""))


rm(list = ls()[!(ls() %in% c('myPath',"All_Review"))])
