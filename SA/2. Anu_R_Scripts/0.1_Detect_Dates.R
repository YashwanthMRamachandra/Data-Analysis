#--- Take filepath from terminal
#myPath <- commandArgs(trailingOnly = TRUE)
#myPath <- paste(myPath,"/",sep="")

###---hardcoding mypath
setwd("C:/Yashwanth/Sports Authority/1. Inputs/2. Review All/0. All Categories/1. activity monitors/")
myPath<-"C:/Yashwanth/Sports Authority/1. Inputs/2. Review All/0. All Categories/1. activity monitors/"

#--- Get names of the data files
dataFilesPath<-paste(myPath,"Anurag_Raw_Data/",sep ="")
dataFiles <- list.files(path=dataFilesPath)

#--- Prepare input file
Retailer_Info <- read.delim(file=paste(myPath,"Inputs/Retailer_Info.csv",sep =""),sep = ",",header = T,row.names=NULL)
#Retailer_Info<-Retailer_Info[0,]



#--- Read all the retailer review files listed in the dataFiles
for(retailerIndex in c(1:length(dataFiles))){  # retailerIndex <- 1
  #--- System notifications 
  #sysCommand <- paste('echo "', (15/length(dataFiles)),'"; echo "# Reading file ', dataFiles[retailerIndex],'..."',sep="",collapse = "")
  #system(sysCommand)
  
  #--- Read reviews into tempDf
  fileToRead <- paste(dataFilesPath,dataFiles[retailerIndex],sep="")
  tempDf <- read.delim(file=fileToRead,sep = ",",header = T,nrows = 100)
  
  #--- Remove unwanted columns
  tempDf <- tempDf[,c(1:32)]
  
  #--- Save colnames of the 1st file which will be applied to the remaining datasets
  
  colnames(tempDf) <- colnames(read.csv(file=paste(myPath,"Inputs/DataTemplate.csv",sep="")))
  
  
  #--- Take sample dates and write in the file
  Retailer_Info[retailerIndex,] <- c(as.character(dataFiles[retailerIndex]),as.character(fileToRead),"",as.character(tempDf$Review.Creation_Date[c(1,12,23,31,37,42,57,59,67,87)]),"")
}

write.csv(x=Retailer_Info,file=paste(myPath,"/Inputs/Retailer_Info.csv",sep =""),row.names=F)

rm(list = ls()[!(ls() %in% c('myPath','dataFilesPath'))])
