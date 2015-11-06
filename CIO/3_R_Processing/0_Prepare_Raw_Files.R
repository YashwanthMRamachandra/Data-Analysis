

Raw_Data <- read.csv(file=paste0(getwd(),"/Datasets/Raw data for R/RawData.csv"))

#--- Preparing MainTable
temp <- Raw_Data[,c(1:79)]
temp <- cbind(temp, Raw_Data[,c(291:311)])
write.csv(x=temp, file=paste0(getwd(),"/Datasets/Raw data for R/BaseTable_for_R.csv"),row.names = FALSE,na = "")
rm(temp)

#--- Preparing Images Table
temp <- Raw_Data[,c(237:290)]
write.csv(x=temp, file=paste0(getwd(),"/Datasets/Raw data for R/Images_for_R.csv"),row.names = FALSE,na = "")
rm(temp)

#--- Preparing Relevancy table
temp <- Raw_Data[,c(80:137)]
write.csv(x=temp, file=paste0(getwd(),"/Datasets/Raw data for R/Relevance_for_R.csv"),row.names = FALSE,na = "")
rm(temp)

rm(Raw_Data)

