#--- Collate all data
AllData <- MainTable
AllData <- cbind(AllData, Relevance[,c(2:ncol(Relevance))])
AllData <- cbind(AllData, Other_Variables[,c(2:ncol(Other_Variables))])
AllData <- cbind(AllData, Helper_Variables[,c(2:ncol(Helper_Variables))])
AllData <- cbind(AllData, Pivot[,c(2:ncol(Pivot))])
AllData <- cbind(AllData, Images[,c(2:ncol(Images))])


#--- Save the data
save(x=AllData, file=paste0(getwd(),"/Datasets/R/AllData.RData"))
write.table(x=AllData, file=paste0(getwd(),"/Datasets/txt/AllData.txt"),row.names = FALSE,na = "", quote = FALSE,append = FALSE,eol = "\n", sep = "~")
write.csv(x=AllData[c(1:5),], file=paste0(getwd(),"/Data Template/AllData.csv"),row.names = FALSE,na = "", quote = FALSE, eol = "\n")


