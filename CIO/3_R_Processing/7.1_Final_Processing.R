#-------------------------------------------------------------
#-- This script generates the values for table Pivot based on various rules/formulae
#-- Data being taken into account is MainTable_Processed.RData, Images
#-------------------------------------------------------------

#--- This option makes the script not use exponents for large or small number
options(scipen=999)


#-- Load dependency data
load(file=paste0(getwd(),"/Datasets/R/MainTable_Processed.RData"))
load(file=paste0(getwd(),"/Datasets/R/Other_Variables_Processed.RData"))
load(file=paste0(getwd(),"/Datasets/R/Helper_Variables_Processed.RData"))
load(file=paste0(getwd(),"/Datasets/R/Pivot_Processed.RData"))

#--- Maintable$Content Score 
MainTable$Content_Score <- round(x = (Other_Variables$Search_Score * 0.5) + (Helper_Variables$Final_Engagement_Score * 0.5),digits = 0)

#--- Pivot$Pivot Score
Pivot$Pivot_Score <- as.character(Pivot$Pivot_Score)
MainTable$Content_Score <- as.numeric(MainTable$Content_Score)
Pivot$Pivot_Score[MainTable$Content_Score >= 85] <- "85-100"
Pivot$Pivot_Score[(MainTable$Content_Score >= 60) & (MainTable$Content_Score < 85)] <- "65-85"
Pivot$Pivot_Score[(MainTable$Content_Score >= 45) & (MainTable$Content_Score < 60)] <- "45-60"
Pivot$Pivot_Score[(MainTable$Content_Score >= 30) & (MainTable$Content_Score < 45)] <- "30-45"
Pivot$Pivot_Score[(MainTable$Content_Score >= 15) & (MainTable$Content_Score < 30)] <- "15-30"
Pivot$Pivot_Score[(MainTable$Content_Score >= 0) & (MainTable$Content_Score < 15)] <- "0-15"

#-- Save to txt as '~' seperated file for importing into SQL Server
write.table(x=MainTable, file=paste0(getwd(),"/Datasets/txt/MainTable_Processed.txt"),row.names = FALSE,na = "", quote = FALSE,append = FALSE,eol = "\n", sep = "~")
write.table(x=Pivot, file=paste0(getwd(),"/Datasets/txt/Pivot_Processed.txt"),row.names = FALSE,na = "", quote = FALSE,append = FALSE,eol = "\n", sep = "~")
#-- Save a copy in R Data
save(x=MainTable, file=paste0(getwd(),"/Datasets/R/MainTable_Processed.RData"))
save(x=Pivot, file=paste0(getwd(),"/Datasets/R/Pivot_Processed.RData"))

#-- NOTE: Use txt file to import the data into SQL Server