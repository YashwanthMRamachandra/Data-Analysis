#-------------------------------------------------------------
#-- This script prepares the MainTable from the Raw Input
#-- MainTable should have 100+1(DataId) no of columns
#-- Include columns from: "Ugam ID" to "Count of G+1" and "Keyword6 to sd"
#-------------------------------------------------------------

#-- Read Raw file
MainTable <- read.csv(file=paste0(getwd(),"/Datasets/Raw data for R/BaseTable_for_R.csv"))

#-- Format them as Caharacter
for(i in 1:ncol(MainTable)){
  MainTable[,c(i)] <- as.character(MainTable[,i])
  MainTable[,c(i)] <- gsub("^\\s+|\\s+$", "", x=MainTable[,c(i)])
  MainTable[,c(i)] <- gsub("~", " ", x=MainTable[,c(i)])
}
rm(i)

#-- Convert all the Numeric fields
MainTable$Final_Price <- gsub(pattern = "\\$", replacement = "", x=MainTable$Final_Price)
MainTable$Final_Price <- as.numeric(as.character(MainTable$Final_Price))

MainTable$Page_Rank <- as.numeric(as.character(MainTable$Page_Rank))

MainTable$No_of_images <- as.numeric(as.character(MainTable$No_of_images))

MainTable$Length_Of_Title <- as.numeric(as.character(MainTable$Length_Of_Title))

MainTable$Length_of_description <- as.numeric(as.character(MainTable$Length_of_description))

MainTable$No_of_feature_bullets <- as.numeric(as.character(MainTable$No_of_feature_bullets))

MainTable$No_of_specification <- as.numeric(as.character(MainTable$No_of_specification))

MainTable$No_of_reviews <- as.numeric(as.character(MainTable$No_of_reviews))

MainTable$Avg_review_Ratings <- as.numeric(as.character(MainTable$Avg_review_Ratings))

MainTable$Backlinks_on_page <- as.numeric(as.character(MainTable$Backlinks_on_page))

MainTable$Backlinks_on_domain <- gsub(pattern = ",", replacement = "", x=MainTable$Backlinks_on_domain)
MainTable$Backlinks_on_domain <- as.numeric(as.character(MainTable$Backlinks_on_domain))

MainTable$PageloadTimeSeconds <- as.numeric(as.character(MainTable$PageloadTimeSeconds))

MainTable$Bounce_Rate_Percent <- gsub(pattern = "%", replacement = "", x=MainTable$Bounce_Rate_Percent)
MainTable$Bounce_Rate_Percent <- as.numeric(as.character(MainTable$Bounce_Rate_Percent))

MainTable$Domain_age_Months <- as.numeric(as.character(MainTable$Domain_age_Months))

MainTable$Text_to_code_ratio_Percent <- gsub(pattern = "%", replacement = "", x=MainTable$Text_to_code_ratio_Percent)
MainTable$Text_to_code_ratio_Percent <- as.numeric(as.character(MainTable$Text_to_code_ratio_Percent))

MainTable$Count_of_pins <- as.numeric(as.character(MainTable$Count_of_pins))

MainTable$Count_of_FB_likes <- as.numeric(as.character(MainTable$Count_of_FB_likes))

MainTable$Count_of_FB_shares <- as.numeric(as.character(MainTable$Count_of_FB_shares))

MainTable$Count_of_Tweets <- as.numeric(as.character(MainTable$Count_of_Tweets))

MainTable$Count_of_G_1 <- as.numeric(as.character(MainTable$Count_of_G_1))

MainTable$Total_Estimated_Traffic_for_KW1 <- as.numeric(as.character(MainTable$Total_Estimated_Traffic_for_KW1))
MainTable$Total_Estimated_Traffic_for_KW2 <- as.numeric(as.character(MainTable$Total_Estimated_Traffic_for_KW2))
MainTable$Total_Estimated_Traffic_for_KW3 <- as.numeric(as.character(MainTable$Total_Estimated_Traffic_for_KW3))
MainTable$Total_Estimated_Traffic_for_KW4 <- as.numeric(as.character(MainTable$Total_Estimated_Traffic_for_KW4))
MainTable$Total_Estimated_Traffic_for_KW5 <- as.numeric(as.character(MainTable$Total_Estimated_Traffic_for_KW5))





#-- Keep content_score = NA as this value will be generated later
MainTable$Content_Score <- NA

#-- Keep column sd as NA
MainTable$sd <- NA

#-- Fill NA for n/a
MainTable$Brand[grepl(x = MainTable$Brand, pattern = "n/a")] <- NA

#-- Generate unique id column
#MainTable <- MainTable[,-c(80)]
Data_id <- MainTable$Ugam_ID
Data_id <- c(1:nrow(MainTable))
MainTable <- cbind(MainTable, Data_id)
colnames(MainTable)[2:ncol(MainTable)] <- colnames(MainTable)[1:ncol(MainTable)-1]
MainTable[,c(2:ncol(MainTable))] <- MainTable[,c(1:ncol(MainTable)-1)]
MainTable[,c(1)] <- Data_id
colnames(MainTable)[1] <- "Data_Id"
rm(Data_id)



#-- Save to txt as '~' seperated file for importing into SQL Server
write.table(x=MainTable, file=paste0(getwd(),"/Datasets/txt/MainTable_Processed.txt"),row.names = FALSE,na = "", quote = FALSE,append = FALSE,eol = "\n", sep = "~")
#-- Save a copy in R Data
save(x=MainTable, file=paste0(getwd(),"/Datasets/R/MainTable_Processed.RData"))

#-- NOTE: Use txt file to import the data into SQL Server