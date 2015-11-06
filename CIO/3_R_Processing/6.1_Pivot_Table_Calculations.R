#-------------------------------------------------------------
#-- This script generates the values for table Pivot based on various rules/formulae
#-- Data being taken into account is MainTable_Processed.RData, Images
#-- Should have 40+1 (Data_Id) no. of columns
#-- "Pivot Score" to "No_of_Variant"
#-------------------------------------------------------------

#--- This option makes the script not use exponents for large or small number
options(scipen=999)


#-- Load dependency data
load(file=paste0(getwd(), "/Datasets/R/MainTable_Processed.RData"))
load(file=paste0(getwd(), "/Datasets/R/Images_Processed.RData"))
load(file=paste0(getwd(), "/Datasets/R/Helper_Variables_Processed.RData"))
load(file=paste0(getwd(), "/Datasets/R/Relevance_Processed.RData"))
load(file=paste0(getwd(), "/Datasets/R/Other_Variables_Processed.RData"))


#-- Load the template
Pivot <- read.csv(file=paste0(getwd(), "/Data Template/Pivot.csv"))

#-- Remove all the data
Pivot <- Pivot[-c(1:nrow(Pivot)),]

#-- Convert all the Factors to Characters
for(i in 1:ncol(Pivot)){
  if(class(Pivot[,i]) == "factor"){
    Pivot[,i] <- as.character(Pivot[,i])
  }
}


#--- Format Pivot table so that it has same length as MainTable table
# Save Pivot in temp
temp <- Pivot
# Copy MainTable table into Pivot with same no of col as it had so that it has the same no of rows
Pivot <- MainTable[,c(1:ncol(temp))]
# Format the columns according to the format it had previously
for(i in 1:ncol(temp)){
  colnames(Pivot)[i] <- colnames(temp)[i]
  
  Pivot[,i] <- NA
  
  if(class(temp[,i]) == "integer"){
    Pivot[,i] <- as.integer(Pivot[,i])
  }
  else if(class(temp[,i]) == "character"){
    Pivot[,i] <- as.character(Pivot[,i])
  }
  if(class(temp[,i]) == "numeric"){
    Pivot[,i] <- as.numeric(Pivot[,i])
  }
  else {
    Pivot[,i] <- as.factor(Pivot[,i])
  }
}
rm(temp,i)





#--------------------- FORMULA APPLICATION ----------------------------#
#-- Apply Data Id in pivot also so that mapping can be done
Pivot$Data_Id <- MainTable$Data_Id

#-- Pivot_Score handled in Final_Processing_R


#-- Pivot_Image
Pivot$Pivot_Image <- MainTable$No_of_images
Pivot$Pivot_Image <- as.character(Pivot$Pivot_Image)
Pivot$Pivot_Image[as.numeric(Pivot$Pivot_Image) >= 8] <- "Images >= 8"
Pivot$Pivot_Image[as.numeric(Pivot$Pivot_Image) >= 5 & as.numeric(Pivot$Pivot_Image) < 8] <- "Images 5-7"
Pivot$Pivot_Image[as.numeric(Pivot$Pivot_Image) >= 2 & as.numeric(Pivot$Pivot_Image) < 5] <- "Images Count 2 - 4"
Pivot$Pivot_Image[as.numeric(Pivot$Pivot_Image) == 1] <- "Images = 1"
Pivot$Pivot_Image[as.numeric(Pivot$Pivot_Image) == 0] <- "Image = 0"

#-- Pivot_Specification
Pivot$Pivot_Specification <- MainTable$No_of_specification
Pivot$Pivot_Specification <- as.character(Pivot$Pivot_Specification)
Pivot$Pivot_Specification[as.numeric(Pivot$Pivot_Specification) >= 10] <- "Specification >= 10"
Pivot$Pivot_Specification[as.numeric(Pivot$Pivot_Specification) >= 5 & as.numeric(Pivot$Pivot_Specification) < 10] <- "Specification >= 5"
Pivot$Pivot_Specification[as.numeric(Pivot$Pivot_Specification) >= 1 & as.numeric(Pivot$Pivot_Specification) < 5] <- "Specification >= 1"
Pivot$Pivot_Specification[as.numeric(Pivot$Pivot_Specification) == 0] <- "No Specification"

#-- Desc_gt_than_100
Pivot$Desc_gt_than_100 <- MainTable$Length_of_description
Pivot$Desc_gt_than_100 <- as.character(Pivot$Desc_gt_than_100)
Pivot$Desc_gt_than_100[as.numeric(Pivot$Desc_gt_than_100) >= 200] <- "> 200 Words"
Pivot$Desc_gt_than_100[as.numeric(Pivot$Desc_gt_than_100) >= 100 & as.numeric(Pivot$Desc_gt_than_100) < 200] <- "100-200 Words"
Pivot$Desc_gt_than_100[as.numeric(Pivot$Desc_gt_than_100) < 100] <- "< 100 Words"

#-- Presence_of_Meta_KW_in_Description
Pivot$Presence_of_Meta_KW_in_Description <- Relevance$Relevancy_of_search_phrase_in_Descriptions_1

#-- Reviews --------------------------- Check source formula
Pivot$Reviews <- NA
Pivot$Reviews <- as.character(Pivot$Reviews)
Pivot$Reviews[(MainTable$No_of_reviews >= 100)] <- ">=100"
Pivot$Reviews[(MainTable$No_of_reviews >= 50) & (MainTable$No_of_reviews < 100)] <- "50-99"
Pivot$Reviews[(MainTable$No_of_reviews >= 1) & (MainTable$No_of_reviews < 50)] <- "1 - 50"
Pivot$Reviews[(MainTable$No_of_reviews < 1)] <- "No Reviews"
Pivot$Reviews[is.na(MainTable$No_of_reviews)] <- "No Reviews"
#GT=IF(BO6>=100,">=100",IF(BO6>=50,"50-99",IF(BO6>1,"1 - 50",IF(BO6=0,"No Review","No Review"))))
#=IF(OR(BO6=0, BO6="n/a", BO6="")=TRUE,"No Review",IF(BO6>=100,">=100",IF(BO6>=50,"50-99",IF(BO6>=1,"1 - 50"))))

#-- Rating --------------------------- Check source formula
Pivot$Rating <- NA
Pivot$Rating <- as.character(Pivot$Rating)
Pivot$Rating[MainTable$Avg_review_Ratings >= 3.5] <- ">=3.5"
Pivot$Rating[(MainTable$Avg_review_Ratings >= 2) & (MainTable$Avg_review_Ratings < 3.5)] <- "2-3.4"
Pivot$Rating[(MainTable$Avg_review_Ratings >= 1) & (MainTable$Avg_review_Ratings < 2)] <- "1 - 1.9"
Pivot$Rating[(MainTable$Avg_review_Ratings == 0)] <- "No Rating"
Pivot$Rating[is.na(MainTable$Avg_review_Ratings)] <- "No Rating"
#GU=IF(BP6>=3.5,">=3.5",IF(BP6>=2,"2-3.4",IF(BP6>1,"1 - 1.9",IF(BP6=0,"No Rating","No Rating"))))

#-- Features
Pivot$Features_pivot <- NA
Pivot$Features_pivot <- as.character(Pivot$Features_pivot)
Pivot$Features_pivot[(MainTable$No_of_feature_bullets >= 20)] <- ">=20 Features"
Pivot$Features_pivot[(MainTable$No_of_feature_bullets >= 15) & (MainTable$No_of_feature_bullets < 20)] <- ">=15 Features"
Pivot$Features_pivot[(MainTable$No_of_feature_bullets >= 10) & (MainTable$No_of_feature_bullets < 15)] <- ">=10 Features"
Pivot$Features_pivot[(MainTable$No_of_feature_bullets >= 5) & (MainTable$No_of_feature_bullets < 10)] <- ">=5 Features"
Pivot$Features_pivot[(MainTable$No_of_feature_bullets >= 1) & (MainTable$No_of_feature_bullets < 5)] <- "Features 1 - 5"
Pivot$Features_pivot[(MainTable$No_of_feature_bullets == 0)] <- "No Features"

#-- No of Images
temp <- !is.na(Images[,c(2:12)])
Pivot$No_of_Images_pivot <- rowSums(temp)
rm(temp)

#-- Presence_of_Description
Pivot$Presence_of_Description <- MainTable$product_description
Pivot$Presence_of_Description <- as.character(Pivot$Presence_of_Description)
Pivot$Presence_of_Description[Pivot$Presence_of_Description != "n/a"] <- "Y"
Pivot$Presence_of_Description[Pivot$Presence_of_Description == "n/a"] <- "N"

#-- Presence_of_features
Pivot$Presence_of_features <- MainTable$Features
Pivot$Presence_of_features <- as.character(Pivot$Presence_of_features)
Pivot$Presence_of_features[Pivot$Presence_of_features != "n/a"] <- "Y"
Pivot$Presence_of_features[Pivot$Presence_of_features == "n/a"] <- "N"

#-- Brand_in_Specification
tempBrand <- MainTable$Brand
tempBrand[is.na(tempBrand)] <- "n/a"
tempModel <- MainTable$Model
tempModel[is.na(tempModel)] <- "n/a"
tempCol <- paste(tempBrand,tempModel,sep = "")
Pivot$Brand_in_Specification <- NA
Pivot$Brand_in_Specification <- as.character(Pivot$Brand_in_Specification)
Pivot$Brand_in_Specification[grepl(x = tempCol, pattern = "n/a")] <- "Not Available"
Pivot$Brand_in_Specification[!grepl(x = tempCol, pattern = "n/a")] <- "Available"
rm(tempCol,tempBrand,tempModel)

#-- Length_of_Title
Pivot$Length_of_Title_pivot <- gsub("^\\s+|\\s+$", "", MainTable$product_name)
Pivot$Length_of_Title_pivot <- as.character(Pivot$Length_of_Title_pivot)
Pivot$Length_of_Title_pivot[nchar(Pivot$Length_of_Title_pivot) != 0] <- nchar(Pivot$Length_of_Title_pivot)-nchar(gsub(" ","",Pivot$Length_of_Title_pivot))+1
Pivot$Length_of_Title_pivot[nchar(Pivot$Length_of_Title_pivot) == 0] <- "0"

#-- Mandatory_words_in_Title ---------------------- Confusion
# Check if Brand is present in Product Name_ But 1st remove all the non alphanumeric characters
tempTarget <- MainTable$product_name
tempPattern <- MainTable$Brand

tempTarget <- gsub("\\(", "", as.character(tempTarget))
tempTarget <- gsub("\\)", "", as.character(tempTarget))
tempTarget <- gsub("%", "", as.character(tempTarget))
tempTarget <- gsub("-", " ", as.character(tempTarget))
tempTarget <- gsub("\\+", " ", as.character(tempTarget))
tempTarget <- gsub("^\\s+|\\s+$", "", as.character(tempTarget))
tempTarget <- gsub("\\_", "", as.character(tempTarget))

tempPattern <- gsub("\\(", "", as.character(tempPattern))
tempPattern <- gsub("\\)", "", as.character(tempPattern))
tempPattern <- gsub("%", "", as.character(tempPattern))
tempPattern <- gsub("-", " ", as.character(tempPattern))
tempPattern <- gsub("\\+", " ", as.character(tempPattern))
tempPattern <- gsub("^\\s+|\\s+$", "", as.character(tempPattern))
tempPattern <- gsub("\\_", "", as.character(tempPattern))

tempBrandInProdName <- tempTarget  # Create temp
tempBrandInProdName <- NA
tempBrandInProdName <- as.logical(tempBrandInProdName)
for(i in c(1:length(MainTable$Brand))){
  tryCatch({
    tempBrandInProdName[i] <- grepl(x = tempTarget[i], pattern = tempPattern[i], ignore_case = T)
  }, error=function(e){ tempBrandInProdName[i] <- 0 })
}

#--

tempTarget <- MainTable$product_name
tempPattern <- MainTable$Model

tempTarget <- gsub("\\(", "", as.character(tempTarget))
tempTarget <- gsub("\\)", "", as.character(tempTarget))
tempTarget <- gsub("%", "", as.character(tempTarget))
tempTarget <- gsub("-", " ", as.character(tempTarget))
tempTarget <- gsub("\\+", " ", as.character(tempTarget))
tempTarget <- gsub("^\\s+|\\s+$", "", as.character(tempTarget))
tempTarget <- gsub("\\_", "", as.character(tempTarget))

tempPattern <- gsub("\\(", "", as.character(tempPattern))
tempPattern <- gsub("\\)", "", as.character(tempPattern))
tempPattern <- gsub("%", "", as.character(tempPattern))
tempPattern <- gsub("-", " ", as.character(tempPattern))
tempPattern <- gsub("\\+", " ", as.character(tempPattern))
tempPattern <- gsub("^\\s+|\\s+$", "", as.character(tempPattern))
tempPattern <- gsub("\\_", "", as.character(tempPattern))

tempModelInModelName <- tempTarget  # Create temp
tempModelInModelName <- NA
tempModelInModelName <- as.logical(tempModelInModelName)
for(i in c(1:length(MainTable$Brand))){
  tryCatch({
    tempModelInModelName[i] <- grepl(x = tempTarget[i], pattern = tempPattern[i], ignore_case = T)
  }, error=function(e){ tempModelInModelName[i] <- 0 })
}

Pivot$Mandatory_words_in_Title <- NA
Pivot$Mandatory_words_in_Title[(tempBrandInProdName & tempModelInModelName) == T] <- "Available"
Pivot$Mandatory_words_in_Title[is.na(Pivot$Mandatory_words_in_Title)] <- "Not Available"
#HB=IFERROR(IF(AND(IF(SEARCH(BD6,E6)>0,1,0),IF(SEARCH(BE6,E6)>0,1,0))=TRUE,"Available","Not Available"),"Not Available")

#-- Meta_Desc
Pivot$Meta_Desc <- gsub("^\\s+|\\s+$", "", MainTable$meta_descriptions)
Pivot$Meta_Desc <- as.character(Pivot$Meta_Desc)
Pivot$Meta_Desc <- gsub("\\s+"," ",Pivot$Meta_Desc)
Pivot$Meta_Desc[nchar(Pivot$Meta_Desc) != 0] <- nchar(Pivot$Meta_Desc)-nchar(gsub("\\s+","",Pivot$Meta_Desc))+1
Pivot$Length_of_Title_pivot[nchar(Pivot$Meta_Desc) == 0] <- "0"

#-- Relevancy_of_Content
Pivot$Relevancy_of_Content <- Relevance$Score1

#-- Relevancy_of_Content_Percent
Pivot$Relevancy_of_ContentPercent <- Pivot$Relevancy_of_Content/35

#-- Pivot$Relevancy_of_Images_Video
Pivot$Relevancy_of_Images_Video <- Relevance$Score2

#-- Relevancy_of_Images_Video_Percent
Pivot$Relevancy_of_Images_VideoPercent <- Pivot$Relevancy_of_Images_Video/10

#-- Backlinks_Page_Rank
Pivot$Backlinks_Page_Rank <- Other_Variables$Score3

#-- Backlinks_Page_Rank_Percent
Pivot$Backlinks_Page_RankPercent <- Pivot$Backlinks_Page_Rank/25

#-- Page_Technical
Pivot$Page_Technical <- Other_Variables$Score4

#-- Page_Technical_Percent
Pivot$Page_TechnicalPercent <- Pivot$Page_Technical/7

#-- Social_Factors
Pivot$Social_Factors <- Other_Variables$Score5

#-- Social_Factors_Percent
Pivot$Social_FactorsPercent <- Pivot$Social_Factors

#-- Product_Information
Pivot$Product_Information <- rowSums(x = Helper_Variables[,c(2,4,6,8,10,12,14,16,18,20,22,24,26)],na.rm = T)

#-- Product_Information_Percent
Pivot$Product_InformationPercent <- Pivot$Product_Information/50

#-- Image_Video_content
Pivot$Image_Video_content <- rowSums(x = Helper_Variables[,c(29,31,33)], na.rm = T)

#-- Image_Video_content_Percent
Pivot$Image_Video_contentPercent <- Pivot$Image_Video_content/25

#-- Reviews_Ratings
Pivot$Reviews_Ratings <- rowSums(x = Helper_Variables[,c(35,37,39)], na.rm = T)


#-- Reviews_Ratings_Percent
Pivot$Reviews_RatingsPercent <- Pivot$Reviews_Ratings/20

#-- Content_for_upselling_Cross_Sell
Pivot$Content_for_upselling_or_Cross_Sell <- rowSums(x = Helper_Variables[,c(41,43)], na.rm = T)

#-- Content_for_upselling_Cross_Sell_Percent
Pivot$Content_for_upselling_or_Cross_SellPercent <- Pivot$Content_for_upselling_or_Cross_Sell/5

#-- Sum_of_Estimated_Traffic
temp1 <- grep(x=colnames(MainTable),pattern = "Estimated_Traffic")
temp1 <- temp1[1:(length(temp1)-1)] #-- Last column is not taken for some reason
tempTbl <- MainTable[,c(temp1)]
for(i in 1:ncol(tempTbl)){
  tempTbl[,i] <- as.numeric(as.character(tempTbl[,i]))
}
Pivot$Sum_of_Estimated_Traffic <- rowSums(x=tempTbl[,c(1:length(temp1))],na.rm = T)
rm(temp1,tempTbl)

#-- Sum_of_Estimated_Traffic_Bucket
Pivot$Sum_of_Estimated_Traffic_Bucket <- Pivot$Sum_of_Estimated_Traffic
Pivot$Sum_of_Estimated_Traffic_Bucket[] <- "0"
Pivot$Sum_of_Estimated_Traffic_Bucket[as.numeric(Pivot$Sum_of_Estimated_Traffic) > 1] <- "1-500"
Pivot$Sum_of_Estimated_Traffic_Bucket[as.numeric(Pivot$Sum_of_Estimated_Traffic) > 500] <- "500-1000"
Pivot$Sum_of_Estimated_Traffic_Bucket[as.numeric(Pivot$Sum_of_Estimated_Traffic) > 1000] <- ">1000"

#-- Traffic Weightage ---------------------- Confusion
#Pivot$Traffic_Weightage


#-- Weighted_Google_Rank ---------------------- Confusion
Pivot$Weighted_Google_Rank <- "0"
Pivot$Weighted_Google_Rank <- Pivot$Weighted_Google_Rank[as.numeric(Pivot$Traffic_Weightage) > 0] <- "1-3"
Pivot$Weighted_Google_Rank <- Pivot$Weighted_Google_Rank[as.numeric(Pivot$Traffic_Weightage) > 3] <- "4 - 7"
Pivot$Weighted_Google_Rank <- Pivot$Weighted_Google_Rank[as.numeric(Pivot$Traffic_Weightage) > 7] <- "8 - 30"
Pivot$Weighted_Google_Rank <- Pivot$Weighted_Google_Rank[as.numeric(Pivot$Traffic_Weightage) > 30] <- "Did not surface in Top 30 position on Google"
Pivot$Weighted_Google_Rank <- Pivot$Weighted_Google_Rank[is.na(as.numeric(Pivot$Traffic_Weightage))] <- NA

#-- Page_Rank
Pivot$Page_Rank_pivot <- MainTable$Page_Rank
Pivot$Page_Rank_pivot <- as.character(Pivot$Page_Rank_pivot)
Pivot$Page_Rank_pivot[as.numeric(Pivot$Page_Rank_pivot) >= 71] <- "71 - 100"
Pivot$Page_Rank_pivot[as.numeric(Pivot$Page_Rank_pivot) >= 31 & as.numeric(Pivot$Page_Rank_pivot) < 71] <- "31-70"
Pivot$Page_Rank_pivot[as.numeric(Pivot$Page_Rank_pivot) >= 1 & as.numeric(Pivot$Page_Rank_pivot) < 31] <- "1-30"

#-- Sum_of_Social_Signals (No formula yet)
Pivot$Sum_of_Social_Signals <- as.numeric(Pivot$Sum_of_Social_Signals)

#--- No_of_Variant (No formula yet)
Pivot$No_of_Variant <- as.numeric(Pivot$No_of_Variant)

#--- Cleanup
rm(i,tempBrandInProdName,tempCol,tempColIndex,tempModelInModelName,tempPattern,tempTarget)

#-- Save to txt as '~' seperated file for importing into SQL Server
write.table(x=Pivot, file=paste0(getwd(), "/Datasets/txt/Pivot_Processed.txt"),row.names = FALSE,na = "", quote = FALSE,append = FALSE,eol = "\n", sep = "~")
#-- Save a copy in R Data
save(x=Pivot, file=paste0(getwd(), "/Datasets/R/Pivot_Processed.RData"))

#-- NOTE: Use txt file to import the data into SQL Server