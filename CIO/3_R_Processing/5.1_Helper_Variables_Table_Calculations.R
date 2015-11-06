#-------------------------------------------------------------
#-- This script generates the values for table Helper_Variables based on various rules/formulae
#-- Data being taken into account is MainTable_Processed.RData & Search_Element_Weight
#-- This table should have 43+1(DataId) no of columns
#-- "Title"
#-------------------------------------------------------------

#--- This option makes the script not use exponents for large or small number
options(scipen=999)


#------------------ PROVIDE INPUTS -------------------------#
iterationNoColIndex <- 3    #-- Values from which iteration to consider for weights from Search_Element_Weight
# load Search_Element_Weight
load(file=paste0(getwd(), "/Datasets/R/Search_Element_Weight_Processed.RData"))
#-----------------------------------------------------------#


#-- Load the template
Helper_Variables <- read.csv(file=paste0(getwd(), "/Data Template/Helper_Variables.csv"))

#-- Create Data_Id column
Helper_Variables$Data_Id <- NA

#-- Convert all columns into numeric
for(i in 1:ncol(Helper_Variables)){
  Helper_Variables[,i] <- as.numeric(Helper_Variables[,i])
}

#-- Remove all the data
Helper_Variables <- Helper_Variables[-c(1:nrow(Helper_Variables)),]

#-- Shift Data_Id to the 1st position
colnames(Helper_Variables)[2:ncol(Helper_Variables)] <- colnames(Helper_Variables)[1:ncol(Helper_Variables)-1]
colnames(Helper_Variables)[1] <- "Data_Id"
# Also shift the data
Helper_Variables[,c(2:ncol(Helper_Variables))] <- Helper_Variables[,c(1:ncol(Helper_Variables)-1)]
#Helper_Variables$Data_Id <- NA



#--- Resize Helper_Variables table so that it has same length as MainTable table
# Save Helper_Variables in temp
temp <- Helper_Variables
# Copy MainTable table into Helper_Variables with same no of col as it had so that it has the same no of rows
Helper_Variables <- MainTable[,c(1:ncol(temp))]
# Format the columns according to the format it previously had
for(i in 1:ncol(temp)){
  colnames(Helper_Variables)[i] <- colnames(temp)[i]
  
  Helper_Variables[,i] <- NA
  
  if(class(temp[,i]) == "integer"){
    Helper_Variables[,i] <- as.integer(Helper_Variables[,i])
  }
  else if(class(temp[,i]) == "character"){
    Helper_Variables[,i] <- as.character(Helper_Variables[,i])
  }
  if(class(temp[,i]) == "numeric"){
    Helper_Variables[,i] <- as.numeric(Helper_Variables[,i])
  }
  else {
    Helper_Variables[,i] <- as.factor(Helper_Variables[,i])
  }
}
Helper_Variables[c(1:nrow(temp)),] <- temp[c(1:nrow(temp)),]
rm(temp,i)


#-- Apply Data Id in pivot also so that mapping can be done
Helper_Variables$Data_Id <- MainTable$Data_Id




#--------------------- FORMULA APPLICATION ----------------------------#
#--- Title ----------------------- Empty

#--- Presence of Title (%)

#--- Length of Title
Helper_Variables$Length_of_Title_weight <- 0
Helper_Variables$Length_of_Title_weight <- as.numeric(Helper_Variables$Length_of_Title_weight)

# find the column name which has to be searched in Search_Element_Weight and then get the weight
lookupValue <- sub("^\\s+|\\s+$", "", gsub(x=colnames(Helper_Variables)[4],pattern = "\\_+",replacement = " "))
weight <- as.numeric(Search_Element_Weight[Search_Element_Weight$ValueName == lookupValue,iterationNoColIndex])[1]

tempCol <- ifelse(test = MainTable$Length_Of_Title <= 2, yes = weight * 0.25, no = 0)
Helper_Variables$Length_of_Title_weight <- Helper_Variables$Length_of_Title_weight + tempCol
tempCol <- ifelse(test = ((MainTable$Length_Of_Title <= 4) & (MainTable$Length_Of_Title > 2)), yes = weight * 0.5, no = 0)
Helper_Variables$Length_of_Title_weight <- Helper_Variables$Length_of_Title_weight + tempCol
tempCol <- ifelse(test = ((MainTable$Length_Of_Title <= 6) & (MainTable$Length_Of_Title > 4)), yes = weight * 0.75, no = 0)
Helper_Variables$Length_of_Title_weight <- Helper_Variables$Length_of_Title_weight + tempCol
tempCol <- ifelse(test = ((MainTable$Length_Of_Title > 6)), yes = weight * 1, no = 0)
Helper_Variables$Length_of_Title_weight <- Helper_Variables$Length_of_Title_weight + tempCol


#--- Length of Title (%)
Helper_Variables$Length_of_Title_Percent <- Helper_Variables$Length_of_Title_weight


#--- Description

#--- Presence of Description (%)

#--- Length of description
Helper_Variables$Length_of_description_weight <- 0
Helper_Variables$Length_of_description_weight <- as.numeric(Helper_Variables$Length_of_description_weight)

# find the column name which has to be searched in Search_Element_Weight and then get the weight
lookupValue <- sub("^\\s+|\\s+$", "", gsub(x=colnames(Helper_Variables)[8],pattern = "\\_+",replacement = " "))
weight <- as.numeric(Search_Element_Weight[Search_Element_Weight$ValueName == lookupValue,iterationNoColIndex])[1]

tempCol <- ifelse(test = MainTable$Length_of_description <= 25, yes = weight * 0.25, no = 0)
Helper_Variables$Length_of_description_weight <- Helper_Variables$Length_of_description_weight + tempCol
tempCol <- ifelse(test = ((MainTable$Length_of_description <= 75) & (MainTable$Length_of_description > 25)), yes = weight * 0.5, no = 0)
Helper_Variables$Length_of_description_weight <- Helper_Variables$Length_of_description_weight + tempCol
tempCol <- ifelse(test = ((MainTable$Length_of_description <= 125) & (MainTable$Length_of_description > 75)), yes = weight * 0.75, no = 0)
Helper_Variables$Length_of_description_weight <- Helper_Variables$Length_of_description_weight + tempCol
tempCol <- ifelse(test = ((MainTable$Length_of_description > 125)), yes = weight * 1, no = 0)
Helper_Variables$Length_of_description_weight <- Helper_Variables$Length_of_description_weight + tempCol

#--- Length of Description (%)
Helper_Variables$Length_of_Description_Percent <- Helper_Variables$Length_of_description_weight


#--- Features


#--- Presence of Features (%)


#--- No_ of feature bullets
Helper_Variables$No_of_feature_bullets_weight <- 0
Helper_Variables$No_of_feature_bullets_weight <- as.numeric(Helper_Variables$No_of_feature_bullets_weight)

# find the column name which has to be searched in Search_Element_Weight and then get the weight
lookupValue <- sub("^\\s+|\\s+$", "", gsub(x=colnames(Helper_Variables)[12],pattern = "\\_+",replacement = " "))
weight <- as.numeric(Search_Element_Weight[Search_Element_Weight$ValueName == lookupValue,iterationNoColIndex])[1]

tempCol <- ifelse(test = MainTable$No_of_feature_bullets <= 2, yes = weight * 0.25, no = 0)
Helper_Variables$No_of_feature_bullets_weight <- Helper_Variables$No_of_feature_bullets_weight + tempCol
tempCol <- ifelse(test = ((MainTable$No_of_feature_bullets <= 4) & (MainTable$No_of_feature_bullets > 2)), yes = weight * 0.5, no = 0)
Helper_Variables$No_of_feature_bullets_weight <- Helper_Variables$No_of_feature_bullets_weight + tempCol
tempCol <- ifelse(test = ((MainTable$No_of_feature_bullets <= 8) & (MainTable$No_of_feature_bullets > 4)), yes = weight * 0.75, no = 0)
Helper_Variables$No_of_feature_bullets_weight <- Helper_Variables$No_of_feature_bullets_weight + tempCol
tempCol <- ifelse(test = ((MainTable$No_of_feature_bullets > 8)), yes = weight * 1, no = 0)
Helper_Variables$No_of_feature_bullets_weight <- Helper_Variables$No_of_feature_bullets_weight + tempCol


#--- No_ of Feature Bullets (%)
Helper_Variables$No_of_Feature_Bullets_Percent <- Helper_Variables$No_of_feature_bullets_weight

#--- Availability of Brand in Title
lookupValue <- sub("^\\s+|\\s+$", "", gsub(x=colnames(Helper_Variables)[14],pattern = "\\_+",replacement = " "))
weight <- as.numeric(Search_Element_Weight[Search_Element_Weight$ValueName == lookupValue,iterationNoColIndex])[1]

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

for(i in c(1:length(MainTable$Brand))){
  tryCatch({
    Helper_Variables$Availability_of_Brand_in_Title[i] <- grepl(x = tempTarget[i], pattern = tempPattern[i], ignore.case = T) * weight
  }, error=function(e){ Helper_Variables$Availability_of_Brand_in_Title[i] <- 0 })
}
Helper_Variables$Availability_of_Brand_in_Title[is.na(MainTable$Brand)] <- 0
rm(tempTarget,tempPattern)

#--- Availability of Brand in Title (%)
Helper_Variables$Availability_of_Brand_in_Title_Percent <- Helper_Variables$Availability_of_Brand_in_Title


#--- Availability of Product Type in Title


#--- Availability of Product Type in Title (%)


#--- Availability of Model in Title
lookupValue <- sub("^\\s+|\\s+$", "", gsub(x=colnames(Helper_Variables)[18],pattern = "\\_+",replacement = " "))
weight <- as.numeric(Search_Element_Weight[Search_Element_Weight$ValueName == lookupValue,iterationNoColIndex])[1]

# Check if Model is present in Product Name_ But 1st remove all the non alphanumeric characters
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

MainTable$Brand[grepl(x = MainTable$Brand, pattern = "n/a")] <- NA
for(i in c(1:length(MainTable$Brand))){
  tryCatch({
    Helper_Variables$Availability_of_Model_in_Title[i] <- grepl(x = tempTarget[i], pattern = tempPattern[i], ignore.case = T) * weight
  }, error=function(e){ Helper_Variables$Availability_of_Model_in_Title[i] <- 0 })
}
Helper_Variables$Availability_of_Model_in_Title[is.na(MainTable$Brand)] <- 0
rm(tempTarget,tempPattern)


#FN=(IFERROR(IF(BE6="n/a",0,IF(SEARCH(BE6,E6,1)>=1,1,0)),0)*VLOOKUP(FN$5,'Raw Sheet Revised'!$LH$4:$LJ$65,3,0))

#--- Availability of Model in Title (%)
Helper_Variables$Availability_of_Model_in_Title_Percent <- Helper_Variables$Availability_of_Model_in_Title

#--- Availability of MPN in Title

#--- Availability of MPN in Title (%)

#--- Availability of Variants 1 in Title(Color, capacity etc_)

#--- Availability of Variants 1 in Title (%)

#--- Availability of Variants 2 in Title(Color, capacity etc_)

#--- Availability of Variants 2 in Title (%)

#--- No_ of specification
Helper_Variables$No_of_specification_weight <- 0
Helper_Variables$No_of_specification_weight <- as.numeric(Helper_Variables$No_of_specification_weight)

# find the column name which has to be searched in Search_Element_Weight and then get the weight
lookupValue <- sub("^\\s+|\\s+$", "", gsub(x=colnames(Helper_Variables)[26],pattern = "\\_+",replacement = " "))
weight <- as.numeric(Search_Element_Weight[Search_Element_Weight$ValueName == lookupValue,iterationNoColIndex])[1]

tempCol <- ifelse(test = MainTable$No_of_specification <= 2, yes = weight * 0.25, no = 0)
Helper_Variables$No_of_specification_weight <- Helper_Variables$No_of_specification_weight + tempCol
tempCol <- ifelse(test = ((MainTable$No_of_specification <= 4) & (MainTable$No_of_specification > 2)), yes = weight * 0.5, no = 0)
Helper_Variables$No_of_specification_weight <- Helper_Variables$No_of_specification_weight + tempCol
tempCol <- ifelse(test = ((MainTable$No_of_specification <= 6) & (MainTable$No_of_specification > 4)), yes = weight * 0.75, no = 0)
Helper_Variables$No_of_specification_weight <- Helper_Variables$No_of_specification_weight + tempCol
tempCol <- ifelse(test = ((MainTable$No_of_specification > 6)), yes = weight * 1, no = 0)
Helper_Variables$No_of_specification_weight <- Helper_Variables$No_of_specification_weight + tempCol

#--- No_ of Specification (%)
Helper_Variables$No_of_Specification_Percent <- Helper_Variables$No_of_specification_weight/10

#--- No_ of images

#--- No_ of images_1
# find the column name which has to be searched in Search_Element_Weight and then get the weight
lookupValue <- sub("^\\s+|\\s+$", "", gsub(x=colnames(Helper_Variables)[29],pattern = "\\_+|[ 1]",replacement = " "))
weight <- as.numeric(Search_Element_Weight[Search_Element_Weight$ValueName == lookupValue,iterationNoColIndex])[1]

temp <- !is.na(Images[,c(2:12)])
tempNoOfImages <- rowSums(temp)

Helper_Variables$No_of_images_weight <- 0
tempCol <- ifelse(test = tempNoOfImages >= 4, yes = weight * 1, no = 0)
Helper_Variables$No_of_images_weight <- Helper_Variables$No_of_images_weight + tempCol
tempCol <- ifelse(test = ((tempNoOfImages >= 3) & (tempNoOfImages < 4)), yes = weight * 0.75, no = 0)
Helper_Variables$No_of_images_weight <- Helper_Variables$No_of_images_weight + tempCol
tempCol <- ifelse(test = ((tempNoOfImages >= 2) & (tempNoOfImages < 3)), yes = weight * 0.5, no = 0)
Helper_Variables$No_of_images_weight <- Helper_Variables$No_of_images_weight + tempCol
tempCol <- ifelse(test = ((tempNoOfImages < 2)), yes = weight * 0.25, no = 0)
Helper_Variables$No_of_images_weight <- Helper_Variables$No_of_images_weight + tempCol

rm(temp,tempNoOfImages)

#--- Avg Pix size of Images_1

#--- Avg Pix size of Images
lookupValue <- sub("^\\s+|\\s+$", "", gsub(x=colnames(Helper_Variables)[31],pattern = "\\_+|[ 1]",replacement = " "))
weight <- as.numeric(Search_Element_Weight[Search_Element_Weight$ValueName == lookupValue,iterationNoColIndex])[1]

Helper_Variables$Avg_Pix_size_of_Images_weight[grepl(x = MainTable$High_Resolution_Images,pattern = "N",ignore.case = T)] <- 0
Helper_Variables$Avg_Pix_size_of_Images_weight[grepl(x = MainTable$High_Resolution_Images,pattern = "Y",ignore.case = T)] <- weight

#--- Presence of Videos_1

#--- Presence of Videos
lookupValue <- sub("^\\s+|\\s+$", "", gsub(x=colnames(Helper_Variables)[33],pattern = "\\_+|[ 1]",replacement = " "))
weight <- as.numeric(Search_Element_Weight[Search_Element_Weight$ValueName == lookupValue,iterationNoColIndex])[1]

Helper_Variables$Presence_of_Videos[grepl(x = MainTable$Presence_of_Videos_Y_or_N,pattern = "N",ignore.case = T)] <- 0
Helper_Variables$Presence_of_Videos[grepl(x = MainTable$Presence_of_Videos_Y_or_N,pattern = "Y",ignore.case = T)] <- weight

#--- Presence of Reviews_1

#--- Presence of Reviews
lookupValue <- sub("^\\s+|\\s+$", "", gsub(x=colnames(Helper_Variables)[35],pattern = "\\_+|[ 1]",replacement = " "))
weight <- as.numeric(Search_Element_Weight[Search_Element_Weight$ValueName == lookupValue,iterationNoColIndex])[1]

Helper_Variables$Presence_of_Reviews[grepl(x = MainTable$Presence_of_Reviews_Y_or_N,pattern = "N",ignore.case = T)] <- 0
Helper_Variables$Presence_of_Reviews[grepl(x = MainTable$Presence_of_Reviews_Y_or_N,pattern = "Y",ignore.case = T)] <- weight

#--- No_ of reviews
lookupValue <- sub("^\\s+|\\s+$", "", gsub(x=colnames(Helper_Variables)[37],pattern = "\\_+|[ 1]",replacement = " "))
weight <- as.numeric(Search_Element_Weight[Search_Element_Weight$ValueName == lookupValue,iterationNoColIndex])[1]

tempInput <- MainTable$No_of_reviews

Helper_Variables$No_of_reviews_weight <- 0
tempCol <- ifelse(test = tempInput <= 2, yes = weight * 0.25, no = 0)
Helper_Variables$No_of_reviews_weight <- Helper_Variables$No_of_reviews_weight + tempCol
tempCol <- ifelse(test = ((tempInput <= 4) & (tempInput > 2)), yes = weight * 0.5, no = 0)
Helper_Variables$No_of_reviews_weight <- Helper_Variables$No_of_reviews_weight + tempCol
tempCol <- ifelse(test = ((tempInput <= 6) & (tempInput > 4)), yes = weight * 0.75, no = 0)
Helper_Variables$No_of_reviews_weight <- Helper_Variables$No_of_reviews_weight + tempCol
tempCol <- ifelse(test = ((tempInput > 6)), yes = weight * 1, no = 0)
Helper_Variables$No_of_reviews_weight <- Helper_Variables$No_of_reviews_weight + tempCol
Helper_Variables$No_of_reviews_weight[is.na(Helper_Variables$No_of_reviews_weight)] <- weight
#GG=IF(AND(BO6="", ISNUMBER(BO6)=FALSE, BO6=0)=TRUE, "", IF(BO6<=2,0.25,IF(BO6<=4,0.5,IF(BO6<=6,0.75,1)))), 0)

#--- Avg review Ratings_1

#--- Avg review Ratings
lookupValue <- sub("^\\s+|\\s+$", "", gsub(x=colnames(Helper_Variables)[39],pattern = "\\_+|[ 1]",replacement = " "))
weight <- as.numeric(Search_Element_Weight[Search_Element_Weight$ValueName == lookupValue,iterationNoColIndex])[1]

tempInput <- MainTable$Avg_review_Ratings

Helper_Variables$Avg_review_Ratings_weight <- 0
tempCol <- ifelse(test = tempInput >= 4.5, yes = weight * 1, no = 0)
Helper_Variables$Avg_review_Ratings_weight <- Helper_Variables$Avg_review_Ratings_weight + tempCol
tempCol <- ifelse(test = ((tempInput >= 4) & (tempInput < 4.5)), yes = weight * 0.75, no = 0)
Helper_Variables$Avg_review_Ratings_weight <- Helper_Variables$Avg_review_Ratings_weight + tempCol
tempCol <- ifelse(test = ((tempInput >= 3) & (tempInput < 4)), yes = weight * 0.5, no = 0)
Helper_Variables$Avg_review_Ratings_weight <- Helper_Variables$Avg_review_Ratings_weight + tempCol
tempCol <- ifelse(test = ((tempInput < 3)), yes = weight * 0.25, no = 0)
Helper_Variables$Avg_review_Ratings_weight <- Helper_Variables$Avg_review_Ratings_weight + tempCol
Helper_Variables$Avg_review_Ratings_weight[is.na(Helper_Variables$Avg_review_Ratings_weight)] <- weight
#GI=IFERROR(IF(BP6>=4.5,1,IF(BP6>=4,0.75,IF(BP6>=3,0.5,IF(BP6>=2,0.25)))),0)*VLOOKUP(GI$5,'Raw Sheet Revised'!$LH$4:$LJ$65,3,0)

#--- Presence of upsell offers_1


#--- Presence of upsell offers
lookupValue <- sub("^\\s+|\\s+$", "", gsub(x=colnames(Helper_Variables)[41],pattern = "\\_+|[ 1]",replacement = " "))
weight <- as.numeric(Search_Element_Weight[Search_Element_Weight$ValueName == lookupValue,iterationNoColIndex])[1]

tempCol <- MainTable$Presence_of_upsell_offers_Y_or_N
Helper_Variables$Presence_of_upsell_offers[tempCol == "N"] <- 0
Helper_Variables$Presence_of_upsell_offers[tempCol == "Y"] <- weight


#--- Presence of Cross sell_1


#--- Presence of Cross sell
lookupValue <- sub("^\\s+|\\s+$", "", gsub(x=colnames(Helper_Variables)[43],pattern = "\\_+|[ 1]",replacement = " "))
weight <- as.numeric(Search_Element_Weight[Search_Element_Weight$ValueName == lookupValue,iterationNoColIndex])[1]

tempCol <- MainTable$Presence_of_Cross_sell_Y_or_N
Helper_Variables$Presence_of_Cross_sell[tempCol == "N"] <- 0
Helper_Variables$Presence_of_Cross_sell[tempCol == "Y"] <- weight

#--- Final Engagement Score
Helper_Variables$Final_Engagement_Score <- rowSums(x = Helper_Variables[,c(4,8,12,15,18,26,29,31,33,35,37,39,41,43)], na.rm = T) * 100
#View(Helper_Variables[,c(4,8,12,15,18,26,29,31,33,35,37,39,41,43)])
#GN==SUM(EZ6,FD6,FH6,FK6,FN6,FV6,FY6,GA6,GC6,GE6,GG6,GI6,GK6,GM6)*100

rm(iterationNoColIndex,lookupValue,tempCol,tempInput,weight,i)

#---- REVIEW THESE COLUMNS "No_ of reviews (GG)", "Avg review Ratings (GI)",



#-- Save to txt as '~' seperated file for importing into SQL Server
write.table(x=Helper_Variables, file=paste0(getwd(), "/Datasets/txt/Helper_Variables_Processed.txt"),row.names = FALSE,na = "", quote = FALSE,append = FALSE,eol = "\n", sep = "~")
#-- Save a copy in R Data
save(x=Helper_Variables, file=paste0(getwd(), "/Datasets/R/Helper_Variables_Processed.RData"))

#-- NOTE: Use txt file to import the data into SQL Server
