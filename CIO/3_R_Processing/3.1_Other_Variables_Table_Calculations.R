#-------------------------------------------------------------
#-- This script generates the values for table Other_Variables based on various rules/formulae
#-- Data being taken into account is MainTable_Processed.RData & Search_Element_Weight
#-- This table should have 16+1(DataId) no of columns
#-- "Backlinks on page" to "Search Score"
#-------------------------------------------------------------

#--- This option makes the script not use exponents for large or small number
options(scipen=999)


#------------------ PROVIDE INPUTS -------------------------#
iterationNoColIndex <- 3    #-- Values from which iteration to consider for weights from Search_Element_Weight
# load Search_Element_Weight
load(file=paste0(getwd(),"/Datasets/R/Search_Element_Weight_Processed.RData"))
# load MainTable
load(file=paste0(getwd(),"/Datasets/R/MainTable_Processed.RData"))
#-----------------------------------------------------------#


#-- Load the template
Other_Variables <- read.csv(file=paste0(getwd(),"/Data Template/Other_Variables.csv"))

#-- Create Data_Id column
Other_Variables$Data_Id <- NA

#-- Convert all columns into numeric
for(i in 1:ncol(Other_Variables)){
  Other_Variables[,i] <- as.numeric(Other_Variables[,i])
}

#-- Remove all the data
Other_Variables <- Other_Variables[-c(1:nrow(Other_Variables)),]

#-- Shift Data_Id to the 1st position
colnames(Other_Variables)[2:ncol(Other_Variables)] <- colnames(Other_Variables)[1:ncol(Other_Variables)-1]
colnames(Other_Variables)[1] <- "Data_Id"
# Also shift the data
Other_Variables[,c(2:ncol(Other_Variables))] <- Other_Variables[,c(1:ncol(Other_Variables)-1)]



#--- Resize Other_Variables table so that it has same length as MainTable table
# Save Other_Variables in temp
temp <- Other_Variables
# Copy MainTable table into Other_Variables with same no of col as it had so that it has the same no of rows
Other_Variables <- MainTable[,c(1:ncol(temp))]
# Format the columns according to the format it previously had
for(i in 1:ncol(temp)){
  colnames(Other_Variables)[i] <- colnames(temp)[i]
  
  Other_Variables[,i] <- NA
  
  if(class(temp[,i]) == "integer"){
    Other_Variables[,i] <- as.integer(Other_Variables[,i])
  }
  else if(class(temp[,i]) == "character"){
    Other_Variables[,i] <- as.character(Other_Variables[,i])
  }
  if(class(temp[,i]) == "numeric"){
    Other_Variables[,i] <- as.numeric(Other_Variables[,i])
  }
  else {
    Other_Variables[,i] <- as.factor(Other_Variables[,i])
  }
}
Other_Variables[c(1:nrow(temp)),] <- temp[c(1:nrow(temp)),]
rm(temp,i)


#-- Apply Data Id in pivot also so that mapping can be done
Other_Variables$Data_Id <- MainTable$Data_Id



#--------------------- FORMULA APPLICATION ----------------------------#
#   We need to find average across mutliple sets of columns (5 columns a time, 6 sets) Therefore we are
#   maintaining a lookup table which has the column indeces of the 6 sets 'startCol' signifies where
#   the first column is, 'endCol' the last column 'avgCol' is where the average of the columns are to be saved
#   This is done to avoid repeating the codes If in future more sets of columns are added, just add another 
#   row to this lookup table

colNoBook <- Other_Variables[c(1:3),c(1:3)]
colnames(colNoBook)[1] <- "startCol"
colnames(colNoBook)[2] <- "endCol"
colnames(colNoBook)[3] <- "scoreCol"
colNoBook[1,] <- c(2,4,5)
colNoBook[2,] <- c(6,9,10)
colNoBook[3,] <- c(11,15,16)


#-------------------------------- Backlinks on page ---------------------------------#
MainTable$Backlinks_on_page <- as.numeric(MainTable$Backlinks_on_page)

# find the column name which has to be searched in Search_Element_Weight and then get the weight
lookupValue <- sub("^\\s+|\\s+$", "", gsub(x=colnames(Other_Variables)[2],pattern = "\\_+",replacement = " ", ignore.case = T))
weight <- as.numeric(Search_Element_Weight[Search_Element_Weight$ValueName == lookupValue,iterationNoColIndex])

Other_Variables$Backlinks_on_page_weight <- 0

tempCol <- ifelse(test = MainTable$Backlinks_on_page <= 25, yes = weight * 0.25, no = 0)
Other_Variables$Backlinks_on_page_weight <- Other_Variables$Backlinks_on_page_weight + tempCol
tempCol <- ifelse(test = ((MainTable$Backlinks_on_page <= 50) & (MainTable$Backlinks_on_page > 25)), yes = weight * 0.5, no = 0)
Other_Variables$Backlinks_on_page_weight <- Other_Variables$Backlinks_on_page_weight + tempCol
tempCol <- ifelse(test = ((MainTable$Backlinks_on_page <= 100) & (MainTable$Backlinks_on_page > 50)), yes = weight * 0.75, no = 0)
Other_Variables$Backlinks_on_page_weight <- Other_Variables$Backlinks_on_page_weight + tempCol
tempCol <- ifelse(test = ((MainTable$Backlinks_on_page > 100)), yes = weight * 1, no = 0)
Other_Variables$Backlinks_on_page_weight <- Other_Variables$Backlinks_on_page_weight + tempCol

rm(tempCol)
#------------------------------------------------------------------------------------#


#-------------------------------- Backlinks on domain ---------------------------------#
MainTable$Backlinks_on_domain <- as.numeric(MainTable$Backlinks_on_domain)

# find the column name which has to be searched in Search_Element_Weight and then get the weight
lookupValue <- sub("^\\s+|\\s+$", "", gsub(x=colnames(Other_Variables)[3],pattern = "\\_+",replacement = " ", ignore.case = T))
weight <- as.numeric(Search_Element_Weight[Search_Element_Weight$ValueName == lookupValue,iterationNoColIndex])

Other_Variables$Backlinks_on_domain_weight <- 0

tempCol <- ifelse(test = MainTable$Backlinks_on_domain <= 1000000, yes = weight * 0.25, no = 0)
Other_Variables$Backlinks_on_domain_weight <- Other_Variables$Backlinks_on_domain_weight + tempCol
tempCol <- ifelse(test = ((MainTable$Backlinks_on_domain <= 10000000) & (MainTable$Backlinks_on_domain > 1000000)), yes = weight * 0.5, no = 0)
Other_Variables$Backlinks_on_domain_weight <- Other_Variables$Backlinks_on_domain_weight + tempCol
tempCol <- ifelse(test = ((MainTable$Backlinks_on_domain <= 50000000) & (MainTable$Backlinks_on_domain > 10000000)), yes = weight * 0.75, no = 0)
Other_Variables$Backlinks_on_domain_weight <- Other_Variables$Backlinks_on_domain_weight + tempCol
tempCol <- ifelse(test = ((MainTable$Backlinks_on_domain > 50000000)), yes = weight * 1, no = 0)
Other_Variables$Backlinks_on_domain_weight <- Other_Variables$Backlinks_on_domain_weight + tempCol

rm(tempCol)
#------------------------------------------------------------------------------------#


#-------------------------------- Pagerank ---------------------------------#
MainTable$Page_Rank <- as.numeric(MainTable$Page_Rank)

# find the column name which has to be searched in Search_Element_Weight and then get the weight
lookupValue <- sub("^\\s+|\\s+$", "", gsub(x=colnames(Other_Variables)[4],pattern = "\\_+",replacement = " ", ignore.case = T))
weight <- as.numeric(Search_Element_Weight[Search_Element_Weight$ValueName == lookupValue,iterationNoColIndex])

Other_Variables$Pagerank <- 0

tempCol <- ifelse(test = MainTable$Page_Rank <= 15, yes = weight * 0.25, no = 0)
Other_Variables$Pagerank <- Other_Variables$Pagerank + tempCol
tempCol <- ifelse(test = ((MainTable$Page_Rank <= 25) & (MainTable$Page_Rank > 15)), yes = weight * 0.5, no = 0)
Other_Variables$Pagerank <- Other_Variables$Pagerank + tempCol
tempCol <- ifelse(test = ((MainTable$Page_Rank <= 30) & (MainTable$Page_Rank > 25)), yes = weight * 0.75, no = 0)
Other_Variables$Pagerank <- Other_Variables$Pagerank + tempCol
tempCol <- ifelse(test = ((MainTable$Page_Rank > 30)), yes = weight * 1, no = 0)
Other_Variables$Pagerank <- Other_Variables$Pagerank + tempCol

rm(tempCol)
#------------------------------------------------------------------------------------#

#-------------------------------- Score3 ---------------------------------#
Other_Variables$Score3 <- rowSums(x = Other_Variables[,c(2:4)], na.rm = T)*100
#------------------------------------------------------------------------------------#










#-------------------------------- Pageload Time ---------------------------------#
MainTable$PageloadTimeSeconds <- as.numeric(MainTable$PageloadTimeSeconds)

# find the column name which has to be searched in Search_Element_Weight and then get the weight
lookupValue <- sub("^\\s+|\\s+$", "", gsub(x=colnames(Other_Variables)[6],pattern = "\\_+",replacement = " ", ignore.case = T))
weight <- as.numeric(Search_Element_Weight[Search_Element_Weight$ValueName == lookupValue,iterationNoColIndex])

Other_Variables$Pageload_Time_Seconds <- 0

tempCol <- ifelse(test = (MainTable$PageloadTimeSeconds >= 10) | (MainTable$PageloadTimeSeconds == 0), yes = weight * 0.25, no = 0)
Other_Variables$Pageload_Time_Seconds <- Other_Variables$Pageload_Time_Seconds + tempCol
tempCol <- ifelse(test = ((MainTable$PageloadTimeSeconds >= 5) & (MainTable$PageloadTimeSeconds < 10)), yes = weight * 0.5, no = 0)
Other_Variables$Pageload_Time_Seconds <- Other_Variables$Pageload_Time_Seconds + tempCol
tempCol <- ifelse(test = ((MainTable$PageloadTimeSeconds >= 2) & (MainTable$PageloadTimeSeconds < 5)), yes = weight * 0.75, no = 0)
Other_Variables$Pageload_Time_Seconds <- Other_Variables$Pageload_Time_Seconds + tempCol
tempCol <- ifelse(test = ((MainTable$PageloadTimeSeconds < 2)), yes = weight * 1, no = 0)
Other_Variables$Pageload_Time_Seconds <- Other_Variables$Pageload_Time_Seconds + tempCol

rm(tempCol)
#------------------------------------------------------------------------------------#

#-------------------------------- Bounce Rate ---------------------------------#
MainTable$Bounce_Rate_Percent <- as.numeric(MainTable$Bounce_Rate_Percent)

# find the column name which has to be searched in Search_Element_Weight and then get the weight
lookupValue <- sub("^\\s+|\\s+$", "", gsub(x=colnames(Other_Variables)[7],pattern = "\\_+",replacement = " ", ignore.case = T))
weight <- as.numeric(Search_Element_Weight[Search_Element_Weight$ValueName == lookupValue,iterationNoColIndex])

Other_Variables$Bounce_Rate_Percent_weight <- 0

tempCol <- ifelse(test = ((MainTable$Bounce_Rate_Percent == 50)), yes = 0, no = 0)
Other_Variables$Bounce_Rate_Percent_weight <- Other_Variables$Bounce_Rate_Percent_weight + tempCol
tempCol <- ifelse(test = (MainTable$Bounce_Rate_Percent >= 35), yes = weight * 0.25, no = 0)
Other_Variables$Bounce_Rate_Percent_weight <- Other_Variables$Bounce_Rate_Percent_weight + tempCol
tempCol <- ifelse(test = ((MainTable$Bounce_Rate_Percent >= 20) & (MainTable$Bounce_Rate_Percent < 35)), yes = weight * 0.5, no = 0)
Other_Variables$Bounce_Rate_Percent_weight <- Other_Variables$Bounce_Rate_Percent_weight + tempCol
tempCol <- ifelse(test = ((MainTable$Bounce_Rate_Percent >= 10) & (MainTable$Bounce_Rate_Percent < 20)), yes = weight * 0.75, no = 0)
Other_Variables$Bounce_Rate_Percent_weight <- Other_Variables$Bounce_Rate_Percent_weight + tempCol
tempCol <- ifelse(test = ((MainTable$Bounce_Rate_Percent < 10)), yes = weight * 1, no = 0)
Other_Variables$Bounce_Rate_Percent_weight <- Other_Variables$Bounce_Rate_Percent_weight + tempCol

rm(tempCol)
#------------------------------------------------------------------------------------#

#-------------------------------- Domain age ---------------------------------#
MainTable$Domain_age_Months <- as.numeric(MainTable$Domain_age_Months)

# find the column name which has to be searched in Search_Element_Weight and then get the weight
lookupValue <- sub("^\\s+|\\s+$", "", gsub(x=colnames(Other_Variables)[8],pattern = "\\_+",replacement = " ", ignore.case = T))
weight <- as.numeric(Search_Element_Weight[Search_Element_Weight$ValueName == lookupValue,iterationNoColIndex])

Other_Variables$Domain_age_Months_weight <- 0

tempCol <- ifelse(test = (MainTable$Domain_age_Months >= 180), yes = weight * 1, no = 0)
Other_Variables$Domain_age_Months_weight <- Other_Variables$Domain_age_Months_weight + tempCol
tempCol <- ifelse(test = ((MainTable$Domain_age_Months >= 144) & (MainTable$Domain_age_Months < 180)), yes = weight * 0.75, no = 0)
Other_Variables$Domain_age_Months_weight <- Other_Variables$Domain_age_Months_weight + tempCol
tempCol <- ifelse(test = ((MainTable$Domain_age_Months >= 60) & (MainTable$Domain_age_Months < 144)), yes = weight * 0.75, no = 0)
Other_Variables$Domain_age_Months_weight <- Other_Variables$Domain_age_Months_weight + tempCol
tempCol <- ifelse(test = ((MainTable$Domain_age_Months < 60)), yes = weight * 0.25, no = 0)
Other_Variables$Domain_age_Months_weight <- Other_Variables$Domain_age_Months_weight + tempCol

rm(tempCol)
#------------------------------------------------------------------------------------#

#-------------------------------- Text-to-code ratio ---------------------------------#
MainTable$Text_to_code_ratio_Percent <- as.numeric(MainTable$Text_to_code_ratio_Percent)

# find the column name which has to be searched in Search_Element_Weight and then get the weight
lookupValue <- sub("^\\s+|\\s+$", "", gsub(x=colnames(Other_Variables)[9],pattern = "\\_+",replacement = " ", ignore.case = T))
weight <- as.numeric(Search_Element_Weight[Search_Element_Weight$ValueName == lookupValue,iterationNoColIndex])

Other_Variables$Text_to_code_ratio_Percent_weight <- 0

tempCol <- ifelse(test = (MainTable$Text_to_code_ratio_Percent < 0.05), yes = 0, no = 0)
Other_Variables$Text_to_code_ratio_Percent_weight <- Other_Variables$Text_to_code_ratio_Percent_weight + tempCol
tempCol <- ifelse(test = (MainTable$Text_to_code_ratio_Percent >= 0.05), yes = weight * 0.25, no = 0)
Other_Variables$Text_to_code_ratio_Percent_weight <- Other_Variables$Text_to_code_ratio_Percent_weight + tempCol
tempCol <- ifelse(test = (MainTable$Text_to_code_ratio_Percent >= 0.1), yes = weight * 0.5, no = 0)
Other_Variables$Text_to_code_ratio_Percent_weight <- Other_Variables$Text_to_code_ratio_Percent_weight + tempCol
tempCol <- ifelse(test = (MainTable$Text_to_code_ratio_Percent >= 0.15), yes = weight * 0.75, no = 0)
Other_Variables$Text_to_code_ratio_Percent_weight <- Other_Variables$Text_to_code_ratio_Percent_weight + tempCol

rm(tempCol)
#------------------------------------------------------------------------------------#

#-------------------------------- Score4 ---------------------------------#
Other_Variables$Score4 <- rowSums(x = Other_Variables[,c(6:9)], na.rm = T)*100
#------------------------------------------------------------------------------------#


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>...


#-------------------------------- Count of pins ---------------------------------#
MainTable$Count_of_pins <- as.numeric(MainTable$Count_of_pins)

# find the column name which has to be searched in Search_Element_Weight and then get the weight
lookupValue <- sub("^\\s+|\\s+$", "", gsub(x=colnames(Other_Variables)[11],pattern = "\\_+",replacement = " ", ignore.case = T))
weight <- as.numeric(Search_Element_Weight[Search_Element_Weight$ValueName == lookupValue,iterationNoColIndex])

Other_Variables$Count_of_pins_weight <- 0

tempCol <- ifelse(test = (MainTable$Count_of_pins <= 100), yes = weight * 0.25, no = 0)
Other_Variables$Count_of_pins_weight <- Other_Variables$Count_of_pins_weight + tempCol
tempCol <- ifelse(test = ((MainTable$Count_of_pins <= 300) & (MainTable$Count_of_pins > 100)), yes = weight * 0.5, no = 0)
Other_Variables$Count_of_pins_weight <- Other_Variables$Count_of_pins_weight + tempCol
tempCol <- ifelse(test = ((MainTable$Count_of_pins <= 500) & (MainTable$Count_of_pins > 300)), yes = weight * 0.75, no = 0)
Other_Variables$Count_of_pins_weight <- Other_Variables$Count_of_pins_weight + tempCol
tempCol <- ifelse(test = ((MainTable$Count_of_pins > 500)), yes = weight * 1, no = 0)
Other_Variables$Count_of_pins_weight <- Other_Variables$Count_of_pins_weight + tempCol

rm(tempCol)
#------------------------------------------------------------------------------------#

#-------------------------------- Count of FB likes ---------------------------------#
MainTable$Count_of_FB_likes <- as.numeric(MainTable$Count_of_FB_likes)

# find the column name which has to be searched in Search_Element_Weight and then get the weight
lookupValue <- sub("^\\s+|\\s+$", "", gsub(x=colnames(Other_Variables)[12],pattern = "\\_+",replacement = " ", ignore.case = T))
weight <- as.numeric(Search_Element_Weight[Search_Element_Weight$ValueName == lookupValue,iterationNoColIndex])

Other_Variables$Count_of_FB_likes_weight <- 0

tempCol <- ifelse(test = (MainTable$Count_of_FB_likes <= 100), yes = weight * 0.25, no = 0)
Other_Variables$Count_of_FB_likes_weight <- Other_Variables$Count_of_FB_likes_weight + tempCol
tempCol <- ifelse(test = ((MainTable$Count_of_FB_likes <= 300) & (MainTable$Count_of_FB_likes > 100)), yes = weight * 0.5, no = 0)
Other_Variables$Count_of_FB_likes_weight <- Other_Variables$Count_of_FB_likes_weight + tempCol
tempCol <- ifelse(test = ((MainTable$Count_of_FB_likes <= 500) & (MainTable$Count_of_FB_likes > 300)), yes = weight * 0.75, no = 0)
Other_Variables$Count_of_FB_likes_weight <- Other_Variables$Count_of_FB_likes_weight + tempCol
tempCol <- ifelse(test = ((MainTable$Count_of_FB_likes > 500)), yes = weight * 1, no = 0)
Other_Variables$Count_of_FB_likes_weight <- Other_Variables$Count_of_FB_likes_weight + tempCol

rm(tempCol)
#------------------------------------------------------------------------------------#

#-------------------------------- Count of FB shares ---------------------------------#
MainTable$Count_of_FB_shares <- as.numeric(MainTable$Count_of_FB_shares)

# find the column name which has to be searched in Search_Element_Weight and then get the weight
lookupValue <- sub("^\\s+|\\s+$", "", gsub(x=colnames(Other_Variables)[13],pattern = "\\_+",replacement = " ", ignore.case = T))
weight <- as.numeric(Search_Element_Weight[Search_Element_Weight$ValueName == lookupValue,iterationNoColIndex])

Other_Variables$Count_of_FB_shares_weight <- 0

tempCol <- ifelse(test = (MainTable$Count_of_FB_shares <= 100), yes = weight * 0.25, no = 0)
Other_Variables$Count_of_FB_shares_weight <- Other_Variables$Count_of_FB_shares_weight + tempCol
tempCol <- ifelse(test = ((MainTable$Count_of_FB_shares <= 300) & (MainTable$Count_of_FB_shares > 100)), yes = weight * 0.5, no = 0)
Other_Variables$Count_of_FB_shares_weight <- Other_Variables$Count_of_FB_shares_weight + tempCol
tempCol <- ifelse(test = ((MainTable$Count_of_FB_shares <= 500) & (MainTable$Count_of_FB_shares > 300)), yes = weight * 0.75, no = 0)
Other_Variables$Count_of_FB_shares_weight <- Other_Variables$Count_of_FB_shares_weight + tempCol
tempCol <- ifelse(test = ((MainTable$Count_of_FB_shares > 500)), yes = weight * 1, no = 0)
Other_Variables$Count_of_FB_shares_weight <- Other_Variables$Count_of_FB_shares_weight + tempCol

rm(tempCol)
#------------------------------------------------------------------------------------#

#-------------------------------- Count of tweets ---------------------------------#
MainTable$Count_of_Tweets <- as.numeric(MainTable$Count_of_Tweets)

# find the column name which has to be searched in Search_Element_Weight and then get the weight
lookupValue <- sub("^\\s+|\\s+$", "", gsub(x=colnames(Other_Variables)[14],pattern = "\\_+",replacement = " ", ignore.case = T))
weight <- as.numeric(Search_Element_Weight[Search_Element_Weight$ValueName == lookupValue,iterationNoColIndex])

Other_Variables$Count_of_Tweets_weight <- 0

tempCol <- ifelse(test = (MainTable$Count_of_Tweets <= 100), yes = weight * 0.25, no = 0)
Other_Variables$Count_of_Tweets_weight <- Other_Variables$Count_of_Tweets_weight + tempCol
tempCol <- ifelse(test = ((MainTable$Count_of_Tweets <= 300) & (MainTable$Count_of_Tweets > 100)), yes = weight * 0.5, no = 0)
Other_Variables$Count_of_Tweets_weight <- Other_Variables$Count_of_Tweets_weight + tempCol
tempCol <- ifelse(test = ((MainTable$Count_of_Tweets <= 500) & (MainTable$Count_of_Tweets > 300)), yes = weight * 0.75, no = 0)
Other_Variables$Count_of_Tweets_weight <- Other_Variables$Count_of_Tweets_weight + tempCol
tempCol <- ifelse(test = ((MainTable$Count_of_Tweets > 500)), yes = weight * 1, no = 0)
Other_Variables$Count_of_Tweets_weight <- Other_Variables$Count_of_Tweets_weight + tempCol

rm(tempCol)
#------------------------------------------------------------------------------------#

#-------------------------------- Count of G+1 ---------------------------------#
MainTable$Count_of_G_1 <- as.numeric(MainTable$Count_of_G_1)

# find the column name which has to be searched in Search_Element_Weight and then get the weight
lookupValue <- sub("^\\s+|\\s+$", "", gsub(x=colnames(Other_Variables)[15],pattern = "\\_+",replacement = " ", ignore.case = T))
weight <- as.numeric(Search_Element_Weight[Search_Element_Weight$ValueName == lookupValue,iterationNoColIndex])

Other_Variables$Count_of_G_1_weight <- 0

tempCol <- ifelse(test = (MainTable$Count_of_G_1 <= 100), yes = weight * 0.25, no = 0)
Other_Variables$Count_of_G_1_weight <- Other_Variables$Count_of_G_1_weight + tempCol
tempCol <- ifelse(test = ((MainTable$Count_of_G_1 <= 300) & (MainTable$Count_of_G_1 > 100)), yes = weight * 0.5, no = 0)
Other_Variables$Count_of_G_1_weight <- Other_Variables$Count_of_G_1_weight + tempCol
tempCol <- ifelse(test = ((MainTable$Count_of_G_1 <= 500) & (MainTable$Count_of_G_1 > 300)), yes = weight * 0.75, no = 0)
Other_Variables$Count_of_G_1_weight <- Other_Variables$Count_of_G_1_weight + tempCol
tempCol <- ifelse(test = ((MainTable$Count_of_G_1 > 500)), yes = weight * 1, no = 0)
Other_Variables$Count_of_G_1_weight <- Other_Variables$Count_of_G_1_weight + tempCol

rm(tempCol)
#------------------------------------------------------------------------------------#

#-------------------------------- Score5 ---------------------------------#
Other_Variables$Score5 <- rowSums(x = Other_Variables[,c(11:15)], na.rm = T)*100
#------------------------------------------------------------------------------------#




#-------------------------------- Search Score ---------------------------------#
# Requires Relevance Table
load(file=paste0(getwd(),"/Datasets/R/Relevance_Processed.RData"))

temp1 <- grep(x=colnames(Relevance),pattern = "Score1")
temp2 <- grep(x=colnames(Relevance),pattern = "Score2")
temp3 <- grep(x=colnames(Other_Variables),pattern = "Score3")
temp4 <- grep(x=colnames(Other_Variables),pattern = "Score4")
temp5 <- grep(x=colnames(Other_Variables),pattern = "Score5")
temp <- Relevance[,c(temp1,temp2)]
temp <- cbind(temp,Other_Variables[,c(temp3,temp4,temp5)])
Other_Variables$Search_Score <- rowSums(x = temp,na.rm = T)
rm(temp1,temp2,temp3,temp4,temp5,temp)
#------------------------------------------------------------------------------------#


rm(tempColList,temp,colNoBook,Search_Element_Weight,i,j,iterationNoColIndex,lookupValue,maxTemp,minTemp,weight,temp,tempColList)


#-- Save to txt as '~' seperated file for importing into SQL Server
write.table(x=Other_Variables, file=paste0(getwd(),"/Datasets/txt/Other_Variables_Processed.txt"),row.names = FALSE,na = "", quote = FALSE,append = FALSE,eol = "\n", sep = "~")
#-- Save a copy in R Data
save(x=Other_Variables, file=paste0(getwd(),"/Datasets/R/Other_Variables_Processed.RData"))

#-- NOTE: Use txt file to import the data into SQL Server
