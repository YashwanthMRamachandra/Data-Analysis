#-------------------------------------------------------------
#-- This script generates the values for table Relevance based on various rules/formulae
#-- Data being taken into account is MainTable_Processed.RData & Search_Element_Weight
#-- Relevance table should have 58+1(DataId) columns
#-- Pick up from raw data: "Relevancy of search phrase1 in meta Title" to "Score2"
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

#-- Load the Raw Data
Relevance <- read.csv(file=paste0(getwd(),"/Datasets/Raw data for R/Relevance_for_R.csv"))

#-- Create Data_Id column
Relevance$Data_Id <- NA

#-- Convert all columns into numeric
for(i in 1:ncol(Relevance)){
  Relevance[,i] <- as.numeric(as.character(Relevance[,i]))
}

#-- Shift Data_Id to the 1st position
colnames(Relevance)[2:ncol(Relevance)] <- colnames(Relevance)[1:ncol(Relevance)-1]
colnames(Relevance)[1] <- "Data_Id"
# Also shift the data
Relevance[,c(2:ncol(Relevance))] <- Relevance[,c(1:ncol(Relevance)-1)]
Relevance$Data_Id <- NA



#--- Resize Relevance table so that it has same length as MainTable table
# Save Relevance in temp
temp <- Relevance
# Copy MainTable table into Relevance with same no of col as it had so that it has the same no of rows
Relevance <- MainTable[,c(1:ncol(temp))]
# Format the columns according to the format it previously had
for(i in 1:ncol(temp)){
  colnames(Relevance)[i] <- colnames(temp)[i]
  
  Relevance[,i] <- NA
  
  if(class(temp[,i]) == "integer"){
    Relevance[,i] <- as.integer(Relevance[,i])
  }
  else if(class(temp[,i]) == "character"){
    Relevance[,i] <- as.character(Relevance[,i])
  }
  if(class(temp[,i]) == "numeric"){
    Relevance[,i] <- as.numeric(Relevance[,i])
  }
  else {
    Relevance[,i] <- as.factor(Relevance[,i])
  }
}
Relevance[c(1:nrow(temp)),] <- temp[c(1:nrow(temp)),]
rm(temp,i)


#-- Apply Data Id in pivot also so that mapping can be done
Relevance$Data_Id <- MainTable$Data_Id


#--------------------- FORMULA APPLICATION ----------------------------#
#   We need to find average across mutliple sets of columns (5 columns a time, 6 sets)_ Therefore we are
#   maintaining a lookup table which has the column indeces of the 6 sets_ 'startCol' signifies where
#   the first column is, 'endCol' the last column_ 'avgCol' is where the average of the columns are to be saved
#   This is done to avaoid repeating the codes_ If in future more sets of columns are added, just add another 
#   row to this lookup table

colNoBook <- Relevance[c(1:8),c(1:5)]
colnames(colNoBook)[1] <- "startCol"
colnames(colNoBook)[2] <- "endCol"
colnames(colNoBook)[3] <- "avgCol"
colnames(colNoBook)[4] <- "weightedCol"
colnames(colNoBook)[5] <- "groupId"
colNoBook[1,] <- c(2,6,7,8,1)
colNoBook[2,] <- c(9,13,14,15,1)
colNoBook[3,] <- c(16,20,21,22,1)
colNoBook[4,] <- c(23,27,28,29,1)
colNoBook[5,] <- c(30,34,35,36,1)
colNoBook[6,] <- c(37,41,42,43,1)
colNoBook[7,] <- c(45,49,50,51,2)
colNoBook[8,] <- c(52,56,57,58,2)



#----------------------------------------------------------------------#
#     1_ Find average of Relevancy_of_search_phrase[1:5]_in_meta_Title_ Ignore 0
#       For ignoring 0, we need to convert 0 into NA
#       Therefore, make a copy of the data
for(x in c(1:nrow(colNoBook))){   # x <- 1
  temp <- Relevance[,c(colNoBook$startCol[x]:colNoBook$endCol[x])]
  
  #       Convert all NA into 0 and then all 0 into na. Else it doesnt work for some weird reason
  for(i in 1:ncol(temp)){
    temp[is.na(temp[,i]),c(i)] <- 0
  }
  for(i in c(1:ncol(temp))){
    temp[temp[,i] == 0,c(i)] <- NA
  }
  #       Find the average of each row
  tempAvg <- rowMeans(temp[,c(1:5)], na.rm = T)
  #       Assign the value to proper column
  Relevance[,c(colNoBook$avgCol[x])] <- tempAvg
  rm(tempAvg,temp,i)
}
rm(x)
#----------------------------------------------------------------------#


#----------------------------------------------------------------------#
#     2_ 1st load Search_Element_Weight table
#       weight = weight of that particular column,
#       avg = avgerage that we calculated in 1st column

#       therefore, weightedAvg = weight * ((avg - min(avg))/max(avg))
#       but, Relevancy_of_search_phrase_in_meta_Title (1st row of colNoBook is special)
#       its formula is weight * avg

for(x in c(1:nrow(colNoBook))){   # x <- 1
  # get the column index for the current column name
  weightedAvgColIndex <- grep(paste("^",colnames(Relevance)[colNoBook$weightedCol[x]],"$",sep = ""), colnames(Relevance))
  avgColIndex <- weightedAvgColIndex - 1
  
  # get weight from Search_Element_Weight according to the column name
  lookupValue <- sub("^\\s+|\\s+$", "", gsub(x=colnames(Relevance)[weightedAvgColIndex],pattern = "\\_+",replacement = " "))
  weight <- Search_Element_Weight[Search_Element_Weight$ValueName == lookupValue,iterationNoColIndex]
  
  
  if(sum(x == c(1,2,3))){
    Relevance[,weightedAvgColIndex] <- 0
    
    tempCol <- ifelse(test = Relevance[,avgColIndex] <= 0.05, yes = weight * 0.25, no = 0)
    Relevance[,weightedAvgColIndex] <- Relevance[,weightedAvgColIndex] + tempCol
    tempCol <- ifelse(test = ((Relevance[,avgColIndex] <= 0.15) & (Relevance[,avgColIndex] > 0.05)), yes = weight * 0.5, no = 0)
    Relevance[,weightedAvgColIndex] <- Relevance[,weightedAvgColIndex] + tempCol
    tempCol <- ifelse(test = ((Relevance[,avgColIndex] <= 0.25) & (Relevance[,avgColIndex] > 0.15)), yes = weight * 0.75, no = 0)
    Relevance[,weightedAvgColIndex] <- Relevance[,weightedAvgColIndex] + tempCol
    tempCol <- ifelse(test = ((Relevance[,avgColIndex] > 0.25)), yes = weight * 1, no = 0)
    Relevance[,weightedAvgColIndex] <- Relevance[,weightedAvgColIndex] + tempCol
    
    Relevance[is.na(Relevance[,weightedAvgColIndex]),weightedAvgColIndex] <- 0
    rm(tempCol)
  }
  else if(sum(x == c(4,6,7,8))){
    Relevance[,weightedAvgColIndex] <- 0
    
    tempCol <- ifelse(test = Relevance[,avgColIndex] <= 0.05, yes = weight * 0.25, no = 0)
    Relevance[,weightedAvgColIndex] <- Relevance[,weightedAvgColIndex] + tempCol
    tempCol <- ifelse(test = ((Relevance[,avgColIndex] <= 0.10) & (Relevance[,avgColIndex] > 0.05)), yes = weight * 0.5, no = 0)
    Relevance[,weightedAvgColIndex] <- Relevance[,weightedAvgColIndex] + tempCol
    tempCol <- ifelse(test = ((Relevance[,avgColIndex] <= 0.25) & (Relevance[,avgColIndex] > 0.10)), yes = weight * 0.75, no = 0)
    Relevance[,weightedAvgColIndex] <- Relevance[,weightedAvgColIndex] + tempCol
    tempCol <- ifelse(test = ((Relevance[,avgColIndex] > 0.25)), yes = weight * 1, no = 0)
    Relevance[,weightedAvgColIndex] <- Relevance[,weightedAvgColIndex] + tempCol
    
    Relevance[is.na(Relevance[,weightedAvgColIndex]),weightedAvgColIndex] <- 0
    rm(tempCol)
  }
  else if(sum(x == c(5))){
    Relevance[,weightedAvgColIndex] <- 0
    
    tempCol <- ifelse(test = Relevance[,avgColIndex] <= 0.01, yes = weight * 0.25, no = 0)
    Relevance[,weightedAvgColIndex] <- Relevance[,weightedAvgColIndex] + tempCol
    tempCol <- ifelse(test = ((Relevance[,avgColIndex] <= 0.05) & (Relevance[,avgColIndex] > 0.01)), yes = weight * 0.5, no = 0)
    Relevance[,weightedAvgColIndex] <- Relevance[,weightedAvgColIndex] + tempCol
    tempCol <- ifelse(test = ((Relevance[,avgColIndex] <= 0.10) & (Relevance[,avgColIndex] > 0.05)), yes = weight * 0.75, no = 0)
    Relevance[,weightedAvgColIndex] <- Relevance[,weightedAvgColIndex] + tempCol
    tempCol <- ifelse(test = ((Relevance[,avgColIndex] > 0.10)), yes = weight * 1, no = 0)
    Relevance[,weightedAvgColIndex] <- Relevance[,weightedAvgColIndex] + tempCol
    
    Relevance[is.na(Relevance[,weightedAvgColIndex]),weightedAvgColIndex] <- 0
    rm(tempCol)
  }
}
#----------------------------------------------------------------------#

#----------------------------------------------------------------------#
#     3_ Calculate Score_1
Relevance$Score1 <- 0
Relevance$Score1 <- rowSums(x=Relevance[,c(8,15,22,29,36,43)], na.rm = T)
Relevance$Score1 <- Relevance$Score1 * 100

#     3_ Calculate Score_2
Relevance$Score2 <- 0
Relevance$Score2 <- rowSums(x=Relevance[,c(51,58)], na.rm=T)
Relevance$Score2 <- Relevance$Score2 * 100

#----------------------------------------------------------------------#

#--- Convert all NaN to NA
for(i in 1:ncol(Relevance)){
  Relevance[is.nan(Relevance[,c(i)]),c(i)] <- NA
}

rm(i, avgColIndex,colNoBook,lookupValue,weight,weightedAvgColIndex,x,iterationNoColIndex)


#-- Save to txt as '~' seperated file for importing into SQL Server
write.table(x=Relevance, file=paste0(getwd(),"/Datasets/txt/Relevance_Processed.txt"),row.names = FALSE,na = "", quote = FALSE,append = FALSE,eol = "\n", sep = "~")
#-- Save a copy in R Data
save(x=Relevance, file=paste0(getwd(),"/Datasets/R/Relevance_Processed.RData"))

#-- NOTE: Use txt file to import the data into SQL Server
