#-------------------------------------------------------------
#-- This script imports the URLs of images and image related data
#-- Data being taken into account is MainTable_Processed.RData (for Data_Id)
#-- Images should have 54+1(data id) no of columns
#-- Pick up data from: Image 1 to All reviews
#-- "Image 1" to "Image_Bucket"
#-------------------------------------------------------------
# This script has been reviewed and works fine!

#-- load raw data
Images <- read.csv(file=paste0(getwd(), "/Datasets/Raw data for R/Images_for_R.csv"))

#-- Create Data_Id column
Images$Data_Id <- NA

#-- Convert all columns into character except for Data_Id
Images$Data_Id <- as.numeric(as.character(Images$Data_Id))
for(i in c(1:(ncol(Images)-1))){
  Images[,i] <- as.character(Images[,i])
}

#-- Convert n/a to NA
for(i in c(1:(ncol(Images)-1))){
  Images[grepl(x = Images[,i],pattern = "n/a"),i] <- NA
  Images[grepl(x = Images[,i],pattern = "#REF!"),i] <- NA
}

#-- Shift Data_Id to the 1st position
colnames(Images)[2:ncol(Images)] <- colnames(Images)[1:ncol(Images)-1]
colnames(Images)[1] <- "Data_Id"
# Also shift the data
Images[,c(2:ncol(Images))] <- Images[,c(1:ncol(Images)-1)]
Images$Data_Id <- NA


#--- Resize Images table so that it has same length as MainTable table
# Save Images in temp
temp <- Images
# Copy MainTable table into Images with same no of col as it had so that it has the same no of rows
Images <- MainTable[,c(1:ncol(temp))]
# Format the columns according to the format it previously had
for(i in 1:ncol(temp)){
  colnames(Images)[i] <- colnames(temp)[i]
  Images[,i] <- NA
  Images[c(1:nrow(temp)),i] <- temp[c(1:nrow(temp)),i]
  
  if(class(temp[,i]) == "integer"){
    Images[,i] <- as.integer(Images[,i])
  }
  else if(class(temp[,i]) == "character"){
    Images[,i] <- as.character(Images[,i])
  }
  if(class(temp[,i]) == "numeric"){
    Images[,i] <- as.numeric(Images[,i])
  }
  else {
    Images[,i] <- as.factor(Images[,i])
  }
}
rm(temp,i)


#-- Apply Data Id in pivot also so that mapping can be done
Images$Data_Id <- MainTable$Data_Id

#-- (No formula yet)
#-- All_reviews
#-- Popular_Keywords_Merged
#-- H_Tag
#-- Product_ID
#-- Web_Class
#-- Image_Bucket




#-- Save to txt as '~' seperated file for importing into SQL Server
write.table(x=Images, file=paste0(getwd(), "/Datasets/txt/Images_Processed.txt"),row.names = FALSE,na = "", quote = FALSE,append = FALSE,eol = "\n", sep = "~")
#-- Save a copy in R Data
save(x=Images, file=paste0(getwd(), "/Datasets/R/Images_Processed.RData"))

#-- NOTE: Use txt file to import the data into SQL Server
