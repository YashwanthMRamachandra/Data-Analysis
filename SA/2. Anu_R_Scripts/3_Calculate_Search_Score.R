#--- Library
library("plyr")
library("sqldf")

#--- Load input files
load(file="/media/sf_mita_ubuntu/Sears/SKA/Data/ProcessedData/1_All_Review_Processed.RData")
Attribute_Map <- read.csv(file=paste(myPath,"Inputs/Attribute_Map.csv",sep=""))
Attributes <- read.csv(file=paste(myPath,"Inputs/Attributes.csv",sep=""))
Quarter_Weights<-read.csv(file =paste(myPath,"Inputs/Quarter_Weights.csv",sep=""))
#Review_Score_Data <- read.csv(file="/home/mono/Documents/Assortment/Top_25/Data/ProcessedData/2_Review_Score_Data.csv")

#--- Keep from where the month columns start in df Attribute_Map
monthColIndexForAttributeMap <- ncol(Attribute_Map) + 1
attributeIndexForAttributeMap <- ncol(Attribute_Map) - length(unique(Attributes$AttributeNames)) + 1

#--- Convert attribute names to lower case
Attributes$AttributeVals <- tolower(Attributes$AttributeVals)
for(i in c(attributeIndexForAttributeMap:ncol(Attribute_Map))){
  Attribute_Map[,i] <- tolower(Attribute_Map[,i])
}

#Attribute_Map$Brand<-gsub(pattern = "[[:punct:]]",replacement = "",x = Attribute_Map$Brand)
#Attribute_Map$Type<-gsub(pattern = "\\s",replacement = "",x = Attribute_Map$Type)


#--- Corect monthnames in column names of Attributes table
colnames(Attributes)[4:ncol(Attributes)] <- gsub(pattern = "\\.\\d*",replacement = "",x = colnames(Attributes)[4:ncol(Attributes)])

#--- Prepare template for Search Score
Attribute_Map$Mon1 <- as.numeric(0)
Attribute_Map$Mon2 <- as.numeric(0)
Attribute_Map$Mon3 <- as.numeric(0)
Attribute_Map$Mon4 <- as.numeric(0)
Attribute_Map$Mon5 <- as.numeric(0)
Attribute_Map$Mon6 <- as.numeric(0)
Attribute_Map$Mon7 <- as.numeric(0)
Attribute_Map$Mon8 <- as.numeric(0)
Attribute_Map$Mon9 <- as.numeric(0)
Attribute_Map$Mon10 <- as.numeric(0)
Attribute_Map$Mon11 <- as.numeric(0)
Attribute_Map$Mon12 <- as.numeric(0)
colnames(Attribute_Map)[c(monthColIndexForAttributeMap:ncol(Attribute_Map))] <- colnames(Attributes)[c(4:ncol(Attributes))]


#--- Format data
for(i in c(1:(ncol(Attributes)))){
  if(class(Attributes[,i]) != "integer"){
    Attributes[,i] <- as.character(Attributes[,i])
    Attributes[grepl(x = Attributes[,i],pattern = "n/a"),i] <- NA
  }
}
 


#--- Select all the unique Attributes
attributeList <- unique(Attributes$AttributeNames)

#change weight to numeric
Attributes$Weight<-as.numeric(Attributes$Weight)
#anurag#
#change all to numeric
for(i in c(3:ncol(Attributes))){
  Attributes[,i] <- as.numeric(Attributes[,i])
}

#--- Apply the local weight in Attributes table
for(i in c(4:ncol(Attributes))){
  Attributes[,i] <- Attributes[,i] / Attributes$Weight
}
rm(i)

#--- Calculate different attributes
# A temp table for holding two columns which will be added together
temp <- Attribute_Map[,c(1,2)]
colnames(temp)[1] <- "Col1"
colnames(temp)[2] <- "Col2"
temp$Col1 <- as.numeric(NA)
temp$Col2 <- as.numeric(NA)

#anurag#
#change to lowercase the Attribute value in Attribute map
#Attribute_Map$School<-tolower(Attribute_Map$School)

#--- Sum corresponding months' search values for all the attributes
for(attributeName in attributeList){  # attributeName <- attributeList[1]
  sqlStatement <- paste("SELECT b.* FROM Attribute_Map AS a LEFT OUTER JOIN Attributes AS b ON a.",attributeName," = b.AttributeVals", collapse = "", sep="")
  tempDf <- sqldf(sqlStatement)
  for(i in colnames(tempDf)[c(4:ncol(tempDf))]){  # i <- colnames(tempDf)[4]
    temp[,1] <- Attribute_Map[,c(i)]
    temp[,2] <- tempDf[,c(i)]
    Attribute_Map[,c(i)] <- rowSums(temp, na.rm = T)
  }
}
rm(temp)

#--- Calculate Quarter values
# System command
#system('echo "# Calculating quarter values..."; echo "45"')
#---------
tempDf <- Attribute_Map[,c((ncol(Attribute_Map)-11):ncol(Attribute_Map))]
Attribute_Map$Quarter_1 <- rowSums(tempDf[,c(1:3)],na.rm = T)
Attribute_Map$Quarter_1[Attribute_Map$Quarter_1 <= 0] <- 1

Attribute_Map$Quarter_2 <- rowSums(tempDf[,c(4:6)],na.rm = T)
Attribute_Map$Quarter_2[Attribute_Map$Quarter_2 <= 0] <- 1

Attribute_Map$Quarter_3 <- rowSums(tempDf[,c(7:9)],na.rm = T)
Attribute_Map$Quarter_3[Attribute_Map$Quarter_3 <= 0] <- 1

Attribute_Map$Quarter_4 <- rowSums(tempDf[,c(10:12)],na.rm = T)
Attribute_Map$Quarter_4[Attribute_Map$Quarter_4 <= 0] <- 1

#--- Calculate Growth
# System command
#system('echo "# Calculating growth..."; echo "50"')
#---------
Attribute_Map$Growth_Q1 <- as.numeric(0)
Attribute_Map$Growth_Q2 <- as.numeric(NA)
Attribute_Map$Growth_Q3 <- as.numeric(NA)
Attribute_Map$Growth_Q4 <- as.numeric(NA)
tempColNames <- grep(x = colnames(Attribute_Map),pattern = "growth", ignore.case = T)[c(2,3,4)]
for(i in tempColNames){   # i <- tempColNames[1]
  Attribute_Map[,i] <- Attribute_Map[,(i-4)] + Attribute_Map[,(i-5)]
  tempIndeces <- Attribute_Map[,i] != 2
  Attribute_Map[Attribute_Map[,i] == 2,i] <- 1
  if(sum(tempIndeces==TRUE) > 0){
    Attribute_Map[tempIndeces,i] <- Attribute_Map[tempIndeces,(i-4)] / Attribute_Map[tempIndeces,(i-5)]
  }
  Attribute_Map[is.nan(Attribute_Map[,i]),i] <- 0
  Attribute_Map[is.infinite(Attribute_Map[,i]),i] <- 0
  Attribute_Map[is.na(Attribute_Map[,i]),i] <- 0
}

#--- Calculate NEw Growth
Attribute_Map$New_Growth_Q1 <- as.numeric(0)
Attribute_Map$New_Growth_Q2 <- as.numeric(NA)
Attribute_Map$New_Growth_Q3 <- as.numeric(NA)
Attribute_Map$New_Growth_Q4 <- as.numeric(NA)

for(i in tempColNames){ # i <- tempColNames[1]
  tempColIndex <- i + 4
  Attribute_Map[Attribute_Map[,i] > 2, tempColIndex] <- 1.5
  Attribute_Map[Attribute_Map[,i] >= 5, tempColIndex] <- 2
  Attribute_Map[Attribute_Map[,i] >= 25, tempColIndex] <- 2.5
  Attribute_Map[Attribute_Map[,i] >= 50, tempColIndex] <- 3
  Attribute_Map[Attribute_Map[,i] <= 2, tempColIndex] <- Attribute_Map[Attribute_Map[,i] <= 2, i]
}

#--- Apply quarter weight
Attribute_Map$Final_Quarter_1_Score <- Attribute_Map$Quarter_1 * Quarter_Weights$Quarter_1
Attribute_Map$Final_Quarter_2_Score <- Attribute_Map$Quarter_2 * Attribute_Map$New_Growth_Q2 * Quarter_Weights$Quarter_2
Attribute_Map$Final_Quarter_3_Score <- Attribute_Map$Quarter_3 * Attribute_Map$New_Growth_Q3 * Quarter_Weights$Quarter_3
Attribute_Map$Final_Quarter_4_Score <- Attribute_Map$Quarter_4 * Attribute_Map$New_Growth_Q4 * Quarter_Weights$Quarter_4
rm(Quarter_Weights)

#--- Calculate search score
Attribute_Map$Search_Score <- rowSums(Attribute_Map[,c((ncol(Attribute_Map)-3):ncol(Attribute_Map))], na.rm = T)

#--- write -MMS
write.csv(Attribute_Map,file="Search_Score.csv")



#--- Calculate Net Search Score
Total_Searches <- sum(Attribute_Map[,c(monthColIndexForAttributeMap:(monthColIndexForAttributeMap + 11))])
Total_Reviews <- nrow(All_Review) #(for baseball bats 10900 so 
#--- write
#Total_Reviews<-217600
#Total_Searches<-521470
Search_Review_Ratio <- Total_Searches/Total_Reviews


Attribute_Map$Net_Search_Score<-Attribute_Map$Search_Score/Search_Review_Ratio
#tinkered to factor 0-1 search
#Attribute_Map$Net_Search_Score[Attribute_Map$Search_Score!=1] <- (Attribute_Map$Search_Score[Attribute_Map$Search_Score!=1]) / Search_Review_Ration

#only if there is no review and search score is to be computed then search review ratio is taken to be 1
#Attribute_Map$Net_Search_Score <- Attribute_Map$Search_Score / 1
write.csv(Attribute_Map,file = paste(myPath,"Search_Score.csv"))
#--- Save the search score data
# System command
#system('echo "# Saving data..."; echo "55"')
#---------
Search_Score_Data <- Attribute_Map
rm(Attribute_Map)
save(Search_Score_Data,file=paste(myPath,"ProcessedData/3_Search_Score_Data.RData",sep = ""))
#save(x=Search_Score_Data, file="/home/mono/Documents/Assortment/Top_25/Data/ProcessedData/3_Search_Score_Data.RData")

#--- Remove unwanted stuff
rm(list = ls()[!(ls() %in% c('Search_Score_Data','All_Review'))])
