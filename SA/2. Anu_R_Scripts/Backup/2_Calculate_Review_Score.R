#--- Library
library("plyr")
library("sqldf")

#--- Import input files
# System command
system('echo "# Loading data..."; echo "20"')
#---------

load(file="/home/mono/Documents/Assortment/Top 25/Data/ProcessedData/1_All_Review_Processed.RData")
Quarter_Weights <- read.csv(file="/home/mono/Documents/Assortment/Top 25/Data/Inputs/Quarter_Weights.csv")
Star_Weights <- read.csv(file="/home/mono/Documents/Assortment/Top 25/Data/Inputs/Star_Weights.csv")
Data_Toning_Values <- read.csv(file="/home/mono/Documents/Assortment/Top 25/Data/Inputs/Data_Toning_Vals.csv")
Data_Toning_Values$Variable <- tolower(gsub(x = Data_Toning_Values$Variable, pattern = "\\s", replacement = ""))
Mapped_Products <- read.csv(file="/home/mono/Documents/Assortment/Top 25/Data/Inputs/Mapped_Products.csv")


retailerNames <- tolower(colnames(Mapped_Products))


Epoch_Date <- as.Date(max(All_Review$Review.Creation_Date))
#Epoch_Date <- as.Date("31 Oct, 2014", format="%d %b, %Y")
#--- Calculate quarter
# System command
system('echo "# Calculating quarters..."; echo "25"')
#---------
All_Review$DayDiff <- Epoch_Date - All_Review$Review.Creation_Date
All_Review$DayDiff <- as.numeric(All_Review$DayDiff)
All_Review$Quarter <- floor(All_Review$DayDiff / 91) + 1
#--- Remove quarters > 4
rowsToBeRemoved <- All_Review[,c(1:2)]
colnames(rowsToBeRemoved)[1] <- "RowId"
colnames(rowsToBeRemoved)[2] <- "ToBeRemoved"
rowsToBeRemoved$RowId <- as.numeric(c(1:nrow(All_Review)))
rowsToBeRemoved$ToBeRemoved <- as.logical(F)
rowsToBeRemoved$ToBeRemoved[All_Review$Quarter > 4] <- T
tempRowIndeces <- rowsToBeRemoved$RowId[rowsToBeRemoved$ToBeRemoved == T]
All_Review <- All_Review[-tempRowIndeces,]
# Reorder Quarter number to get the correct quarter
temp <- All_Review$Quarter
temp[All_Review$Quarter == 1] <- 4
temp[All_Review$Quarter == 2] <- 3
temp[All_Review$Quarter == 3] <- 2
temp[All_Review$Quarter == 4] <- 1
All_Review$Quarter <- temp
rm(temp)




#--- Renaming "Review.Rating_Score" for ease
colnames(All_Review)[23] <- "Star"

unique(All_Review$Quarter)


#--- Counting number of reviews
Rating_Summary <- count(All_Review, c("Retailer","Unique_Product_Identifier_Mainproduct","Quarter","Star"))
colnames(Rating_Summary)[5] <- "Review_Count"


#--- Preparing Rating Weight
Rating_Summary$Rating_Score <- as.numeric(NA)

#--- Implementing Star weight
# System command
system('echo "# Implementing star weight..."; echo "27"')
#---------
for(i in c(1:ncol(Star_Weights))){
  #Rating_Summary$Rating_Score[Rating_Summary$Star==i] <- Rating_Summary$Star[Rating_Summary$Star==i] * Star_Weights[1,i] * Rating_Summary$Review_Count[Rating_Summary$Star==i]
  Rating_Summary$Rating_Score[Rating_Summary$Star==i] <- Star_Weights[1,i] * Rating_Summary$Review_Count[Rating_Summary$Star==i]
}
#rm(Star_Weights)

# Implementing Retailer (Toning) Data
# System command
system('echo "# Implementing retailer weight..."; echo "30"')
#---------
for(i in c(1:nrow(Data_Toning_Values))){
  Rating_Summary$Rating_Score[Rating_Summary$Retailer == Data_Toning_Values$Variable[i]] <- Rating_Summary$Rating_Score[Rating_Summary$Retailer == Data_Toning_Values$Variable[i]] * Data_Toning_Values$Value[i]
}
#rm(Data_Toning_Values)

#--- Aggregating Rating_Score based on Retailer, UniqueId, Quarter
Quarter_Rating_Summary <- aggregate(Rating_Summary$Rating_Score, by=list(Rating_Summary$Retailer, Rating_Summary$Unique_Product_Identifier_Mainproduct, Rating_Summary$Quarter), FUN=sum)
colnames(Quarter_Rating_Summary)[1] <- "Retailer"
colnames(Quarter_Rating_Summary)[2] <- "Unique_Product_Identifier_Mainproduct"
colnames(Quarter_Rating_Summary)[3] <- "Quarter"
colnames(Quarter_Rating_Summary)[4] <- "Rating_Score"



#--- Make a df to store Total_Quarter_Scores
Total_Quarter_Scores <- Mapped_Products[,c(1:4)]
for(i in c(1:4)){
  Total_Quarter_Scores[,i] <- NA
  Total_Quarter_Scores[,i] <- as.numeric(Total_Quarter_Scores[,i])
  colnames(Total_Quarter_Scores)[i] <- paste(c("Quarter_",i,"_Score"), collapse = "")
}
#--- Calcuate Quarter_Scores for all the retailers
#retailerNames <- as.character(unique(All_Review$Retailer))
for(quarterVal in c(1:4)){  # quarterVal <- 1
  for(retailerName in retailerNames){   # retailerName <- retailerNames[1]
    sqlStatement = paste(c("SELECT a.*,b.Rating_Score AS ",retailerName,"_",quarterVal,"_Score FROM Mapped_Products AS a LEFT OUTER JOIN Quarter_Rating_Summary AS b ON a.",retailerName,"=b.Unique_Product_Identifier_Mainproduct AND b.Quarter = ",quarterVal),collapse="")
    Mapped_Products <- sqldf(sqlStatement)
  }
  Total_Quarter_Scores[,quarterVal] <- rowSums(Mapped_Products[,c((length(retailerNames)+1):ncol(Mapped_Products))],na.rm = T)
  Mapped_Products <- Mapped_Products[,c(1:length(retailerNames))]
}
Mapped_Products <- cbind(Mapped_Products,Total_Quarter_Scores)

#--- Revaluate Quarter_Scores to 0 if it is smaller than 1
for(i in c((length(retailerNames)+1):ncol(Mapped_Products))){
  Mapped_Products[Mapped_Products[,i] <= 0, i] <- 1
}

#################################################################################################

#--- Calculate quarter growth
# System command
system('echo "# Calculating quarter growth..."; echo "32"')
#---------
Mapped_Products$Growth_Q1 <- as.numeric(0)
Mapped_Products$Growth_Q2 <- as.numeric(NA)
Mapped_Products$Growth_Q3 <- as.numeric(NA)
Mapped_Products$Growth_Q4 <- as.numeric(NA)
growthIndeces <- grep(x = colnames(Mapped_Products),pattern = "growth_q\\d", ignore.case = T)[-1]
for(i in growthIndeces){ # i<-11
  tempColIndex <- i - length(retailerNames) + 1
  Mapped_Products[,i] <- as.numeric(Mapped_Products[,tempColIndex] + Mapped_Products[,(tempColIndex-1)])
  tempIndeces <- Mapped_Products[,i] != 2
  Mapped_Products[Mapped_Products[,i]==2,i] <- 1
  if(sum(tempIndeces==TRUE) > 0){
    Mapped_Products[tempIndeces,i] <- Mapped_Products[tempIndeces,tempColIndex]/Mapped_Products[tempIndeces,(tempColIndex-1)]
  }
  Mapped_Products[is.nan(Mapped_Products[,i]),i] <- 0
  Mapped_Products[is.infinite(Mapped_Products[,i]),i] <- 0
  Mapped_Products[is.na(Mapped_Products[,i]),i] <- 0
}

#rm(quarterVal,retailerName,retailerNames,sqlStatement,tempIndeces,tempColIndex,i)

#--- Calculate new growth (bucketing the growth)
Mapped_Products$New_Growth_Q1 <- as.numeric(0)
Mapped_Products$New_Growth_Q2 <- as.numeric(NA)
Mapped_Products$New_Growth_Q3 <- as.numeric(NA)
Mapped_Products$New_Growth_Q4 <- as.numeric(NA)

for(i in growthIndeces){ #i <- 11
  tempColIndex <- i + 4
  Mapped_Products[Mapped_Products[,i] > 2, tempColIndex] <- 1.5
  Mapped_Products[Mapped_Products[,i] >= 5, tempColIndex] <- 2
  Mapped_Products[Mapped_Products[,i] >= 25, tempColIndex] <- 2.5
  Mapped_Products[Mapped_Products[,i] >= 50, tempColIndex] <- 3
  Mapped_Products[Mapped_Products[,i] <= 2, tempColIndex] <- Mapped_Products[Mapped_Products[,i] <= 2, i]
}


#--- Calculate Final Quarter Score also implementing Quarter_Weights
# System command
system('echo "# Calculating review score..."; echo "35"')
#---------
Mapped_Products$Final_Quarter_1_Score <- Mapped_Products$Quarter_1_Score * Quarter_Weights$Quarter_1
Mapped_Products$Final_Quarter_2_Score <- Mapped_Products$Quarter_2_Score * Mapped_Products$New_Growth_Q2 * Quarter_Weights$Quarter_2
Mapped_Products$Final_Quarter_3_Score <- Mapped_Products$Quarter_3_Score * Mapped_Products$New_Growth_Q3 * Quarter_Weights$Quarter_3
Mapped_Products$Final_Quarter_4_Score <- Mapped_Products$Quarter_4_Score * Mapped_Products$New_Growth_Q4 * Quarter_Weights$Quarter_4
rm(Quarter_Weights)

#--- Calculate Review_Score
Mapped_Products$Review_Score <- Mapped_Products$Final_Quarter_1_Score + Mapped_Products$Final_Quarter_2_Score + Mapped_Products$Final_Quarter_3_Score + Mapped_Products$Final_Quarter_4_Score

#--- Save the data
# System command
system('echo "# Saving data..."; echo "40"')
#---------
Review_Rating_Data <- Mapped_Products
rm(Mapped_Products)
save(Review_Rating_Data,file="/home/mono/Documents/Assortment/Top 25/Data/ProcessedData/2_Review_Rating_Data.RData")

#--- Remove unwanted stuff
rm(list = ls()[!(ls() %in% c('Review_Rating_Data','All_Review'))])
