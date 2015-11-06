#code2
#--- Library
library("plyr")
library("sqldf")

#--- Import input files
# System command
#system('echo "# Loading data..."; echo "20"')
# Load All---------

load(file=paste(myPath,"ProcessedData/1_All_Review_Processed.RData",sep=""))
Quarter_Weights <- read.csv(file=paste(myPath,"Inputs/Quarter_Weights.csv",sep=""))
Star_Weights <- read.csv(file=paste(myPath,"Inputs/Star_Weights.csv",sep=""))
Mapped_Products <- read.csv(file=paste(myPath,"Inputs/Mapped_Products.csv",sep=""))

#changing Mapped Product Ids to 'RETAILER-ID' uppercase:
cols<-colnames(Mapped_Products)
for (i in 1:length(Mapped_Products)){
  Mapped_Products[,i]<-paste(toupper(cols[i]),toupper(Mapped_Products[,i]),sep = "-" )
}

retailerNames <- tolower(colnames(Mapped_Products))

# take 1 yr----
#Take Current Date as 12th month period end
Epoch_Date <- Sys.Date()
#or take max review date as 12th month period end
Epoch_Date <- as.Date(max(All_Review$Review.Creation_Date))
#All_Review<-All_Review[!All_Review$Review.Creation_Date>Sys.Date(),]
#Epoch_Date <- as.Date("31 Oct, 2014", format="%d %b, %Y")

#to check which retailer is having wrong dates
sapply(unique(All_Review$Retailer),function(x){print(as.Date(max(All_Review$Review.Creation_Date[All_Review$Retailer==x])))})

#review backup
All_Review_bak<-All_Review

#check number of reviews for which dates are wrong
sum(All_Review$Review.Creation_Date>Sys.Date())

#if dates are wrong for very few reviews
All_Review<-All_Review[!All_Review$Review.Creation_Date>Sys.Date(),]
Epoch_Date <- Sys.Date()
#Check the Min date in the All_Review_Data
min(All_Review$Review.Creation_Date)


#--- Calculate quarter
# System command
#system('echo "# Calculating quarters..."; echo "25"')
#---------


#getting review for last one year
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

#anurag
#calculate data toning values

#write.csv(cbind(All_Review$Retailer,All_Review$Unique_Product_Identifier_Mainproduct),paste(myPath,"/All_ReviewsBIG.csv",sep=""))
#write.csv(All_Review,"All_Reviews.csv")

Data_Toning_Values <- read.csv(file=paste(myPath,"Inputs/Data_Toning_Vals.csv",sep=""))
Data_Toning_Values<-Data_Toning_Values[0,]

temp<-as.data.frame(cbind(Retailer=All_Review$Retailer,IDs=All_Review$Unique_Product_Identifier_Mainproduct))
temp<-temp[!duplicated(temp[c("Retailer","IDs")]),]
ReviewPerProduct<-table(All_Review$Retailer)/table(temp$Retailer)
Norm<-max(ReviewPerProduct)/ReviewPerProduct
Data_Toning_Values<-as.data.frame(cbind(Variable=names(Norm),Value=Norm))

write.csv(Data_Toning_Values,file=paste(myPath,"Inputs/Data_Toning_Vals.csv",sep =""),row.names =F)
#edit the Retailer Norm file if required

Data_Toning_Values <- read.csv(file=paste(myPath,"Inputs/Data_Toning_Vals.csv",sep=""))
Data_Toning_Values$Variable <- tolower(gsub(x = Data_Toning_Values$Variable, pattern = "\\s", replacement = ""))
Data_Toning_Values$Value<-as.numeric(as.character(Data_Toning_Values$Value))
#find review per product for different retailer and then calculate data toning values

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
#system('echo "# Implementing star weight..."; echo "27"')
#---------
for(i in c(1:ncol(Star_Weights))){
  #Rating_Summary$Rating_Score[Rating_Summary$Star==i] <- Rating_Summary$Star[Rating_Summary$Star==i] * Star_Weights[1,i] * Rating_Summary$Review_Count[Rating_Summary$Star==i]
  Rating_Summary$Rating_Score[Rating_Summary$Star==i] <- Star_Weights[1,i] * Rating_Summary$Review_Count[Rating_Summary$Star==i]
}
#rm(Star_Weights)

# Implementing Retailer (Toning) Data
# System command
#system('echo "# Implementing retailer weight..."; echo "30"')
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
Total_Quarter_Scores <- Mapped_Products[,c(1:3)]  # Nothing to do with mapped_products df. Just creating a df
#Total_Quarter_Scores[,4]<-NA
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
#system('echo "# Calculating quarter growth..."; echo "32"')
#---------
Mapped_Products$Growth_Q1 <- as.numeric(0)
Mapped_Products$Growth_Q2 <- as.numeric(NA)
Mapped_Products$Growth_Q3 <- as.numeric(NA)
Mapped_Products$Growth_Q4 <- as.numeric(NA)
growthIndeces <- grep(x = colnames(Mapped_Products),pattern = "growth_q\\d", ignore.case = T)[-1]
for(i in growthIndeces){ # i<-12
  tempColIndex <- i - 4
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
#system('echo "# Calculating review score..."; echo "35"')
#---------
Mapped_Products$Final_Quarter_1_Score <- Mapped_Products$Quarter_1_Score * Quarter_Weights$Quarter_1
Mapped_Products$Final_Quarter_2_Score <- Mapped_Products$Quarter_2_Score * Mapped_Products$New_Growth_Q2 * Quarter_Weights$Quarter_2
Mapped_Products$Final_Quarter_3_Score <- Mapped_Products$Quarter_3_Score * Mapped_Products$New_Growth_Q3 * Quarter_Weights$Quarter_3
Mapped_Products$Final_Quarter_4_Score <- Mapped_Products$Quarter_4_Score * Mapped_Products$New_Growth_Q4 * Quarter_Weights$Quarter_4
rm(Quarter_Weights)

# Calculate Review_Score------ 
Mapped_Products$Review_Score <- Mapped_Products$Final_Quarter_1_Score + Mapped_Products$Final_Quarter_2_Score + Mapped_Products$Final_Quarter_3_Score + Mapped_Products$Final_Quarter_4_Score

#--- Save the data
# System command
#system('echo "# Saving data..."; echo "40"')
# ---------
Review_Rating_Data <- Mapped_Products
rm(Mapped_Products)
save(Review_Rating_Data,file=paste(myPath,"ProcessedData/2_Review_Rating_Data.RData",sep = ""))
write.csv(Review_Rating_Data,"ReviewScore.csv")

#Get Data for Sheet for 1yr----
ProductInfo<-All_Review[c(3,9,10,11,12,14,32)]

ProductInfo<- ProductInfo[!duplicated(All_Review[colnames(All_Review)[c(3,9,10,11,12,14,32)]]),]
write.csv(ProductInfo,"ProductInfoNew.csv",row.names = F)

# All Product Info----
ProductInfoAll<-All_Review_bak[c(3,9,10,11,12,14,32)]

ProductInfoAll<- ProductInfoAll[!duplicated(All_Review_bak[colnames(All_Review_bak)[c(3,9,10,11,12,14,32)]]),]
write.csv(ProductInfoAll,"ProductInfoAll.csv",row.names = F)

#check rating summary ----
write.csv(Rating_Summary,"Rating_Summary.csv")
#Remove unwanted stuff ----

rm(list = ls()[!(ls() %in% c('Review_Rating_Data','All_Review'))])
