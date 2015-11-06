

#load libraries----
library("plyr")
library("sqldf")


#function to set Date Format----
fn1_SetDateFormat<-function(){
  #--- Get names of the data files
  dataFilesPath<-paste(myPath,"Raw_Data/",sep ="")
  dataFiles <- list.files(path=dataFilesPath)
  
  #--- Prepare input file
  
  Retailer_Info <- read.delim(file=paste(myPath,"Inputs/Retailer_Info.csv",sep =""),sep = ",",header = T,row.names=NULL)
  #Retailer_Info<-Retailer_Info[0,]
  
  
  
  #--- Read all the retailer review files listed in the dataFiles
  for(retailerIndex in c(1:length(dataFiles))){  # retailerIndex <- 1
    #--- System notifications 
    #sysCommand <- paste('echo "', (15/length(dataFiles)),'"; echo "# Reading file ', dataFiles[retailerIndex],'..."',sep="",collapse = "")
    #system(sysCommand)
    
    #--- Read reviews into tempDf
    fileToRead <- paste(dataFilesPath,dataFiles[retailerIndex],sep="")
    tempDf <- read.delim(file=fileToRead,sep = ",",header = T,nrows = 100)
    
    #--- Remove unwanted columns
    tempDf <- tempDf[,c(1:32)]
    
    #--- Save colnames of the 1st file which will be applied to the remaining datasets
    
    colnames(tempDf) <- colnames(read.csv(file=paste(myPath,"Inputs/DataTemplate.csv",sep="")))
    
    
    #--- Take sample dates and write in the file
    Retailer_Info[retailerIndex,] <- c(as.character(dataFiles[retailerIndex]),as.character(fileToRead),"",as.character(tempDf$Review.Creation_Date[c(1,12,23,31,37,42,57,59,67,87)]),"")
  }
  
  write.csv(x=Retailer_Info,file=paste(myPath,"/Inputs/Retailer_Info.csv",sep =""),row.names=F)
  
  rm(list = ls()[!(ls() %in% c('myPath','dataFilesPath'))])
  
}


fn2_readAllData<-function(){
  Retailer_Info <- read.delim(file=paste(myPath,"Inputs/Retailer_Info.csv",sep=""),sep = ",",header = T)
  Retailer_Info <- Retailer_Info[,c(1,2,3)]
  for(i in c(1:ncol(Retailer_Info))){
    Retailer_Info[,i] <- as.character(Retailer_Info[,i])
  }
  
  #--- Get data template (column names)
  colNames <- colnames(read.csv(file=paste(myPath,"Inputs/DataTemplate.csv",sep="")))
  
  #--- Read all the retailer review files mention in the Retailer_Info
  for(retailerIndex in c(1:nrow(Retailer_Info))){  # retailerIndex <- 1
    #--- System notifications
    #sysCommand <- paste('echo "', (15/nrow(Retailer_Info)),'"; echo "# Reading file ', Retailer_Info[retailerIndex,1],'..."',sep="",collapse = "")
    #system(sysCommand)
    
    #--- Read reviews into tempDf
    tempDf <- read.delim(file=Retailer_Info$File_Path[retailerIndex],sep = ",",header = T)
    
    #--- Remove unwanted columns
    tempDf <- tempDf[,c(1:32)]
    
    #--- Save colnames of the 1st file which will be applied to the remaining datasets
    ##---colnames taken directly from data template csv rather than the first read csv file
    colnames(tempDf) <- colNames
    
    #--- Format Data
    for(i in c(1:(ncol(tempDf)))){
      #anurag
      #if(class(tempDf[,i]) == "factor"){
      #  tempDf[,i] <- as.character(tempDf[,i])
      #}
      tempDf[,i] <- as.character(tempDf[,i])
      
      tempDf[grepl(x = tempDf[,i],pattern = "n/a"),i] <- NA
    }
    tempDf$Review.Creation_Date <- as.Date(tempDf$Review.Creation_Date, format=Retailer_Info$Date_Format[retailerIndex])
    tempDf$Retailer <- tolower(gsub(x = tempDf$Retailer, pattern = "\\s", replacement = ""))
    
    
    #--- Merge all data in one df
    if(retailerIndex==1){
      All_Review <- tempDf
    } else{
      All_Review <- rbind(All_Review, tempDf)
    }
  }
  All_Review<<-All_Review
}

#removing NAs from All_Review data frame
fn3_removeNAs<-function(){
  x<-table(All_Review$Retailer)
  colIndex <- grep(x=colnames(All_Review),pattern = "Review.Creation_Date",ignore.case = T)
  All_Review <<- All_Review[complete.cases(All_Review[,colIndex]),]
  colIndex <- grep(x=colnames(All_Review),pattern = "Review.Rating_Score",ignore.case = T)
  All_Review <<- All_Review[complete.cases(All_Review[,colIndex]),]
  colIndex <- grep(x=colnames(All_Review),pattern = "Unique_Product_Identifier_Mainproduct",ignore.case = T)
  All_Review <<- All_Review[complete.cases(All_Review[,colIndex]),]
  
  #changing IDs to 'RETAILER-ID':
  
  All_Review$Unique_Product_Identifier_Mainproduct<<-paste(toupper(All_Review$Retailer),All_Review$Unique_Product_Identifier_Mainproduct,sep = "-")
  
  #--- Remove duplicates
  
  All_Review <<- All_Review[!duplicated(All_Review[c("Retailer","Unique_Product_Identifier_Mainproduct","Review.Header","Review.Creation_Date","Review.By","Review.Text")]),]
  
  #checking what got removed
  print("old")
  print(x)
  print("new")
  print(table(All_Review$Retailer))
  
}

#create duplicate check file
fn4_dupChk<-function(){
  
  temp <- count(All_Review, c("Unique_Product_Identifier_Mainproduct"))
  temp <- sqldf("SELECT a.*,b.Product_reviews FROM temp AS a INNER JOIN All_Review AS b ON a.Unique_Product_Identifier_Mainproduct=b.Unique_Product_Identifier_Mainproduct")
  temp <- unique(temp)
  temp$Product_reviews <- gsub(x = temp$Product_reviews,pattern = ",",replacement = "")
  temp$Product_reviews <- as.numeric(as.character(temp$Product_reviews))
  temp$ratio <- temp$freq / temp$Product_reviews
  write.csv(temp,file = "dupChk.csv")
  
}

fn5_1yrReview<-function(){
  #--- Import input files
  Quarter_Weights <<- read.csv(file=paste(myPath,"Inputs/Quarter_Weights.csv",sep=""))
  Star_Weights <<- read.csv(file=paste(myPath,"Inputs/Star_Weights.csv",sep=""))
  Mapped_Products <<- read.csv(file=paste(myPath,"Inputs/Mapped_Products.csv",sep=""))
  cols<-colnames(Mapped_Products)
  for (i in 1:length(Mapped_Products)){
    Mapped_Products[,i]<<-paste(toupper(cols[i]),toupper(Mapped_Products[,i]),sep = "-" )
  }
  retailerNames <<- tolower(colnames(Mapped_Products))
  
  All_Review$DayDiff <<- Epoch_Date - All_Review$Review.Creation_Date
  All_Review$DayDiff <<- as.numeric(All_Review$DayDiff)
  All_Review$Quarter <<- floor(All_Review$DayDiff / 91) + 1
  #--- Remove quarters > 4
  rowsToBeRemoved <- All_Review[,c(1:2)]
  colnames(rowsToBeRemoved)[1] <- "RowId"
  colnames(rowsToBeRemoved)[2] <- "ToBeRemoved"
  rowsToBeRemoved$RowId <- as.numeric(c(1:nrow(All_Review)))
  rowsToBeRemoved$ToBeRemoved <- as.logical(F)
  rowsToBeRemoved$ToBeRemoved[All_Review$Quarter > 4] <- T
  tempRowIndeces <- rowsToBeRemoved$RowId[rowsToBeRemoved$ToBeRemoved == T]
  All_Review <<- All_Review[-tempRowIndeces,]
  # Reorder Quarter number to get the correct quarter
  temp <- All_Review$Quarter
  temp[All_Review$Quarter == 1] <- 4
  temp[All_Review$Quarter == 2] <- 3
  temp[All_Review$Quarter == 3] <- 2
  temp[All_Review$Quarter == 4] <- 1
  All_Review$Quarter <<- temp
  
}

fn6_retailerNorm<-function(){
  Data_Toning_Values <<- read.csv(file=paste(myPath,"Inputs/RetailerNorm.csv",sep=""))
  Data_Toning_Values<<-Data_Toning_Values[0,]
  
  temp<-as.data.frame(cbind(Retailer=All_Review$Retailer,IDs=All_Review$Unique_Product_Identifier_Mainproduct))
  temp<-temp[!duplicated(temp[c("Retailer","IDs")]),]
  ReviewPerProduct<-table(All_Review$Retailer)/table(temp$Retailer)
  Norm<-max(ReviewPerProduct)/ReviewPerProduct
  Data_Toning_Values<<-as.data.frame(cbind(Variable=names(Norm),Value=Norm))
  
  write.csv(Data_Toning_Values,file=paste(myPath,"Inputs/RetailerNorm.csv",sep =""),row.names =F)
  readline(prompt = "edit the Retailer Norm file if required, press enter when done:")
  
  Data_Toning_Values <<- read.csv(file=paste(myPath,"Inputs/RetailerNorm.csv",sep=""))
  Data_Toning_Values$Variable <<- tolower(gsub(x = Data_Toning_Values$Variable, pattern = "\\s", replacement = ""))
  Data_Toning_Values$Value<<-as.numeric(as.character(Data_Toning_Values$Value))
  
}

fn7_calcRevScore<-function(){
  #--- Renaming "Review.Rating_Score" for ease
  colnames(All_Review)[23] <<- "Star"
  
  print(unique(All_Review$Quarter))
  
  
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
  
  
  # Calculate Review_Score------ 
  Mapped_Products$Review_Score <- Mapped_Products$Final_Quarter_1_Score + Mapped_Products$Final_Quarter_2_Score + Mapped_Products$Final_Quarter_3_Score + Mapped_Products$Final_Quarter_4_Score
  
  #--- Save the data
  # System command
  #system('echo "# Saving data..."; echo "40"')
  # ---------
  Review_Rating_Data <<- Mapped_Products
  Review_Rating_Data <- Mapped_Products
  save(Review_Rating_Data,file=paste(myPath,"ProcessedData/2_Review_Rating_Data.RData",sep = ""))
  write.csv(get("Review_Rating_Data",envir=parent.frame()),"ReviewScore.csv")
  
}