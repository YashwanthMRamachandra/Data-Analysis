
#--- Import data
load(file="/home/mono/Documents/Assortment/Top_25/Data/ProcessedData/2_Review_Rating_Data.RData")
load(file="/home/mono/Documents/Assortment/Top_25/Data/ProcessedData/3_Search_Score_Data.RData")
load(file="/home/mono/Documents/Assortment/Top_25/Data/ProcessedData/4_Social_Signal_Data.RData")
Mapped_Products <- read.csv(file="/home/mono/Documents/Assortment/Top_25/Data/Inputs/Mapped_Products.csv")
Score_Weights <- read.csv(file="/home/mono/Documents/Assortment/Top_25/Data/Inputs/Score_Weights.csv")


#--- Final DDI
DDI_Data <- Review_Rating_Data[,c(1,2,3)]
colnames(DDI_Data) <- c("Review_Rating","Search_Score","Social_Signal_Score")
DDI_Data$Review_Rating <- as.numeric(NA)
DDI_Data$Search_Score <- as.numeric(NA)
DDI_Data$Social_Signal_Score <- as.numeric(NA)
DDI_Data$DDI <- as.numeric(NA)

#--- Calculate DDI
# System command
#system('echo "# Calculating DDI..."; echo "85"')
#---------
DDI_Data$Review_Rating <- Review_Rating_Data$Review_Score
DDI_Data$Search_Score <- Search_Score_Data$Net_Search_Score
DDI_Data$Social_Signal_Score <- Social_Signals$Net_Social_Signal

DDI_Data$DDI <- (DDI_Data$Review_Rating * Score_Weights$Review_Rating_Weight) + (DDI_Data$Search_Score * Score_Weights$Search_Score_Weight) + (DDI_Data$Social_Signal_Score * Score_Weights$Social_Signal_Weight)


#--- Save dtaa
# System command
#system('echo "# Saving final data..."; echo "90"')
#---------
DDI_Data <- cbind(Mapped_Products,DDI_Data)
write.csv(DDI_Data, file="/home/mono/Documents/Assortment/Top_25/Data/ProcessedData/5_Final_DDI.csv", row.names=F)
write.csv(All_Review,"All_Reviews.csv")
