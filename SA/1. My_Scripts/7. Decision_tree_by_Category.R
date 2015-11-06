# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
#                                   Best Seller Model ;  Logistic Ression
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------------
#                                             Input Data Maanipulation
#---------------------------------------------------------------------------------------------------------------

rm(list=setdiff(ls(),"Review_Aggr_Model_Data"))
ls()

library(data.table)
Review_Aggr_Model_Data <- data.table(Review_Aggr_Model_Data)
SA_SII_Act_Mont <- Review_Aggr_Model_Data[Category=="Activity Monitors"]
SA_SII_BBB <- Review_Aggr_Model_Data[Category=="Baseball Bat"]
SA_SII_Cleats <- Review_Aggr_Model_Data[Category=="Cleats"]
SA_SII_GCS <- Review_Aggr_Model_Data[Category=="Golf Complete Sets"]
SA_SII_NFL_App <- Review_Aggr_Model_Data[Category=="NFL Apparel"]
SA_SII_Train_Aid <- Review_Aggr_Model_Data[Category=="Training Aid"]
SA_SII_Tread <- Review_Aggr_Model_Data[Category=="Treadmill"]
SA_SII_WRS <- Review_Aggr_Model_Data[Category=="Women's Running Shoes"]

# Create Sales factor
SA_SII_Act_Mont <- with(SA_SII_Act_Mont,SA_SII_Act_Mont[order(Sales_Units,decreasing = TRUE),])
SA_SII_Act_Mont$Sales <- c(rep(1,10),rep(0,length(11:nrow(SA_SII_Act_Mont))))

SA_SII_BBB <- with(SA_SII_BBB,SA_SII_BBB[order(Sales_Units,decreasing = TRUE),])
SA_SII_BBB$Sales <- c(rep(1,15),rep(0,length(16:nrow(SA_SII_BBB))))

SA_SII_Cleats <- with(SA_SII_Cleats,SA_SII_Cleats[order(Sales_Units,decreasing = TRUE),])
SA_SII_Cleats$Sales <- c(rep(1,30),rep(0,length(31:nrow(SA_SII_Cleats))))

SA_SII_GCS <- with(SA_SII_GCS,SA_SII_GCS[order(Sales_Units,decreasing = TRUE),])
SA_SII_GCS$Sales <- c(rep(1,5),rep(0,length(6:nrow(SA_SII_GCS))))

SA_SII_NFL_App <- with(SA_SII_NFL_App,SA_SII_NFL_App[order(Sales_Units,decreasing = TRUE),])
SA_SII_NFL_App$Sales <- c(rep(1,15),rep(0,length(16:nrow(SA_SII_NFL_App))))

SA_SII_Train_Aid <- with(SA_SII_Train_Aid,SA_SII_Train_Aid[order(Sales_Units,decreasing = TRUE),])
SA_SII_Train_Aid$Sales <- c(rep(1,35),rep(0,length(36:nrow(SA_SII_Train_Aid))))

SA_SII_Tread <- with(SA_SII_Tread,SA_SII_Tread[order(Sales_Units,decreasing = TRUE),])
SA_SII_Tread$Sales <- c(rep(1,20),rep(0,length(21:nrow(SA_SII_Tread))))

SA_SII_WRS <- with(SA_SII_WRS,SA_SII_WRS[order(Sales_Units,decreasing = TRUE),])
SA_SII_WRS$Sales <- c(rep(1,10),rep(0,length(11:nrow(SA_SII_WRS))))

SA_SII_DT <- rbind(SA_SII_Act_Mont,SA_SII_BBB,SA_SII_Cleats,SA_SII_GCS,SA_SII_NFL_App,SA_SII_Train_Aid,SA_SII_Tread,SA_SII_WRS)
  
#---------------------------------------------------------------------------------------------------------------
#                                                   Decision Tree
#---------------------------------------------------------------------------------------------------------------

SA_SII_DT <- data.frame(SA_SII_DT)
SA_SII_DT_IND <- NA
SA_SII_DT_Train <- NA
SA_SII_DT_Test <- NA
for(i in c(unique(SA_SII_DT$Category))){
  set.seed(400)
  SA_SII_DT_IND[i] <- sample(nrow(SA_SII_DT[i]),size=round(((nrow(SA_SII_DT[i])/100)*70)+1,0))
  SA_SII_DT_Train[i] <- SA_SII_DT[SA_SII_DT_IND[i],]
  SA_SII_DT_Test[i] <- SA_SII_DT[-SA_SII_DT_IND[i],]
}


# Sampling
set.seed(400)
SA_SII_DT_IND <- sample(nrow(SA_SII_DT),size=round(((nrow(SA_SII_DT)/100)*70)+1,0))
SA_SII_DT_Train <- SA_SII_DT[SA_SII_DT_IND,]
SA_SII_DT_Test <- SA_SII_DT[-SA_SII_DT_IND,]

# Build the Tree
library(rpart)
SA_DT <- rpart(Decision_Criterion ~ Conversion+Price_Comp+OA_Bounce_rate+OA_Cart_Views+OA_Visit_Freq, 
               data = SA_SII_DT_Train)
