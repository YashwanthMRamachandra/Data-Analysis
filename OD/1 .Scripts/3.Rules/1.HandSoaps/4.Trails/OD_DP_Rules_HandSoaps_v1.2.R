# -----------------------------------------------------------------------------------------------------------------------------------------------#
# -----------------------------------------------------------------------------------------------------------------------------------------------#
# -----------------------------------------------------------------------------------------------------------------------------------------------#
#                                                   || Rules Implementation : Pre-Processing Rules ||
# -----------------------------------------------------------------------------------------------------------------------------------------------#
# -----------------------------------------------------------------------------------------------------------------------------------------------#
# -----------------------------------------------------------------------------------------------------------------------------------------------#

  setwd("C:/Yashwanth/Pricing/1.OfficeDepot/10.Rules/1.Rules_data_files/")
  
  OD.Handsoap_Rule1 <- read.table("Rule1.csv",header = TRUE, sep = ",", quote = "\"", dec = ".",
                                  fill=TRUE, comment.char="", as.is=TRUE)

# -----------------------------------------------------------------------------------------------------------------------------------------------#
#                                                       ||  Rule #1 ||
# -----------------------------------------------------------------------------------------------------------------------------------------------#

  # Create Dummy varaible(Iferror)
  OD.Handsoap_Rule1$New_Lower_Threshold <- as.numeric(OD.Handsoap_Rule1$Lower_Threshold)
  OD.Handsoap_Rule1$New_Lower_Threshold[is.na(OD.Handsoap_Rule1$New_Lower_Threshold)] <- 1
  
  # Create Rule Condition
  Rule1 <- NA
  for(i in 1:nrow(OD.Handsoap_Rule1)){
    Rule1[i] <- ifelse(OD.Handsoap_Rule1$Last_2_week_Average_Sales1[i]<OD.Handsoap_Rule1$New_Lower_Threshold[i],TRUE,FALSE)
  }
  
  OD.Handsoap_Rule1 <- cbind(OD.Handsoap_Rule1,Rule1)
  OD.Handsoap_Rule1$Rule1[is.na(OD.Handsoap_Rule1$Rule1)] <- "FALSE"
  OD.Handsoap_Rule1$Action1 <- as.factor(ifelse(OD.Handsoap_Rule1$Rule1==TRUE,"R"," "))
  
  # Define Floor attribute
  attach(OD.Handsoap_Rule1)
  Floor1 <- NA
  for(i in c(1:nrow(OD.Handsoap_Rule1))){
    Floor1[i] <- ifelse(Action1[i]=="R",
                        max(0,0.11*Cost[i],0.8*Min_Competitor_Price[i])," ")
  }
  
  # Define CAP attribute
  CAP1 <- NA
  for(i in c(1:nrow(OD.Handsoap_Rule1))){
    CAP1[i] <- ifelse(Action1[i]=="R",Latest_OD_Price[i]," ")
  }
  detach(OD.Handsoap_Rule1);detach(OD.Handsoap_Rule1)
  
  OD.Handsoap_Rule1 <- cbind(OD.Handsoap_Rule1,Floor1,CAP1)

# -----------------------------------------------------------------------------------------------------------------------------------------------#
#                                                       ||  Rule #2 ||
# -----------------------------------------------------------------------------------------------------------------------------------------------#

  OD.Handsoap_Rule_12 <- read.table("Rule2.csv",header = TRUE, sep = ",", quote = "\"", dec = "." ,
                                  fill=TRUE, comment.char="", as.is=TRUE)
  
  # Create Dummy varaible(Iferror)
  OD.Handsoap_Rule_12 <- merge(OD.Handsoap_Rule1,OD.Handsoap_Rule_12,by="SKU")
  OD.Handsoap_Rule_12$Staples_Threshold <- as.numeric(OD.Handsoap_Rule_12$Staples_Threshold)
  OD.Handsoap_Rule_12$Staples_Threshold[is.na(OD.Handsoap_Rule_12$Staples_Threshold )] <- 0
  
  # Create Rule Condition
  Rule2 <- NA
  for(i in 1:nrow(OD.Handsoap_Rule_12)){
    Rule2[i] <- ifelse(OD.Handsoap_Rule_12$Latest_Staples_Price[i]>0 & OD.Handsoap_Rule_12$Last_Week_Staples_Price[i]>0,
                ifelse(OD.Handsoap_Rule_12$Latest_Staples_Price[i]<=OD.Handsoap_Rule_12$Staples_Threshold[i]*OD.Handsoap_Rule_12$Last_Week_Staples_Price[i],
                       TRUE,FALSE),FALSE)
  }
  OD.Handsoap_Rule_12 <- cbind(OD.Handsoap_Rule_12,Rule2)
  OD.Handsoap_Rule_12$Action2 <- as.factor(ifelse(OD.Handsoap_Rule_12$Rule2==TRUE,"R"," "))
  
  # Define Floor attribute
  attach(OD.Handsoap_Rule_12)
  Floor2 <- NA
  for(i in c(1:nrow(OD.Handsoap_Rule_12))){
    Floor2[i] <- ifelse(Action2[i]=="R",
                        max(0,0.11*Cost[i],0.8*Min_Competitor_Price[i])," ")
  }
  
  # Define CAP attribute
  CAP2 <- NA
  for(i in c(1:nrow(OD.Handsoap_Rule_12))){
    CAP2[i] <- ifelse(Action2[i]=="R",Latest_OD_Price[i]," ")
  }
  
  detach(OD.Handsoap_Rule_12);detach(OD.Handsoap_Rule_12)
  OD.Handsoap_Rule_12 <- cbind(OD.Handsoap_Rule_12,Floor2,CAP2)

# -----------------------------------------------------------------------------------------------------------------------------------------------#
#                                                       ||  Rule #3 ||
# -----------------------------------------------------------------------------------------------------------------------------------------------#

  OD.Handsoap_Rule_123 <- read.table("Rule3.csv",header = TRUE, sep = ",", quote = "\"", dec = "." ,
                                  fill=TRUE, comment.char="", as.is=TRUE)
  
  # Create Dummy varaible(Iferror)
  OD.Handsoap_Rule_123 <- merge(OD.Handsoap_Rule_12,OD.Handsoap_Rule_123,by="SKU")
  OD.Handsoap_Rule_123$Upper_Threshold <- as.numeric(OD.Handsoap_Rule_123$Upper_Threshold)
  OD.Handsoap_Rule_123$New2_Lower_Threshold <- as.numeric(OD.Handsoap_Rule_123$Lower_Threshold)
  OD.Handsoap_Rule_123$New2_Lower_Threshold[is.na(OD.Handsoap_Rule_123$New2_Lower_Threshold)] <- 1000
  
  # Create Rule Condition
  attach(OD.Handsoap_Rule_123)
  Rule3 <- NA
  for(i in 1:nrow(OD.Handsoap_Rule_123)){
    Rule3[i] <- ifelse(Last_2_week_Average_Sales3[i]!=" ",ifelse(Last_2_week_Average_Sales3[i]>New2_Lower_Threshold[i],
                                TRUE,FALSE),FALSE)
  }
  
  OD.Handsoap_Rule_123 <- cbind(OD.Handsoap_Rule_123,Rule3)
  OD.Handsoap_Rule_123$Rule3[is.na(OD.Handsoap_Rule_123$Rule3)] <- "FALSE"
  OD.Handsoap_Rule_123$Action3 <- as.factor(ifelse(OD.Handsoap_Rule_123$Rule3==TRUE,"I"," "))
  detach(OD.Handsoap_Rule_123);detach(OD.Handsoap_Rule_123)
  
  # Define Floor attribute
  attach(OD.Handsoap_Rule_123)
  Floor3 <- NA
  for(i in c(1:nrow(OD.Handsoap_Rule_123))){
    Floor3[i] <- ifelse(OD.Handsoap_Rule_123$Action3[i]=="I",
                        Latest_OD_Price[i]," ")
  }
  
  # Define CAP attribute
  CAP3 <- NA
  for(i in c(1:nrow(OD.Handsoap_Rule_123))){
    CAP3[i] <- ifelse(Action3[i]=="I",
                      min(1.1*Max_Competitor_Price[i],1.3*Latest_OD_Price[i])," ")
  }
  
  detach(OD.Handsoap_Rule_123);detach(OD.Handsoap_Rule_123)
  OD.Handsoap_Rule_123 <- cbind(OD.Handsoap_Rule_123,Floor3,CAP3)

# -----------------------------------------------------------------------------------------------------------------------------------------------#
#                                                       ||  Rule #4 ||
# -----------------------------------------------------------------------------------------------------------------------------------------------#

  OD.Handsoap_Rule_1234 <- OD.Handsoap_Rule_123
  
  # Create Rule Condition
  OD.Handsoap_Rule_1234$Rule4 <- ifelse(OD.Handsoap_Rule_1234$Latest_OD_Price<0.8*OD.Handsoap_Rule_1234$Min_Competitor_Price,TRUE,FALSE)
  OD.Handsoap_Rule_1234$Action4 <- ifelse(OD.Handsoap_Rule_1234$Rule4=="TRUE","I"," ")
  
  # Define Floor attribute
  attach(OD.Handsoap_Rule_1234)
  Floor4 <- NA
  for(i in 1:nrow(OD.Handsoap_Rule_1234)){
    Floor4[i] <- ifelse(Action4[i]=="I",Latest_OD_Price[i]," ")  
  }
  
  # Define CAP attribute
  CAP4 <- NA
  for(i in 1:nrow(OD.Handsoap_Rule_1234)){
    CAP4[i] <- ifelse(Action4[i]=="I",
                                 min(1.1*Max_Competitor_Price[i],1.3*Latest_OD_Price[i])," ")  
  }
  detach(OD.Handsoap_Rule_1234);detach(OD.Handsoap_Rule_1234)
  
  OD.Handsoap_Rule_1234 <- cbind(OD.Handsoap_Rule_1234,Floor4,CAP4)

# -----------------------------------------------------------------------------------------------------------------------------------------------#
#                                                       ||  Final Action, Floor & CAP ||
# -----------------------------------------------------------------------------------------------------------------------------------------------#

  OD.Handsoap_Rule_1234$Final_Action <- ifelse(OD.Handsoap_Rule_1234$Action1=="R" | OD.Handsoap_Rule_1234$Action2=="R","R",
                                        ifelse(OD.Handsoap_Rule_1234$Action3=="I" | OD.Handsoap_Rule_1234$Action4=="I","I"," "))
    
#  =IF(OR(Action1="R",Action2="R"),"R",IF(OR(Action31="I",Action4="I"),"I",""))

  # Define Floor attribute
  attach(OD.Handsoap_Rule_1234)
  Final_Floor <- NA
  for(i in 1:nrow(OD.Handsoap_Rule_1234)){
    Final_Floor[i] <- ifelse(Final_Action[i]=="R",max(Floor1[i],Floor2[i]),ifelse(Final_Action[i]=="I",max(Floor3[i],Floor4[i])," "))
  }

#  =IF(Final_Action="R",MAX(Floor1,Floor2),IF(Final_action="I",MAX(Floor3,Floor4),""))
 
  # Define CAP attribute
  Final_CAP <- NA
  for(i in 1:nrow(OD.Handsoap_Rule_1234)){
    Final_CAP[i] <-  ifelse(Final_Action[i]=="R",min(CAP1[i],CAP2[i]),ifelse(Final_Action[i]=="I",min(CAP3[i],CAP4[i])," "))
  }
  
  # =IF(Final_Action="R",MIN(CAP1,CAP2),IF(Final_Action="I",MIN(CAP3,CAP4),""))
  
  detach(OD.Handsoap_Rule_1234);detach(OD.Handsoap_Rule_1234)
  
  OD.Handsoap_Rule_1234 <- cbind(OD.Handsoap_Rule_1234,Final_Floor,Final_CAP)
  
  #merge(OD.Handsoap_Issue_SKU,OD.Handsoap_Rule_1234[,c(1,32:34)],by="SKU")
# -----------------------------------------------------------------------------------------------------------------------------------------------#
#                                                       ||  Export to Excel ||
# -----------------------------------------------------------------------------------------------------------------------------------------------#

  library(xlsx)
  write.xlsx(OD.Handsoap_Rule_1234,"C:/Yashwanth/Pricing/1.OfficeDepot/10.Rules/4.Execution/Handsoap_execution_R_v1.xlsx",
             row.names=FALSE,sheetName="Handsoap_Rules")
  
  write.xlsx(OD.Handsoap_Rule_1234,"Handsoap_execution_R.xlsx",
             row.names=FALSE,sheetName="Optimizer",append=TRUE)
  

#---------------------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------------

# --------------------------------------------------------------------------------------------------------------
#                                       || Rough Work ||
# --------------------------------------------------------------------------------------------------------------
#                                       || Data Filter ||
# --------------------------------------------------------------------------------------------------------------

OD.Comm_aggr <- aggregate(OD.Handsoap_Rule1$weekno,list(OD.Handsoap_Rule1$SKU),length)
names(OD.Comm_aggr) <- c("SKU","Count")
OD.Comm_aggr <- data.frame(subset(OD.Comm_aggr,Count>30,names(OD.Comm_aggr)),row.names=NULL)

OD.Comm_Filter <- merge(OD.Comm_aggr[1],OD.Handsoap_Rule1,by="SKU")
OD.Comm_Filter <- data.frame(with(OD.Comm_Filter,OD.Comm_Filter[order(SKU,weekno),]),row.names=NULL)


# --------------------------------------------------------------------------------------------------------------
#                                       || Average Sales : Moving Averages ||
# --------------------------------------------------------------------------------------------------------------

OD.Comm_Filter$Avg_Sales <- ceiling(SMA(OD.Comm_Filter$sales_units,n=2))

# --------------------------------------------------------------------------------------------------------------
#                                       || Rule #1 ||
# --------------------------------------------------------------------------------------------------------------


vec <- c(1:3,NA,5,NA,7:10)
X <- vec[c(4,6)]
for (j in 1:length(vec)){
vec[j] <- try(vec[j]==j,TRUE)
}
vec
