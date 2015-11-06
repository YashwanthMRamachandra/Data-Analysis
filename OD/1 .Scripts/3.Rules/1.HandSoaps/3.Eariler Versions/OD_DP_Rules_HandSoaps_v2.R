# -----------------------------------------------------------------------------------------------------------------------------------------------#
# -----------------------------------------------------------------------------------------------------------------------------------------------#
# -----------------------------------------------------------------------------------------------------------------------------------------------#
#                                                   || Rules Implementation : Pre Processing Rules ||
# -----------------------------------------------------------------------------------------------------------------------------------------------#
# -----------------------------------------------------------------------------------------------------------------------------------------------#
# -----------------------------------------------------------------------------------------------------------------------------------------------#

  setwd("C:/Yashwanth/Pricing/1.OfficeDepot/10.Rules/1.Rules_data_files/3.Data_v2.1/")
  
  OD.Handsoap_Rule1 <- read.table("Rule_1.csv",header = TRUE, sep = ",", quote = "\"", dec = ".",
                                  fill=TRUE, comment.char="", as.is=TRUE)
  OD.Handsoap_Rule1$Last_Weeks_OD_Price <- as.numeric(OD.Handsoap_Rule1$Last_Weeks_OD_Price)
  OD.Handsoap_Rule1$Latest_OD_Price <- as.numeric(OD.Handsoap_Rule1$Latest_OD_Price)
  OD.Handsoap_Rule1$Min_Competitor_Price <- as.numeric(OD.Handsoap_Rule1$Min_Competitor_Price)
  OD.Handsoap_Rule1$Max_Competitor_Price <- as.numeric(OD.Handsoap_Rule1$Max_Competitor_Price)
  
# -----------------------------------------------------------------------------------------------------------------------------------------------#
#                                                       ||  Rule #1 ||
# -----------------------------------------------------------------------------------------------------------------------------------------------#

  # Create Dummy varaible(Iferror)
  OD.Handsoap_Rule1$New_Lower_Threshold <- as.numeric(OD.Handsoap_Rule1$Lower_Threshold)
  OD.Handsoap_Rule1$New_Lower_Threshold[is.na(OD.Handsoap_Rule1$New_Lower_Threshold)] <- 1

#------------------------------------------------------------ Rule One Condition ----------------------------------------------------#
#                =IF(Last_2_week_Average_Sales1<>"",IF(Last_2_week_Average_Sales1<IFERROR(New_Lower_Threshold,1),TRUE,FALSE),FALSE)  #
#------------------------------------------------------------ Rule One Condition ----------------------------------------------------#
  attach(OD.Handsoap_Rule1)
  Rule1 <- NA
  for(i in 1:nrow(OD.Handsoap_Rule1)){
    Rule1[i] <- ifelse(Last_2_week_Average_Sales1[i]!=" ",ifelse(Last_2_week_Average_Sales1[i]<New_Lower_Threshold[i],TRUE,FALSE),FALSE)
  }
  
  OD.Handsoap_Rule1 <- cbind(OD.Handsoap_Rule1,Rule1)
  OD.Handsoap_Rule1$Rule1[is.na(OD.Handsoap_Rule1$Rule1)] <- "FALSE"
  OD.Handsoap_Rule1$Action1 <- as.factor(ifelse(OD.Handsoap_Rule1$Rule1==TRUE,"R"," "))
  detach(OD.Handsoap_Rule1)
  
  # Define Floor attribute
  attach(OD.Handsoap_Rule1)
  Floor1 <- NA
  for(i in c(1:nrow(OD.Handsoap_Rule1))){
    Floor1[i] <- ifelse(Action1[i]=="R",
                        max(0,1.11*Cost[i],0.8*Min_Competitor_Price[i])," ")
  }
  
  # Define CAP attribute
  CAP1 <- NA
  for(i in c(1:nrow(OD.Handsoap_Rule1))){
    CAP1[i] <- ifelse(Action1[i]=="R",Latest_OD_Price[i]," ")
  }

  detach(OD.Handsoap_Rule1)
  OD.Handsoap_Rule1 <- cbind(OD.Handsoap_Rule1,Floor1,CAP1)

# -----------------------------------------------------------------------------------------------------------------------------------------------#
#                                                       ||  Rule #2 ||
# -----------------------------------------------------------------------------------------------------------------------------------------------#

  OD.Handsoap_Rule2 <- read.table("Rule_2.csv",header = TRUE, sep = ",", quote = "\"", dec = "." ,
                                  fill=TRUE, comment.char="", as.is=TRUE)
  
  OD.Handsoap_Rule2$Latest_Staples_Price <- as.numeric(OD.Handsoap_Rule2$Latest_Staples_Price)
  OD.Handsoap_Rule2$Staples_Threshold <- as.numeric(OD.Handsoap_Rule2$Staples_Threshold)
  OD.Handsoap_Rule2$Last_Week_Staples_Price <- as.numeric(OD.Handsoap_Rule2$Last_Week_Staples_Price)
  
  # Create Dummy varaible(Iferror)
  OD.Handsoap_Rule_12 <- cbind(OD.Handsoap_Rule1,OD.Handsoap_Rule2[,c(2:ncol(OD.Handsoap_Rule2))])
  OD.Handsoap_Rule_12$Staples_Threshold <- as.numeric(OD.Handsoap_Rule_12$Staples_Threshold)
  OD.Handsoap_Rule_12$Staples_Threshold[is.na(OD.Handsoap_Rule_12$Staples_Threshold )] <- 0
  
#------------------------------------------------------------ Rule Two Condition ----------------------------------------------------#
#                             # =IF(AND(IFERROR(Latest Staples Price,0)>0,IFERROR(Last Week Staples Price,0)>0),                     #
#                             #  IF(Latest Staples Price<=(IFERROR(Staples Threshold,0)*Last Week Staples Price),TRUE,FALSE),FALSE)  #
#------------------------------------------------------------ Rule Two Condition ----------------------------------------------------#
  attach(OD.Handsoap_Rule_12)
  Rule2 <- NA
  for(i in 1:nrow(OD.Handsoap_Rule_12)){
    Rule2[i] <- ifelse(Latest_Staples_Price[i]>0 & Last_Week_Staples_Price[i]>0,
                 ifelse(Latest_Staples_Price[i]<=Staples_Threshold[i]*Last_Week_Staples_Price[i],TRUE,FALSE),FALSE)
  }
  Rule2[is.na(Rule2)] <- FALSE
  
  OD.Handsoap_Rule_12 <- cbind(OD.Handsoap_Rule_12,Rule2)
  OD.Handsoap_Rule_12$Action2 <- as.factor(ifelse(OD.Handsoap_Rule_12$Rule2==TRUE,"R"," "))
  detach(OD.Handsoap_Rule_12)
    
  # Define Floor attribute
  attach(OD.Handsoap_Rule_12)
  Floor2 <- NA
  for(i in c(1:nrow(OD.Handsoap_Rule_12))){
    Floor2[i] <- ifelse(Action2[i]=="R",
                        max(0,1.11*Cost[i],0.8*Min_Competitor_Price[i])," ")
  }
  
  # Define CAP attribute
  CAP2 <- NA
  for(i in c(1:nrow(OD.Handsoap_Rule_12))){
    CAP2[i] <- ifelse(Action2[i]=="R",Latest_OD_Price[i]," ")
  }
  
  detach(OD.Handsoap_Rule_12)
  OD.Handsoap_Rule_12 <- cbind(OD.Handsoap_Rule_12,Floor2,CAP2)

# -----------------------------------------------------------------------------------------------------------------------------------------------#
#                                                       ||  Rule #3 ||
# -----------------------------------------------------------------------------------------------------------------------------------------------#

  OD.Handsoap_Rule3 <- read.table("Rule_3.csv",header = TRUE, sep = ",", quote = "\"", dec = "." ,
                                  fill=TRUE, comment.char="", as.is=TRUE)
  
  # Create Dummy varaible(Iferror)
  OD.Handsoap_Rule_123 <- cbind(OD.Handsoap_Rule_12,OD.Handsoap_Rule3[,2:ncol(OD.Handsoap_Rule3)])
  OD.Handsoap_Rule_123$Upper_Threshold <- as.numeric(OD.Handsoap_Rule_123$Upper_Threshold)
  OD.Handsoap_Rule_123$Upper_Threshold[is.na(OD.Handsoap_Rule_123$Upper_Threshold)] <- 1000

#------------------------------------------------------------ Rule Two Condition ----------------------------------------------------#
#               =IF(Last 2 week Average Sales<>"",IF(Last 2 week Average Sales>IFERROR(Upper Threshold,1000),TRUE,FALSE),FALSE),     #
#------------------------------------------------------------ Rule Two Condition ----------------------------------------------------#
  attach(OD.Handsoap_Rule_123)
  Rule3 <- NA
  for(i in 1:nrow(OD.Handsoap_Rule_123)){
    Rule3[i] <- ifelse(Last_2_week_Average_Sales3[i]!=" ",ifelse(Last_2_week_Average_Sales3[i]>Upper_Threshold[i],TRUE,FALSE),FALSE)
  }
  Rule3[is.na(Rule3)] <- FALSE
  
  OD.Handsoap_Rule_123 <- cbind(OD.Handsoap_Rule_123,Rule3)
  OD.Handsoap_Rule_123$Action3 <- as.factor(ifelse(OD.Handsoap_Rule_123$Rule3==TRUE,"I"," "))
  detach(OD.Handsoap_Rule_123)
  
  # Define Floor attribute
  attach(OD.Handsoap_Rule_123)
  Floor3 <- NA
  for(i in c(1:nrow(OD.Handsoap_Rule_123))){
    Floor3[i] <- ifelse(OD.Handsoap_Rule_123$Action3[i]=="I",Latest_OD_Price[i]," ")
  }
  
  # Define CAP attribute
  CAP3 <- NA
  for(i in c(1:nrow(OD.Handsoap_Rule_123))){
    CAP3[i] <- ifelse(Action3[i]=="I",min(1.2*Max_Competitor_Price[i],1.3*Latest_OD_Price[i])," ")
  }
  
  detach(OD.Handsoap_Rule_123)
  OD.Handsoap_Rule_123 <- cbind(OD.Handsoap_Rule_123,Floor3,CAP3)

# -----------------------------------------------------------------------------------------------------------------------------------------------#
#                                                       ||  Rule #4 ||
# -----------------------------------------------------------------------------------------------------------------------------------------------#

  OD.Handsoap_Rule_1234 <- OD.Handsoap_Rule_123

#------------------------------------------------------------ Rule Four Condition ----------------------------------------------------#
#                                       # =IF(Latest OD Price<0.8*Min Competitor Price,TRUE,FALSE)                                    #
#------------------------------------------------------------ Rule Four Condition ----------------------------------------------------#

  OD.Handsoap_Rule_1234$Rule4 <- ifelse(OD.Handsoap_Rule_1234$Latest_OD_Price<0.8*OD.Handsoap_Rule_1234$Min_Competitor_Price,TRUE,FALSE)
  OD.Handsoap_Rule_1234$Rule4[is.na(OD.Handsoap_Rule_1234$Rule4)] <- FALSE
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
    CAP4[i] <- ifelse(Action4[i]=="I",min(1.2*Max_Competitor_Price[i],1.3*Latest_OD_Price[i])," ")  
  }
  
  detach(OD.Handsoap_Rule_1234)
  OD.Handsoap_Rule_1234 <- cbind(OD.Handsoap_Rule_1234,Floor4,CAP4)

# -----------------------------------------------------------------------------------------------------------------------------------#
#                                                       ||  Final Action, Floor & CAP ||                                             #
#                                   = IF(OR(Action1="R",Action2="R"),"R",IF(OR(Action3="I",Action4="I"),"I",""))                     #             
# -----------------------------------------------------------------------------------------------------------------------------------#

  OD.Handsoap_Rule_1234$Final_Action <- ifelse(OD.Handsoap_Rule_1234$Action1=="R" | OD.Handsoap_Rule_1234$Action2=="R","R",
                                               ifelse(OD.Handsoap_Rule_1234$Action3=="I" | OD.Handsoap_Rule_1234$Action4=="I","I"," "))
  
  # Define Floor attribute
  attach(OD.Handsoap_Rule_1234)
  Final_Floor <- NA
  for(i in 1:nrow(OD.Handsoap_Rule_1234)){
    Final_Floor[i] <- ifelse(Final_Action[i]=="R",max(Floor1[i],Floor2[i]),
                             ifelse(Final_Action[i]=="I",max(Floor3[i],Floor4[i])," "))
  }
  
  # Define CAP attribute
  Final_CAP <- NA
  for(i in 1:nrow(OD.Handsoap_Rule_1234)){
    Final_CAP[i] <- ifelse(Final_Action[i]=="R",max(CAP1[i],CAP2[i]),
                           ifelse(Final_Action[i]=="I",max(CAP3[i],CAP4[i])," "))
  }
  
  detach(OD.Handsoap_Rule_1234)
  OD.Handsoap_Rule_1234 <- cbind(OD.Handsoap_Rule_1234,Final_Floor,Final_CAP)

# -----------------------------------------------------------------------------------------------------------------------------------------------#
#                                                       ||  Export to Excel ||
# -----------------------------------------------------------------------------------------------------------------------------------------------#

  library(xlsx)
  write.xlsx(OD.Handsoap_Rule_1234,"C:/Yashwanth/Pricing/1.OfficeDepot/10.Rules/3.Execution/1.Handsoaps/Handsoap_execution_R_v2.xlsx",
             row.names=FALSE,sheetName="Pricing_Rules")


#------------------------------------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------------------------------
#------------------------------- END ------------------------ Rules Implementation ---------------------------- END -----------------------------
#------------------------------------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------------------------------
