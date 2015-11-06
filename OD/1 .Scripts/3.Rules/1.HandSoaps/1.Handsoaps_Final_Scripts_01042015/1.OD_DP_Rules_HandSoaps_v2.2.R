# -----------------------------------------------------------------------------------------------------------------------------------------------#
# -----------------------------------------------------------------------------------------------------------------------------------------------#
# -----------------------------------------------------------------------------------------------------------------------------------------------#
#                                                   || Rules Implementation : Pre Processing Rules ||
# -----------------------------------------------------------------------------------------------------------------------------------------------#
# -----------------------------------------------------------------------------------------------------------------------------------------------#
# -----------------------------------------------------------------------------------------------------------------------------------------------#

# -----------------------------------------------------------------------------------------------------------------------------------------------#
#                                                           || Data Preparation ||
# -----------------------------------------------------------------------------------------------------------------------------------------------#

    rm(list=ls())
    setwd("C:/Yashwanth/Pricing/1.OfficeDepot/10.Rules/1.Rules_data_files/4.Data_v2.2/1.Handsoaps/")

# ------------------------------------------------------------------------------------------------------------------------------------------#
#                                                       || Rule 1 : Raw data ||                                                             #     #
# ------------------------------------------------------------------------------------------------------------------------------------------#
    
    OD.Handsoap_Rule1 <- read.table("Rule_1.csv",header = TRUE, sep = ",", quote = "\"", dec = ".",
                                    fill=TRUE, comment.char="", as.is=TRUE)
    OD.Handsoap_Rule1$Last_Weeks_OD_Price <- as.numeric(OD.Handsoap_Rule1$Last_Weeks_OD_Price)
    OD.Handsoap_Rule1$Latest_OD_Price <- as.numeric(OD.Handsoap_Rule1$Latest_OD_Price)
    OD.Handsoap_Rule1$Min_Competitor_Price <- as.numeric(OD.Handsoap_Rule1$Min_Competitor_Price)
    OD.Handsoap_Rule1$Max_Competitor_Price <- as.numeric(OD.Handsoap_Rule1$Max_Competitor_Price)
    OD.Handsoap_Rule1$Cost <- as.numeric(OD.Handsoap_Rule1$Cost)

# ------------------------------------------------------------------------------------------------------------------------------------------#
#                                                       || Cost data ||                                                                #
# ------------------------------------------------------------------------------------------------------------------------------------------#

    OD.Handsoap_Cost <- read.table("Cost.csv",header = TRUE, sep = ",", quote = "\"", dec = ".",
                                        fill=TRUE, comment.char="", as.is=TRUE)
    
# ------------------------------------------------------------------------------------------------------------------------------------------#
#                                                       || Threshold data ||                                                                #
# ------------------------------------------------------------------------------------------------------------------------------------------#
    
    OD.Handsoap_Threshold <- read.table("Threshold.csv",header = TRUE, sep = ",", quote = "\"", dec = ".",
                                    fill=TRUE, comment.char="", as.is=TRUE)

# ------------------------------------------------------------------------------------------------------------------------------------------#
#                                                         || Sales data ||                                                                 #
#-------------------------------------------------------------------------------------------------------------------------------------------#

    OD.Handsoap_Sales <- read.table("Sales.csv",header = TRUE, sep = ",", quote = "\"", dec = ".",
                                    fill=TRUE, comment.char="", as.is=TRUE)
      
    # Average of Last two Weeks
    OD.Handsoap_Sales$Average_of_last_2_weeks <- NA
    attach(OD.Handsoap_Sales)
    for(i in 1:nrow(OD.Handsoap_Sales)){
      OD.Handsoap_Sales$Average_of_last_2_weeks[i] <- ifelse(mean(c(X201512[i],X201513[i]),na.rm=T)=="NaN",0,
                                                             mean(c(X201512[i],X201513[i]),na.rm=T))
    }
    detach(OD.Handsoap_Sales)
      
    # Sum of Last 10 Weeks  
    OD.Handsoap_Sales$Sum_of_Last_10_weeks <- NA
    attach(OD.Handsoap_Sales)
    for(i in 1:nrow(OD.Handsoap_Sales)){
      j <- c(2:10,ncol(OD.Handsoap_Sales)-3);
      OD.Handsoap_Sales$Sum_of_Last_10_weeks[i] <- ifelse(sum(OD.Handsoap_Sales[i,j],na.rm=T)=="NA",0,
                                                             sum(OD.Handsoap_Sales[i,j],na.rm=T))
    }
    detach(OD.Handsoap_Sales)
      
    # Lower Threshold  & Last 2 week Average Sales  
    library(plyr)
    OD.Handsoap_Rule1 <- merge(OD.Handsoap_Rule1,OD.Handsoap_Threshold[,c("SKU","Sales_Lower_Threshold")],by="SKU",all.x=T)
    OD.Handsoap_Rule1 <- merge(OD.Handsoap_Rule1,OD.Handsoap_Sales[,c("SKU","Average_of_last_2_weeks")],by="SKU",all.x=T)
    
    OD.Handsoap_Rule1 <- rename(OD.Handsoap_Rule1,c("Sales_Lower_Threshold"="Lower_Threshold",
                                                    "Average_of_last_2_weeks"="Last_2_week_Average_Sales"))
                        
# -----------------------------------------------------------------------------------------------------------------------------------------------#
#                                                       ||  Rule #1 ||
# -----------------------------------------------------------------------------------------------------------------------------------------------#

    # Create Dummy varaible(Iferror)
    OD.Handsoap_Rule1$New_Lower_Threshold <- OD.Handsoap_Rule1$Lower_Threshold
    OD.Handsoap_Rule1$New_Lower_Threshold <- 
      
      ifelse(OD.Handsoap_Rule1$Cost=="NA",ifelse(OD.Handsoap_Rule1$New_Lower_Threshold=="NA",
                                            OD.Handsoap_Rule1$New_Lower_Threshold=="NA",
                                            OD.Handsoap_Rule1$New_Lower_Threshold==OD.Handsoap_Rule1$Lower_Threshold),
                                            OD.Handsoap_Rule1$New_Lower_Threshold==1)
    
    for(i in 1:nrow(OD.Handsoap_Rule1)){
      OD.Handsoap_Rule1$New_Lower_Threshold <- ifelse(OD.Handsoap_Rule1$Cost[i]=="NA" & OD.Handsoap_Rule1$Lower_Threshold[i]=="NA",
           ifelse(OD.Handsoap_Rule1$New_Lower_Threshold[i]=="NA",
           OD.Handsoap_Rule1$New_Lower_Threshold[is.na(OD.Handsoap_Rule1$New_Lower_Threshold[i])]==1),
           OD.Handsoap_Rule1$New_Lower_Threshold[i]==OD.Handsoap_Rule1$New_Lower_Threshold[i])
    }
    
    OD.Handsoap_Rule1$New_Last_2_week_Average_Sales <- OD.Handsoap_Rule1$Last_2_week_Average_Sales
    OD.Handsoap_Rule1$New_Last_2_week_Average_Sales[is.na(OD.Handsoap_Rule1$New_Last_2_week_Average_Sales)] <- 0
    OD.Handsoap_Rule1$New_Lower_Threshold <- OD.Handsoap_Rule1$Lower_Threshold
    OD.Handsoap_Rule1$New_Lower_Threshold[is.na(OD.Handsoap_Rule1$New_Lower_Threshold)] <- 1

#------------------------------------------------------------ Rule One Condition ----------------------------------------------------#
#                =IF(Last_2_week_Average_Sales1<>"",IF(Last_2_week_Average_Sales1<IFERROR(New_Lower_Threshold,1),TRUE,FALSE),FALSE)  #
#------------------------------------------------------------ Rule One Condition ----------------------------------------------------#
    
    attach(OD.Handsoap_Rule1)
    Rule1 <- NA
    for(i in 1:nrow(OD.Handsoap_Rule1)){
      Rule1[i] <- ifelse(New_Last_2_week_Average_Sales[i]<New_Lower_Threshold[i],TRUE,FALSE)
    }
    
    OD.Handsoap_Rule1 <- cbind(OD.Handsoap_Rule1,Rule1)
    OD.Handsoap_Rule1$Action1 <- as.factor(ifelse(OD.Handsoap_Rule1$Rule1==TRUE,"R"," "))
    detach(OD.Handsoap_Rule1)
    
    # Define Floor attribute
    attach(OD.Handsoap_Rule1)
    Floor1 <- NA
    for(i in c(1:nrow(OD.Handsoap_Rule1))){
      Floor1[i] <- ifelse(Action1[i]=="R",
                          ifelse(min(Latest_OD_Price[i],max(0,1.11*Cost[i],0.8*Min_Competitor_Price[i]),na.rm=T)=="Inf","NA",
                                 min(Latest_OD_Price[i],max(0,1.11*Cost[i],0.8*Min_Competitor_Price[i]),na.rm=T))," ")
    }
    
    # Define CAP attributeOD.Handsoap_Rule_123 <- merge(OD.Handsoap_Rule_
    CAP1 <- NA
    for(i in c(1:nrow(OD.Handsoap_Rule1))){
      CAP1[i] <- ifelse(Action1[i]=="R",Latest_OD_Price[i]," ")
    }
    
    detach(OD.Handsoap_Rule1)
    OD.Handsoap_Rule1 <- cbind(OD.Handsoap_Rule1,Floor1,CAP1)
    OD.Handsoap_Rule1 <- OD.Handsoap_Rule1[,-which(names(OD.Handsoap_Rule1) %in% c("New_Lower_Threshold","New_Last_2_week_Average_Sales"))]
    
# ------------------------------------------------------------------------------------------------------------------------------------------#
#                                                       ||  Rule #2 ||                                                                      #
# ------------------------------------------------------------------------------------------------------------------------------------------#
    
    # Price Latest data
    OD.Handsoap_Price_Latest <- read.table("Price_Latest.csv",header = TRUE, sep = ",", quote = "\"", dec = ".",
                                        fill=TRUE, comment.char="", as.is=TRUE,na.strings=" ")
    
    OD.Handsoap_Price_Latest$Min_Competitor_Regular_Price <- NA;
    attach(OD.Handsoap_Price_Latest)
    for(i in 1:nrow(OD.Handsoap_Price_Latest)){
    OD.Handsoap_Price_Latest$Min_Competitor_Regular_Price[i] <- ifelse(min(amz_mkt_regular_price[i],amz_regular_price[i],staples_regular_price[i],
                              Walmart_regular_price[i],BestBuy_regular_price[i],na.rm=T)=="Inf",0,
                              min(amz_mkt_regular_price[i],amz_regular_price[i],staples_regular_price[i],
                              Walmart_regular_price[i],BestBuy_regular_price[i],na.rm=T))
    }
    detach(OD.Handsoap_Price_Latest)
    
    attach(OD.Handsoap_Price_Latest)
    for(i in 1:nrow(OD.Handsoap_Price_Latest)){
      OD.Handsoap_Price_Latest$Max_Competitor_Regular_Price[i] <- ifelse(max(amz_mkt_regular_price[i],amz_regular_price[i],staples_regular_price[i],
                              Walmart_regular_price[i],BestBuy_regular_price[i],na.rm=T)=="-Inf",0,
                              max(amz_mkt_regular_price[i],amz_regular_price[i],staples_regular_price[i],
                              Walmart_regular_price[i],BestBuy_regular_price[i],na.rm=T))
    }
    detach(OD.Handsoap_Price_Latest)
    
    OD.Handsoap_Rule_12 <- merge(OD.Handsoap_Rule1,OD.Handsoap_Price_Latest[,c("SKU","staples_final_price")],by="SKU",all.x=TRUE)
    OD.Handsoap_Rule_12 <- rename(OD.Handsoap_Rule_12,c("staples_final_price"="Latest_Staples_Price"))

    # Threshold data
    OD.Handsoap_Rule_12 <- merge(OD.Handsoap_Rule_12,OD.Handsoap_Threshold[,c("SKU","Staples_Price_Change_Threshold")],by="SKU",all.x=T)
    OD.Handsoap_Rule_12 <- rename(OD.Handsoap_Rule_12,c("Staples_Price_Change_Threshold"="Staples_Threshold"))
    OD.Handsoap_Rule_12$Staples_Threshold <- as.numeric(gsub("%","",OD.Handsoap_Rule_12$Staples_Threshold))/100
    
    # Price Previous data
    OD.Handsoap_Price_Prev_Week <- read.table("Price_Prev_Week.csv",header = TRUE, sep = ",", quote = "\"", dec = ".",
                                           fill=TRUE, comment.char="", as.is=TRUE)
    OD.Handsoap_Rule_12 <- merge(OD.Handsoap_Rule_12,OD.Handsoap_Price_Prev_Week[,c("SKU","staples_final_price")],by="SKU",all.x=T)                    
    OD.Handsoap_Rule_12 <- rename(OD.Handsoap_Rule_12,c("staples_final_price"="Last_Week_Staples_Price"))
      
#------------------------------------------------------------ Rule Two Condition ----------------------------------------------------#
#                             # =IF(AND(IFERROR(Latest Staples Price,0)>0,IFERROR(Last Week Staples Price,0)>0),                     #
#                             #  IF(Latest Staples Price<=(IFERROR(Staples Threshold,0)*Last Week Staples Price),TRUE,FALSE),FALSE)  #
#------------------------------------------------------------ Rule Two Condition ----------------------------------------------------#
    
    # Create Dummy varaible(Iferror)
    OD.Handsoap_Rule_12$Latest_Staples_Price[is.na(OD.Handsoap_Rule_12$Latest_Staples_Price )] <- 0
    OD.Handsoap_Rule_12$Staples_Threshold[is.na(OD.Handsoap_Rule_12$Staples_Threshold )] <- 0
    OD.Handsoap_Rule_12$Last_Week_Staples_Price[is.na(OD.Handsoap_Rule_12$Last_Week_Staples_Price )] <- 0
    
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
                          min(Latest_OD_Price,max(0,1.11*Cost[i],0.8*Min_Competitor_Price[i]))," ")
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
  
  OD.Handsoap_Rule_123 <- merge(OD.Handsoap_Rule_12,OD.Handsoap_Threshold[,c("SKU","Sales_Upper_Threshold")],by="SKU",all.x=T)
  OD.Handsoap_Rule_123 <- rename(OD.Handsoap_Rule_123,c("Sales_Upper_Threshold"="Upper_Threshold"))
    
  OD.Handsoap_Rule_123 <- merge(OD.Handsoap_Rule_123,OD.Handsoap_Sales[,c("SKU","Average_of_last_2_weeks")],by="SKU",all.x=T)
  OD.Handsoap_Rule_123 <- rename(OD.Handsoap_Rule_123,c("Average_of_last_2_weeks"="Last_2_week_Average_Sales1"))
      
#------------------------------------------------------------ Rule Three Condition ----------------------------------------------------#
#               =IF(Last 2 week Average Sales<>"",IF(Last 2 week Average Sales>IFERROR(Upper Threshold,1000),TRUE,FALSE),FALSE),     #
#------------------------------------------------------------ Rule Two Condition ----------------------------------------------------#

  # Create Dummy varaible(Iferror)
  OD.Handsoap_Rule_123$Upper_Threshold[is.na(OD.Handsoap_Rule_123$Upper_Threshold)] <- 1000
  OD.Handsoap_Rule_123$Last_2_week_Average_Sales1[is.na(OD.Handsoap_Rule_123$Last_2_week_Average_Sales1)] <- 0

  attach(OD.Handsoap_Rule_123)
  Rule3 <- NA
  for(i in 1:nrow(OD.Handsoap_Rule_123)){
    Rule3[i] <- ifelse(Last_2_week_Average_Sales1[i]>Upper_Threshold[i],TRUE,FALSE)
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
    CAP3[i] <- ifelse(Action3[i]=="I",max(Latest_OD_Price[i],min(1.2*Max_Competitor_Price[i],1.3*Latest_OD_Price[i]))," ")
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
    CAP4[i] <- ifelse(Action4[i]=="I",max(Latest_OD_Price[i],min(1.2*Max_Competitor_Price[i],1.3*Latest_OD_Price[i]))," ")  
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
  write.xlsx(OD.Handsoap_Rule_1234,"C:/Yashwanth/Pricing/1.OfficeDepot/10.Rules/3.Execution/1.Handsoaps/Handsoap_execution_R_v2.2.xlsx",
             row.names=FALSE,sheetName="Pricing_Rules")


#------------------------------------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------------------------------
#------------------------------- END ------------------------ Rules Implementation ---------------------------- END -----------------------------
#------------------------------------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------------------------------
