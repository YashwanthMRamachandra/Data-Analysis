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
install.packages("plyr",dependencies=T)
install.packages("xlsx",dependencies=T)

rm(list=ls())
setwd("C:/Yashwanth/Pricing/1.OfficeDepot/10.Rules/1.Rules_data_files/5.Data_v2.3/1.Handsoaps/")

# ------------------------------------------------------------------------------------------------------------------------------------------#
#                                                       || Rule 1 : Raw data ||                                                             #     #
# ------------------------------------------------------------------------------------------------------------------------------------------#

OD.Handsoap_Rule1 <- read.table("Rule_1.csv",header = TRUE, sep = ",", quote = "\"", dec = ".",
                                fill=TRUE, comment.char="", as.is=TRUE)

# ------------------------------------------------------------------------------------------------------------------------------------------#
#                                                       || Price Previous Week ||                                                           #
# ------------------------------------------------------------------------------------------------------------------------------------------#

OD.Handsoap_Price_Prev_Week <- read.table("Price_Prev_Week.csv",header = TRUE, sep = ",", quote = "\"", dec = ".",
                                          fill=TRUE, comment.char="", as.is=TRUE)

OD.Handsoap_Price_Prev_Week$Min_Competitor_Regular_Price <- NA;
attach(OD.Handsoap_Price_Prev_Week)
for(i in 1:nrow(OD.Handsoap_Price_Prev_Week)){
  OD.Handsoap_Price_Prev_Week$Min_Competitor_Regular_Price[i] <- ifelse(min(amz_mkt_regular_price[i],amz_regular_price[i],staples_regular_price[i],
                                                                            Walmart_regular_price[i],BestBuy_regular_price[i],na.rm=T)=="Inf",0,
                                                                        min(amz_mkt_regular_price[i],amz_regular_price[i],staples_regular_price[i],
                                                                            Walmart_regular_price[i],BestBuy_regular_price[i],na.rm=T))
}
detach(OD.Handsoap_Price_Prev_Week)

OD.Handsoap_Price_Prev_Week$Max_Competitor_Regular_Price <- NA
attach(OD.Handsoap_Price_Prev_Week)
for(i in 1:nrow(OD.Handsoap_Price_Prev_Week)){
  OD.Handsoap_Price_Prev_Week$Max_Competitor_Regular_Price[i] <- ifelse(max(amz_mkt_regular_price[i],amz_regular_price[i],staples_regular_price[i],
                                                                            Walmart_regular_price[i],BestBuy_regular_price[i],na.rm=T)=="-Inf",0,
                                                                        max(amz_mkt_regular_price[i],amz_regular_price[i],staples_regular_price[i],
                                                                            Walmart_regular_price[i],BestBuy_regular_price[i],na.rm=T))
}
detach(OD.Handsoap_Price_Prev_Week)

library(plyr)
OD.Handsoap_Rule1 <- merge(OD.Handsoap_Rule1,OD.Handsoap_Price_Prev_Week[,c("SKU","od_final_price")],by="SKU",all.x=T)
OD.Handsoap_Rule1 <- rename(OD.Handsoap_Rule1,c("od_final_price"="Last_Weeks_OD_Price"))

# ------------------------------------------------------------------------------------------------------------------------------------------#
#                                                       || Price Latest ||                                                                  #
# ------------------------------------------------------------------------------------------------------------------------------------------#
OD.Handsoap_Price_Latest <- read.table("Price_Latest.csv",header = TRUE, sep = ",", quote = "\"", dec = ".",
                                       fill=TRUE, comment.char="", as.is=TRUE)

OD.Handsoap_Price_Latest$Min_Competitor_Regular_Price <- NA;
attach(OD.Handsoap_Price_Latest)
for(i in 1:nrow(OD.Handsoap_Price_Latest)){
  OD.Handsoap_Price_Latest$Min_Competitor_Regular_Price[i] <- ifelse(min(amz_mkt_regular_price[i],amz_regular_price[i],staples_regular_price[i],
                                                                         Walmart_regular_price[i],BestBuy_regular_price[i],na.rm=T)=="Inf",0,
                                                                     min(amz_mkt_regular_price[i],amz_regular_price[i],staples_regular_price[i],
                                                                         Walmart_regular_price[i],BestBuy_regular_price[i],na.rm=T))
}
detach(OD.Handsoap_Price_Latest)

OD.Handsoap_Price_Latest$Max_Competitor_Regular_Price <- NA
attach(OD.Handsoap_Price_Latest)
for(i in 1:nrow(OD.Handsoap_Price_Latest)){
  OD.Handsoap_Price_Latest$Max_Competitor_Regular_Price[i] <- ifelse(max(amz_mkt_regular_price[i],amz_regular_price[i],staples_regular_price[i],
                                                                         Walmart_regular_price[i],BestBuy_regular_price[i],na.rm=T)=="-Inf",0,
                                                                     max(amz_mkt_regular_price[i],amz_regular_price[i],staples_regular_price[i],
                                                                         Walmart_regular_price[i],BestBuy_regular_price[i],na.rm=T))
}
detach(OD.Handsoap_Price_Latest)

OD.Handsoap_Rule1 <- merge(OD.Handsoap_Rule1,OD.Handsoap_Price_Latest[,c("SKU","od_final_price","Min_Competitor_Regular_Price",
                                                                         "Max_Competitor_Regular_Price")],by="SKU",all.x=T)
OD.Handsoap_Rule1 <- rename(OD.Handsoap_Rule1,c("od_final_price"="Latest_OD_Price","Min_Competitor_Regular_Price"="Min_Competitor_Price",
                                                "Max_Competitor_Regular_Price"="Max_Competitor_Price"))

# ------------------------------------------------------------------------------------------------------------------------------------------#
#                                                       || Cost data ||                                                                     #
# ------------------------------------------------------------------------------------------------------------------------------------------#

OD.Handsoap_Cost <- read.table("Cost.csv",header = TRUE, sep = ",", quote = "\"", dec = ".",
                               fill=TRUE, comment.char="", as.is=TRUE)

OD.Handsoap_Rule1 <- merge(OD.Handsoap_Rule1,OD.Handsoap_Cost[,c("SKU","WTD_UNIT_COST")],by="SKU",all.x=T)
OD.Handsoap_Rule1 <- rename(OD.Handsoap_Rule1,c("WTD_UNIT_COST"="Cost"))

# ------------------------------------------------------------------------------------------------------------------------------------------#
#                                                       || Threshold data ||                                                                #
# ------------------------------------------------------------------------------------------------------------------------------------------#

OD.Handsoap_Threshold <- read.table("Threshold.csv",header = TRUE, sep = ",", quote = "\"", dec = ".",
                                    fill=TRUE, comment.char="", as.is=TRUE)

OD.Handsoap_Rule1 <- merge(OD.Handsoap_Rule1,OD.Handsoap_Threshold[,c("SKU","Sales_Lower_Threshold")],by="SKU",all.x=T)
OD.Handsoap_Rule1 <- rename(OD.Handsoap_Rule1,c("Sales_Lower_Threshold"="Lower_Threshold"))

# ------------------------------------------------------------------------------------------------------------------------------------------#
#                                                         || Sales data ||                                                                 #
#-------------------------------------------------------------------------------------------------------------------------------------------#

OD.Handsoap_Sales <- read.table("Sales.csv",header = TRUE, sep = ",", quote = "\"", dec = ".",
                                fill=TRUE, comment.char="", as.is=TRUE)

# Average of Last two Weeks
OD.Handsoap_Sales$Average_of_last_2_weeks <- NA
attach(OD.Handsoap_Sales)
for(i in 1:nrow(OD.Handsoap_Sales)){
  OD.Handsoap_Sales$Average_of_last_2_weeks[i] <- ifelse(is.na(FISCAL_WEEK_13_OF_2015[i] & is.na(FISCAL_WEEK_14_OF_2015[i])),0,
                                                         ifelse(is.na(FISCAL_WEEK_13_OF_2015[i]) | is.na(FISCAL_WEEK_14_OF_2015[i]),
                                                                sum(FISCAL_WEEK_13_OF_2015[i],FISCAL_WEEK_14_OF_2015[i],na.rm=T),mean(c(FISCAL_WEEK_13_OF_2015[i],FISCAL_WEEK_14_OF_2015[i]))))
}
detach(OD.Handsoap_Sales)

# Sum of Last 10 Weeks  
OD.Handsoap_Sales$Sum_of_Last_10_weeks <- NA
attach(OD.Handsoap_Sales)
for(i in 1:nrow(OD.Handsoap_Sales)){
  j <- c(2:10,ncol(OD.Handsoap_Sales)-2);
  OD.Handsoap_Sales$Sum_of_Last_10_weeks[i] <- ifelse(sum(OD.Handsoap_Sales[i,j],na.rm=T)=="NA",0,
                                                      sum(OD.Handsoap_Sales[i,j],na.rm=T))
}
detach(OD.Handsoap_Sales)

# Lower Threshold  & Last 2 week Average Sales  
library(plyr)
OD.Handsoap_Rule1 <- merge(OD.Handsoap_Rule1,OD.Handsoap_Sales[,c("SKU","Average_of_last_2_weeks")],by="SKU",all.x=T)
OD.Handsoap_Rule1 <- rename(OD.Handsoap_Rule1,c("Average_of_last_2_weeks"="Last_2_week_Average_Sales"))

# -----------------------------------------------------------------------------------------------------------------------------------------------#
#                                                       ||  Rule #1 ||
# -----------------------------------------------------------------------------------------------------------------------------------------------#

# Create Dummy varaible(Iferror)
OD.Handsoap_Rule1$New_Lower_Threshold <- as.numeric(OD.Handsoap_Rule1$Lower_Threshold)
OD.Handsoap_Rule1$New_Lower_Threshold[is.na(OD.Handsoap_Rule1$New_Lower_Threshold)] <- 1

OD.Handsoap_Rule1$New_Last_2_week_Average_Sales <- as.numeric(OD.Handsoap_Rule1$Last_2_week_Average_Sales)
OD.Handsoap_Rule1$New_Last_2_week_Average_Sales[is.na(OD.Handsoap_Rule1$New_Last_2_week_Average_Sales)] <- 0

#------------------------------------------------------------ Rule One Condition ----------------------------------------------------#
#                =IF(Last_2_week_Average_Sales1<>"",IF(Last_2_week_Average_Sales1<IFERROR(New_Lower_Threshold,1),TRUE,FALSE),FALSE)  #
#------------------------------------------------------------ Rule One Condition ----------------------------------------------------#

attach(OD.Handsoap_Rule1)
OD.Handsoap_Rule1$Rule1 <- NA
for(i in 1:nrow(OD.Handsoap_Rule1)){
  OD.Handsoap_Rule1$Rule1[i] <- ifelse(New_Last_2_week_Average_Sales[i]<New_Lower_Threshold[i],TRUE,FALSE)
}

OD.Handsoap_Rule1$Action1 <- as.factor(ifelse(OD.Handsoap_Rule1$Rule1==TRUE,"R"," "))
detach(OD.Handsoap_Rule1)

# Define Floor attribute
attach(OD.Handsoap_Rule1)
OD.Handsoap_Rule1$Floor1 <- NA
for(i in c(1:nrow(OD.Handsoap_Rule1))){
  OD.Handsoap_Rule1$Floor1[i] <- ifelse(Action1[i]=="R",
                                        ifelse(min(0.9*Latest_OD_Price[i],max(0,1.11*Cost[i],0.8*Min_Competitor_Price[i]))=="Inf","NA",
                                               min(0.9*Latest_OD_Price[i],max(0,1.11*Cost[i],0.8*Min_Competitor_Price[i])))," ")
}

# Define CAP 
OD.Handsoap_Rule1$CAP1 <- NA
for(i in c(1:nrow(OD.Handsoap_Rule1))){
  OD.Handsoap_Rule1$CAP1[i] <- ifelse(Action1[i]=="R",0.9*Latest_OD_Price[i]," ")
}

detach(OD.Handsoap_Rule1)
OD.Handsoap_Rule1 <- OD.Handsoap_Rule1[,grep("New",names(OD.Handsoap_Rule1),value=T,invert=T)]

# ------------------------------------------------------------------------------------------------------------------------------------------#
#                                                       ||  Rule #2 ||                                                                      #
# ------------------------------------------------------------------------------------------------------------------------------------------#

# Price Latest
OD.Handsoap_Rule_12 <- merge(OD.Handsoap_Rule1,OD.Handsoap_Price_Latest[,c("SKU","staples_final_price")],by="SKU",all.x=TRUE)
OD.Handsoap_Rule_12 <- rename(OD.Handsoap_Rule_12,c("staples_final_price"="Latest_Staples_Price"))

# Threshold data
OD.Handsoap_Rule_12 <- merge(OD.Handsoap_Rule_12,OD.Handsoap_Threshold[,c("SKU","Staples_Price_Change_Threshold")],by="SKU",all.x=T)
OD.Handsoap_Rule_12 <- rename(OD.Handsoap_Rule_12,c("Staples_Price_Change_Threshold"="Staples_Threshold"))
OD.Handsoap_Rule_12$Staples_Threshold <- as.numeric(gsub("%","",OD.Handsoap_Rule_12$Staples_Threshold))/100

# Price Previous data
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
                      min(0.9*Latest_OD_Price,max(0,1.11*Cost[i],0.8*Min_Competitor_Price[i]))," ")
}

# Define CAP attribute
CAP2 <- NA
for(i in c(1:nrow(OD.Handsoap_Rule_12))){
  CAP2[i] <- ifelse(Action2[i]=="R",0.9*Latest_OD_Price[i]," ")
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
#------------------------------------------------------------ Rule Three Condition ----------------------------------------------------#

# Create Dummy varaible(Iferror)
OD.Handsoap_Rule_123$New_Upper_Threshold <- as.numeric(OD.Handsoap_Rule_123$Upper_Threshold)
OD.Handsoap_Rule_123$New_Upper_Threshold[is.na(OD.Handsoap_Rule_123$New_Upper_Threshold)] <- 1000

OD.Handsoap_Rule_123$New_Last_2_week_Average_Sales1 <- as.numeric(OD.Handsoap_Rule_123$Last_2_week_Average_Sales1)
OD.Handsoap_Rule_123$New_Last_2_week_Average_Sales1[is.na(OD.Handsoap_Rule_123$New_Last_2_week_Average_Sales1)] <- 0

attach(OD.Handsoap_Rule_123)
Rule3 <- NA
for(i in 1:nrow(OD.Handsoap_Rule_123)){
  Rule3[i] <- ifelse(New_Last_2_week_Average_Sales1[i]>New_Upper_Threshold[i],TRUE,FALSE)
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
OD.Handsoap_Rule_123 <- OD.Handsoap_Rule_123[,grep("New",names(OD.Handsoap_Rule_123),value=T,invert=T)]

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


# Rules Triggered
OD.Handsoap_Rule_1234$Rules_Triggered <- NA
attach(OD.Handsoap_Rule_1234)
for(i in 1:nrow(OD.Handsoap_Rule_1234)){
  OD.Handsoap_Rule_1234$Rules_Triggered[i] <- ifelse(Final_Action[i]==" "," ",ifelse(Rule1[i]=="TRUE","Sales too low",ifelse(Rule2[i]=="TRUE","Comp dropped price",
                                                                                                                             ifelse(Rule3[i]=="TRUE","Sales high",ifelse(Rule4[i]=="TRUE","OD price lower than market")))))
}
detach(OD.Handsoap_Rule_1234)

# -----------------------------------------------------------------------------------------------------------------------------------------------#
#                                                       ||  Export to Excel ||
# -----------------------------------------------------------------------------------------------------------------------------------------------#

save.image("~/OD Dynamic Pricing/OD_DynamicPricing/2.Rules/1.Handsoaps/Rules_Implemetation_v2.3.RData")

library(xlsx)
write.xlsx(OD.Handsoap_Rule_1234,"C:/Yashwanth/Pricing/1.OfficeDepot/10.Rules/3.Execution/1.Handsoaps/Handsoap_execution_R_v2.3.xlsx",
           row.names=FALSE,sheetName="Pricing_Rules")


#------------------------------------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------------------------------
#------------------------------- END ------------------------ Rules Implementation ---------------------------- END -----------------------------
#------------------------------------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------------------------------

# -----------------------------------------------------------------------------------------------------------------------------------------------#
# -----------------------------------------------------------------------------------------------------------------------------------------------#
# -----------------------------------------------------------------------------------------------------------------------------------------------#
#                                                   || Rules Implementation : Optimizer ||
# -----------------------------------------------------------------------------------------------------------------------------------------------#
# -----------------------------------------------------------------------------------------------------------------------------------------------#
# -----------------------------------------------------------------------------------------------------------------------------------------------#

# Model  Ln Sales = Int -0.03 (OD Final Price - Staples Final Price) + 0.0013 * Sales of Previous Week
# Ln S2 - Ln S1 = 0.03 (ODP 1 - STP 1 - ODP2 + STP2) + 0.0013 * (S1 - S0) 
# Ln S2 = ln S1 + 0.03(ODP 1 + STP2 - STP 1 - ODP2) + 0.0013 * (S1 - S0) 
# S2 = exp(ln S1 + 0.03(ODP1 + STP 2 - STP 1 - ODP2) + 0.0013 * (S1 - S0) )
# 
# Margin2 = S2 (ODP2 - Cost)
# Margin 2 = exp[ln S1 + 0.03 (ODP 1 + STP 2 - STP 1 - ODP2) + 0.0013 * (S1 - S0) ]*(ODP2-Cost)

rm(list=ls())
load("~/OD Dynamic Pricing/OD_DynamicPricing/2.Rules/1.Handsoaps/Rules_Implemetation_v2.3.RData")
rm(list=setdiff(ls(),c("OD.Handsoap_Rule_1234","OD.Handsoap_Sales","OD.Handsoap_Price_Prev_Week","OD.Handsoap_Price_Latest",
                       "OD.Handsoap_Cost","OD.Handsoap_Threshold")))

setwd("C:/Yashwanth/Pricing/1.OfficeDepot/10.Rules/1.Rules_data_files/5.Data_v2.3/1.Handsoaps/")
OD.Handsoap_Optimizer <- read.table("Optimizer.csv",header = TRUE, sep = ",", quote = "\"", dec = ".",
                                    fill=TRUE, comment.char="", as.is=TRUE)

OD.Handsoap_Optimizer <- merge(OD.Handsoap_Optimizer,OD.Handsoap_Sales[,c("SKU","FISCAL_WEEK_14_OF_2015","FISCAL_WEEK_13_OF_2015")],by="SKU",all.x=T)
OD.Handsoap_Optimizer <- merge(OD.Handsoap_Optimizer,OD.Handsoap_Price_Prev_Week[,c("SKU","od_final_price")],by="SKU",all.x=T)
OD.Handsoap_Optimizer <- merge(OD.Handsoap_Optimizer,OD.Handsoap_Price_Latest[,c("SKU","staples_final_price")],by="SKU",all.x=T)
OD.Handsoap_Optimizer <- merge(OD.Handsoap_Optimizer,OD.Handsoap_Price_Prev_Week[,c("SKU","staples_final_price")],by="SKU",all.x=T)
OD.Handsoap_Optimizer <- merge(OD.Handsoap_Optimizer,OD.Handsoap_Cost[,c("SKU","WTD_UNIT_COST")],by="SKU",all.x=T)
OD.Handsoap_Optimizer <- merge(OD.Handsoap_Optimizer,OD.Handsoap_Rule_1234[c("SKU","Final_Floor","Final_CAP")],by="SKU",all.x=T)

library(plyr)
OD.Handsoap_Optimizer <- rename(OD.Handsoap_Optimizer,c("FISCAL_WEEK_14_OF_2015"="S1_Last_weeks_sales",
                                                        "FISCAL_WEEK_13_OF_2015"="S0_Last_to_last_weeks_sales","od_final_price"="ODP_1_Last_weeks_OD_Price",
                                                        "staples_final_price.x"="STP_2_Latest_Staples_Price","staples_final_price.y"="STP_1_Last_weeks_Staples_price",
                                                        "WTD_UNIT_COST"="Cost","Final_Floor"="ODP_2_Floor","Final_CAP"="ODP_2_Cap"))

# -----------------------------------------------------------------------------------------------------------------------------------------------#
#                                                     || Data Preparation ||
# -----------------------------------------------------------------------------------------------------------------------------------------------#

OD.Handsoap_Optimizer$ODP_1_Last_weeks_OD_Price[is.na(OD.Handsoap_Optimizer$ODP_1_Last_weeks_OD_Price)] <- "N/A"
OD.Handsoap_Optimizer$STP_2_Latest_Staples_Price[is.na(OD.Handsoap_Optimizer$STP_2_Latest_Staples_Price)] <- "N/A"
OD.Handsoap_Optimizer$STP_1_Last_weeks_Staples_price[is.na(OD.Handsoap_Optimizer$STP_1_Last_weeks_Staples_price)] <- "N/A"

attach(OD.Handsoap_Optimizer)
OD.Handsoap_Optimizer$STP_2_Latest_Staples_Price <- ifelse(ODP_1_Last_weeks_OD_Price=="N/A",STP_2_Latest_Staples_Price=="N/A",
                                                           ifelse(STP_2_Latest_Staples_Price=="N/A",0,STP_2_Latest_Staples_Price))
OD.Handsoap_Optimizer$STP_1_Last_weeks_Staples_price <- ifelse(ODP_1_Last_weeks_OD_Price=="N/A",STP_1_Last_weeks_Staples_price=="N/A",
                                                               ifelse(STP_1_Last_weeks_Staples_price=="N/A",0,STP_1_Last_weeks_Staples_price))
detach(OD.Handsoap_Optimizer)

OD.Handsoap_Optimizer$STP_2_Latest_Staples_Price <- as.numeric(OD.Handsoap_Optimizer$STP_2_Latest_Staples_Price)
OD.Handsoap_Optimizer$STP_1_Last_weeks_Staples_price <- as.numeric(OD.Handsoap_Optimizer$STP_1_Last_weeks_Staples_price)  
OD.Handsoap_Optimizer$ODP_1_Last_weeks_OD_Price <- as.numeric(OD.Handsoap_Optimizer$ODP_1_Last_weeks_OD_Price)

#   rm(list=setdiff(ls(),c("OD.Handsoap_Rule_1234","OD.Handsoap_Sales","OD.Handsoap_Price_Prev_Week","OD.Handsoap_Price_Latest",
#                          "OD.Handsoap_Cost","OD.Handsoap_Threshold","OD.Handsoap_Optimizer")))
OD.Handsoap_Optimizer$ODP_2_Floor <- as.numeric(as.character(OD.Handsoap_Optimizer$ODP_2_Floor))
OD.Handsoap_Optimizer$ODP_2_Cap <- as.numeric(as.character(OD.Handsoap_Optimizer$ODP_2_Cap))

OD.Handsoap_Optimizer$S1_Last_weeks_sales[is.na(OD.Handsoap_Optimizer$S1_Last_weeks_sales)] <- 0
OD.Handsoap_Optimizer$S0_Last_to_last_weeks_sales[is.na(OD.Handsoap_Optimizer$S0_Last_to_last_weeks_sales)] <- 0

# ----------------------------------------------------------------------------------------------------------------------------------#
#                                                   || ODP values : 0,1,2,3,4,5,6,7,8,9 ||
#                                             = ODP 2 Floor+ODP_value*($ODP 2 Cap-ODP 2 Floor)/9  
# ---------------------------------------------------------------------------------------------------------------------------------#

OD_ODP2_Value_seq <- NA  
for(j in seq(0,10,by=1)){
  OD_ODP2_Value_seq[j] <- j-1
}

OD.Handsoap_Optimizer$OD_ODP2_Value1 <- NA;OD.Handsoap_Optimizer$OD_ODP2_Value2<-NA;OD.Handsoap_Optimizer$OD_ODP2_Value3 <- NA;
OD.Handsoap_Optimizer$OD_ODP2_Value4 <- NA;OD.Handsoap_Optimizer$OD_ODP2_Value5 <- NA;OD.Handsoap_Optimizer$OD_ODP2_Value6 <- NA;
OD.Handsoap_Optimizer$OD_ODP2_Value7 <- NA;OD.Handsoap_Optimizer$OD_ODP2_Value8 <- NA;OD.Handsoap_Optimizer$OD_ODP2_Value9 <- NA;
OD.Handsoap_Optimizer$OD_ODP2_Value10 <- NA;

attach(OD.Handsoap_Optimizer)
for(i in 1:nrow(OD.Handsoap_Optimizer)){
  OD.Handsoap_Optimizer$OD_ODP2_Value1[i]  <- (ODP_2_Floor[i]+OD_ODP2_Value_seq[1]*(ODP_2_Cap[i]-ODP_2_Floor[i])/9);
  OD.Handsoap_Optimizer$OD_ODP2_Value2[i]  <- (ODP_2_Floor[i]+OD_ODP2_Value_seq[2]*(ODP_2_Cap[i]-ODP_2_Floor[i])/9);
  OD.Handsoap_Optimizer$OD_ODP2_Value3[i]  <- (ODP_2_Floor[i]+OD_ODP2_Value_seq[3]*(ODP_2_Cap[i]-ODP_2_Floor[i])/9);
  OD.Handsoap_Optimizer$OD_ODP2_Value4[i]  <- (ODP_2_Floor[i]+OD_ODP2_Value_seq[4]*(ODP_2_Cap[i]-ODP_2_Floor[i])/9);
  OD.Handsoap_Optimizer$OD_ODP2_Value5[i]  <- (ODP_2_Floor[i]+OD_ODP2_Value_seq[5]*(ODP_2_Cap[i]-ODP_2_Floor[i])/9);
  OD.Handsoap_Optimizer$OD_ODP2_Value6[i]  <- (ODP_2_Floor[i]+OD_ODP2_Value_seq[6]*(ODP_2_Cap[i]-ODP_2_Floor[i])/9);
  OD.Handsoap_Optimizer$OD_ODP2_Value7[i]  <- (ODP_2_Floor[i]+OD_ODP2_Value_seq[7]*(ODP_2_Cap[i]-ODP_2_Floor[i])/9);
  OD.Handsoap_Optimizer$OD_ODP2_Value8[i]  <- (ODP_2_Floor[i]+OD_ODP2_Value_seq[8]*(ODP_2_Cap[i]-ODP_2_Floor[i])/9);
  OD.Handsoap_Optimizer$OD_ODP2_Value9[i]  <- (ODP_2_Floor[i]+OD_ODP2_Value_seq[9]*(ODP_2_Cap[i]-ODP_2_Floor[i])/9);
  OD.Handsoap_Optimizer$OD_ODP2_Value10[i]  <- (ODP_2_Floor[i]+OD_ODP2_Value_seq[10]*(ODP_2_Cap[i]-ODP_2_Floor[i])/9);
}    
detach(OD.Handsoap_Optimizer)

# ------------------------------------------------------------------------------------------------------------------------------------------#
#                                               || OD Margin values : 1,2,3,4,5,6,7,8,9,10 ||
#                               =IF(S1_Last_weeks_sales<=0,(EXP(Intercept2-0.03*(ODP_2_Value_1-STP_2_Latest_Staples_Price)+
#                               0.0013*S1_Last_weeks_sales)*(ODP_2_Value_1-Cost)),
#                               (EXP(LN(S1_Last_weeks_sales)+0.03*(ODP_1_Last_weeks_OD_Price+STP_2_Latest_Staples_Price-STP_1_Last_weeks_Staples_price-ODP_2_Value_1)+
#                               0.0013*(S1_Last_weeks_sales-S0_Last_to_last_weeks_sales))*(ODP_2_Value_1-Cost)))
# ------------------------------------------------------------------------------------------------------------------------------------------#

OD.Handsoap_Intercept <- read.table("Intercept.csv",header = TRUE, sep = ",", quote = "\"", dec = ".",
                                    fill=TRUE, comment.char="", as.is=TRUE)
OD.Handsoap_Optimizer <- merge(OD.Handsoap_Optimizer,OD.Handsoap_Intercept[,c("Full_List_of_SKU","Intercept2")],
                               by.x="SKU",by.y=,"Full_List_of_SKU",all.x=T)

OD_ODP2_Margin_seq <- NA  
for(j in seq(1,10,by=1)){
  OD_ODP2_Margin_seq[j] <- j
}

OD.Handsoap_Optimizer$OD_Margin2_Value1 <- NA;OD.Handsoap_Optimizer$OD_Margin2_Value2 <- NA;OD.Handsoap_Optimizer$OD_Margin2_Value3 <- NA
OD.Handsoap_Optimizer$OD_Margin2_Value4 <- NA;OD.Handsoap_Optimizer$OD_Margin2_Value5 <- NA;OD.Handsoap_Optimizer$OD_Margin2_Value6 <- NA
OD.Handsoap_Optimizer$OD_Margin2_Value7 <- NA;OD.Handsoap_Optimizer$OD_Margin2_Value8 <- NA;OD.Handsoap_Optimizer$OD_Margin2_Value9 <- NA
OD.Handsoap_Optimizer$OD_Margin2_Value10 <- NA


attach(OD.Handsoap_Optimizer)
for(i in 1:nrow(OD.Handsoap_Optimizer)){   
  OD.Handsoap_Optimizer$OD_Margin2_Value1[i] <-ifelse(S1_Last_weeks_sales[i]<=0,(exp(Intercept2[i]-
                                                                                       0.03*(OD_ODP2_Value1[i]-STP_2_Latest_Staples_Price[i])+0.0013*S1_Last_weeks_sales[i])*(OD_ODP2_Value1[i]-Cost[i])),
                                                      (exp(log(S1_Last_weeks_sales[i])+0.03*(ODP_1_Last_weeks_OD_Price[i]+STP_2_Latest_Staples_Price[i]-STP_1_Last_weeks_Staples_price[i]-OD_ODP2_Value1[i])+
                                                             0.0013*(S1_Last_weeks_sales[i]-S0_Last_to_last_weeks_sales[i]))*(OD_ODP2_Value1[i]-Cost[i])))
  
  OD.Handsoap_Optimizer$OD_Margin2_Value2[i] <-ifelse(S1_Last_weeks_sales[i]<=0,(exp(Intercept2[i]-
                                                                                       0.03*(OD_ODP2_Value2[i]-STP_2_Latest_Staples_Price[i])+0.0013*S1_Last_weeks_sales[i])*(OD_ODP2_Value2[i]-Cost[i])),
                                                      (exp(log(S1_Last_weeks_sales[i])+0.03*(ODP_1_Last_weeks_OD_Price[i]+STP_2_Latest_Staples_Price[i]-STP_1_Last_weeks_Staples_price[i]-OD_ODP2_Value2[i])+
                                                             0.0013*(S1_Last_weeks_sales[i]-S0_Last_to_last_weeks_sales[i]))*(OD_ODP2_Value2[i]-Cost[i])))
  
  OD.Handsoap_Optimizer$OD_Margin2_Value3[i] <-ifelse(S1_Last_weeks_sales[i]<=0,(exp(Intercept2[i]-
                                                                                       0.03*(OD_ODP2_Value3[i]-STP_2_Latest_Staples_Price[i])+0.0013*S1_Last_weeks_sales[i])*(OD_ODP2_Value3[i]-Cost[i])),
                                                      (exp(log(S1_Last_weeks_sales[i])+0.03*(ODP_1_Last_weeks_OD_Price[i]+STP_2_Latest_Staples_Price[i]-STP_1_Last_weeks_Staples_price[i]-OD_ODP2_Value3[i])+
                                                             0.0013*(S1_Last_weeks_sales[i]-S0_Last_to_last_weeks_sales[i]))*(OD_ODP2_Value3[i]-Cost[i])))
  
  OD.Handsoap_Optimizer$OD_Margin2_Value4[i] <-ifelse(S1_Last_weeks_sales[i]<=0,(exp(Intercept2[i]-
                                                                                       0.03*(OD_ODP2_Value4[i]-STP_2_Latest_Staples_Price[i])+0.0013*S1_Last_weeks_sales[i])*(OD_ODP2_Value4[i]-Cost[i])),
                                                      (exp(log(S1_Last_weeks_sales[i])+0.03*(ODP_1_Last_weeks_OD_Price[i]+STP_2_Latest_Staples_Price[i]-STP_1_Last_weeks_Staples_price[i]-OD_ODP2_Value4[i])+
                                                             0.0013*(S1_Last_weeks_sales[i]-S0_Last_to_last_weeks_sales[i]))*(OD_ODP2_Value4[i]-Cost[i])))
  
  OD.Handsoap_Optimizer$OD_Margin2_Value5[i] <-ifelse(S1_Last_weeks_sales[i]<=0,(exp(Intercept2[i]-
                                                                                       0.03*(OD_ODP2_Value5[i]-STP_2_Latest_Staples_Price[i])+0.0013*S1_Last_weeks_sales[i])*(OD_ODP2_Value5[i]-Cost[i])),
                                                      (exp(log(S1_Last_weeks_sales[i])+0.03*(ODP_1_Last_weeks_OD_Price[i]+STP_2_Latest_Staples_Price[i]-STP_1_Last_weeks_Staples_price[i]-OD_ODP2_Value5[i])+
                                                             0.0013*(S1_Last_weeks_sales[i]-S0_Last_to_last_weeks_sales[i]))*(OD_ODP2_Value5[i]-Cost[i])))
  
  OD.Handsoap_Optimizer$OD_Margin2_Value6[i] <-ifelse(S1_Last_weeks_sales[i]<=0,(exp(Intercept2[i]-
                                                                                       0.03*(OD_ODP2_Value6[i]-STP_2_Latest_Staples_Price[i])+0.0013*S1_Last_weeks_sales[i])*(OD_ODP2_Value6[i]-Cost[i])),
                                                      (exp(log(S1_Last_weeks_sales[i])+0.03*(ODP_1_Last_weeks_OD_Price[i]+STP_2_Latest_Staples_Price[i]-STP_1_Last_weeks_Staples_price[i]-OD_ODP2_Value6[i])+
                                                             0.0013*(S1_Last_weeks_sales[i]-S0_Last_to_last_weeks_sales[i]))*(OD_ODP2_Value6[i]-Cost[i])))
  
  OD.Handsoap_Optimizer$OD_Margin2_Value7[i] <-ifelse(S1_Last_weeks_sales[i]<=0,(exp(Intercept2[i]-
                                                                                       0.03*(OD_ODP2_Value7[i]-STP_2_Latest_Staples_Price[i])+0.0013*S1_Last_weeks_sales[i])*(OD_ODP2_Value7[i]-Cost[i])),
                                                      (exp(log(S1_Last_weeks_sales[i])+0.03*(ODP_1_Last_weeks_OD_Price[i]+STP_2_Latest_Staples_Price[i]-STP_1_Last_weeks_Staples_price[i]-OD_ODP2_Value7[i])+
                                                             0.0013*(S1_Last_weeks_sales[i]-S0_Last_to_last_weeks_sales[i]))*(OD_ODP2_Value7[i]-Cost[i])))
  
  OD.Handsoap_Optimizer$OD_Margin2_Value8[i] <-ifelse(S1_Last_weeks_sales[i]<=0,(exp(Intercept2[i]-
                                                                                       0.03*(OD_ODP2_Value8[i]-STP_2_Latest_Staples_Price[i])+0.0013*S1_Last_weeks_sales[i])*(OD_ODP2_Value8[i]-Cost[i])),
                                                      (exp(log(S1_Last_weeks_sales[i])+0.03*(ODP_1_Last_weeks_OD_Price[i]+STP_2_Latest_Staples_Price[i]-STP_1_Last_weeks_Staples_price[i]-OD_ODP2_Value8[i])+
                                                             0.0013*(S1_Last_weeks_sales[i]-S0_Last_to_last_weeks_sales[i]))*(OD_ODP2_Value8[i]-Cost[i])))
  
  OD.Handsoap_Optimizer$OD_Margin2_Value9[i] <-ifelse(S1_Last_weeks_sales[i]<=0,(exp(Intercept2[i]-
                                                                                       0.03*(OD_ODP2_Value9[i]-STP_2_Latest_Staples_Price[i])+0.0013*S1_Last_weeks_sales[i])*(OD_ODP2_Value9[i]-Cost[i])),
                                                      (exp(log(S1_Last_weeks_sales[i])+0.03*(ODP_1_Last_weeks_OD_Price[i]+STP_2_Latest_Staples_Price[i]-STP_1_Last_weeks_Staples_price[i]-OD_ODP2_Value9[i])+
                                                             0.0013*(S1_Last_weeks_sales[i]-S0_Last_to_last_weeks_sales[i]))*(OD_ODP2_Value9[i]-Cost[i])))
  
  OD.Handsoap_Optimizer$OD_Margin2_Value10[i] <-ifelse(S1_Last_weeks_sales[i]<=0,(exp(Intercept2[i]-
                                                                                        0.03*(OD_ODP2_Value10[i]-STP_2_Latest_Staples_Price[i])+0.0013*S1_Last_weeks_sales[i])*(OD_ODP2_Value10[i]-Cost[i])),
                                                       (exp(log(S1_Last_weeks_sales[i])+0.03*(ODP_1_Last_weeks_OD_Price[i]+STP_2_Latest_Staples_Price[i]-STP_1_Last_weeks_Staples_price[i]-OD_ODP2_Value10[i])+
                                                              0.0013*(S1_Last_weeks_sales[i]-S0_Last_to_last_weeks_sales[i]))*(OD_ODP2_Value10[i]-Cost[i])))
}  
detach(OD.Handsoap_Optimizer)

OD.Handsoap_Optimizer <- OD.Handsoap_Optimizer[,grep("Intercept",names(OD.Handsoap_Optimizer),value=T,invert=T)]

# -----------------------------------------------------------------------------------------------------------------------------------------------#
#                                                             ||  Margins ||
# -----------------------------------------------------------------------------------------------------------------------------------------------#

# Max margin  
OD.Handsoap_Optimizer$Max_Margin2 <- NA
for(i in 1:nrow(OD.Handsoap_Optimizer)){
  OD.Handsoap_Optimizer$Max_Margin2[i] <- max(OD.Handsoap_Optimizer[i,grep("Margin",names(OD.Handsoap_Optimizer),value=T)],na.rm=T)
}

OD.Handsoap_Optimizer$Max_Margin2 <- as.numeric(ifelse(OD.Handsoap_Optimizer$Max_Margin2=="-Inf","NA",
                                                       OD.Handsoap_Optimizer$Max_Margin2))

# ODP2 for Max Margin
OD.Handsoap_Optimizer_MM <- OD.Handsoap_Optimizer[,grep(paste(toMatch <- c("OD_Margin","Max_Margin2"),collapse="|"),
                                                        names(OD.Handsoap_Optimizer),value=T)]
OD.Handsoap_Optimizer_Value <- OD.Handsoap_Optimizer[,grep("OD_ODP2",names(OD.Handsoap_Optimizer),value=T)]
rownames(OD.Handsoap_Optimizer_MM) <- NULL

ODP_2_for_Max_Margin_2 <- NA

for(j in 1:nrow(OD.Handsoap_Optimizer_MM)){
  ODP_2_for_Max_Margin_2[j] <- list(OD.Handsoap_Optimizer_Value[j,match(OD.Handsoap_Optimizer_MM$Max_Margin2[j],
                                                                        OD.Handsoap_Optimizer_MM[j,grep("OD_Margin",names(OD.Handsoap_Optimizer_MM),value=T)])])
}

ODP_2_for_Max_Margin_2 <- as.numeric(ifelse(ODP_2_for_Max_Margin_2=="NULL",NA,ODP_2_for_Max_Margin_2))

OD.Handsoap_Optimizer <- cbind(OD.Handsoap_Optimizer,ODP_2_for_Max_Margin_2)
rm(OD.Handsoap_Optimizer_MM,OD.Handsoap_Optimizer_Value)

# -----------------------------------------------------------------------------------------------------------------------------------------------#
#                                                     ||  Initial Recommended Price ||
# -----------------------------------------------------------------------------------------------------------------------------------------------#

OD.Handsoap_Optimizer <- merge(OD.Handsoap_Optimizer,OD.Handsoap_Rule_1234[,c("SKU","Latest_OD_Price")],by="SKU",all.x=T)  

OD.Handsoap_Optimizer$ODP_2_Floor2 <- OD.Handsoap_Optimizer$ODP_2_Floor;OD.Handsoap_Optimizer$ODP_2_Cap2 <- OD.Handsoap_Optimizer$ODP_2_Cap

OD.Handsoap_Optimizer <- merge(OD.Handsoap_Optimizer,OD.Handsoap_Rule_1234[,c("SKU","Final_Action")],by="SKU",all.x=T)  
OD.Handsoap_Optimizer$Final_Action[is.na(OD.Handsoap_Optimizer$Final_Action)] <- " "

OD.Handsoap_Optimizer$Initial_Rec_Price <- NA
for(i in 1:nrow(OD.Handsoap_Optimizer)){
  OD.Handsoap_Optimizer$Initial_Rec_Price[i] <- ifelse(is.na(OD.Handsoap_Optimizer$ODP_2_for_Max_Margin_2[i]),
                                                       OD.Handsoap_Optimizer$Latest_OD_Price[i],OD.Handsoap_Optimizer$ODP_2_for_Max_Margin_2[i])
}
# Initial_Rec_Price <- round(ifelse(Initial_Rec_Price=="-Inf",NA,Initial_Rec_Price),3)

# -----------------------------------------------------------------------------------------------------------------------------------------------#
#                                                     ||  Flag, Volume change & Price change ||
# -----------------------------------------------------------------------------------------------------------------------------------------------#

OD.Handsoap_Optimizer$New_Cost <- OD.Handsoap_Optimizer$Cost
OD.Handsoap_Optimizer$New_Cost[is.na(OD.Handsoap_Optimizer$New_Cost)] <- 1000
OD.Handsoap_Optimizer$New_ODP_2_Floor2 <- OD.Handsoap_Optimizer$ODP_2_Floor2
OD.Handsoap_Optimizer$New_ODP_2_Floor2[is.na(OD.Handsoap_Optimizer$New_ODP_2_Floor2)] <- 1000
OD.Handsoap_Optimizer$New_ODP_2_Cap2 <- OD.Handsoap_Optimizer$ODP_2_Cap2
OD.Handsoap_Optimizer$New_ODP_2_Cap2[is.na(OD.Handsoap_Optimizer$New_ODP_2_Cap2)] <- 1000  
OD.Handsoap_Optimizer$New_Max_Margin2 <- OD.Handsoap_Optimizer$Max_Margin2
OD.Handsoap_Optimizer$New_Max_Margin2[is.na(OD.Handsoap_Optimizer$New_Max_Margin2)] <- 1000 

# Flag
OD.Handsoap_Optimizer$ODP_1_Last_weeks_OD_Price[is.na(OD.Handsoap_Optimizer$ODP_1_Last_weeks_OD_Price)] <- "N/A"

OD.Handsoap_Optimizer$Flag <- NA
attach(OD.Handsoap_Optimizer)
for(i in 1:nrow(OD.Handsoap_Optimizer)){
  OD.Handsoap_Optimizer$Flag[i] <-
    ifelse(New_Cost[i]==1000,"Cost not available",ifelse(ODP_1_Last_weeks_OD_Price[i]=="N/A" & New_ODP_2_Floor2[i]==1000,"Error in Floor / Cap",
                                                         ifelse(New_ODP_2_Floor2[i]==1000,"No optimization done",ifelse(New_Max_Margin2[i]==1000,"Margin calculation could not be done",
                                                                                                                        ifelse(New_ODP_2_Floor2[i]==Initial_Rec_Price[i],"Initial Reco = Floor",
                                                                                                                               ifelse(New_ODP_2_Cap2[i]==Initial_Rec_Price[i],"Initial Reco = Cap","Disctinct Initial Reco"))))))
}
detach(OD.Handsoap_Optimizer)

OD.Handsoap_Optimizer$ODP_1_Last_weeks_OD_Price <- as.numeric(OD.Handsoap_Optimizer$ODP_1_Last_weeks_OD_Price)
OD.Handsoap_Optimizer <- OD.Handsoap_Optimizer[,grep("New",names(OD.Handsoap_Optimizer),value=T,invert=TRUE)]

# Volume Change
OD.Handsoap_Optimizer$Volume_Change <- NA
attach(OD.Handsoap_Optimizer)
for(i in 1:nrow(OD.Handsoap_Optimizer)){
  OD.Handsoap_Optimizer$Volume_Change[i] <- ifelse(Initial_Rec_Price[i]!=ODP_1_Last_weeks_OD_Price[i],
                                                   (Max_Margin2[i]/(ODP_2_for_Max_Margin_2[i]-Cost[i])/S1_Last_weeks_sales[i]),1)
}
detach(OD.Handsoap_Optimizer)
OD.Handsoap_Optimizer$Volume_Change <- paste(round(100*OD.Handsoap_Optimizer$Volume_Change,0),"%",sep="")
OD.Handsoap_Optimizer$Volume_Change <- ifelse(OD.Handsoap_Optimizer$Volume_Change=="NA%","#N/A",
                                              ifelse(OD.Handsoap_Optimizer$Volume_Change=="Inf%","#DIV/0!",OD.Handsoap_Optimizer$Volume_Change))

# Price Change
OD.Handsoap_Optimizer$Price_Change <- OD.Handsoap_Optimizer$Initial_Rec_Price/OD.Handsoap_Optimizer$Latest_OD_Price 
OD.Handsoap_Optimizer$Price_Change <- paste(round(100*OD.Handsoap_Optimizer$Price_Change,0),"%",sep="")
OD.Handsoap_Optimizer$Price_Change <- ifelse(OD.Handsoap_Optimizer$Price_Change=="NA%","#N/A",OD.Handsoap_Optimizer$Price_Change)

# -----------------------------------------------------------------------------------------------------------------------------------------------#
#                                                             ||  Export to Excel ||
# -----------------------------------------------------------------------------------------------------------------------------------------------#

save.image("~/OD Dynamic Pricing/OD_DynamicPricing/2.Rules/1.Handsoaps/Optimizer_v2.3.RData")

library(xlsx)
write.xlsx(OD.Handsoap_Optimizer,"C:/Yashwanth/Pricing/1.OfficeDepot/10.Rules/3.Execution/1.Handsoaps/Handsoap_execution_R_v2.3.xlsx",
           row.names=FALSE,sheetName="Optimizer",append=TRUE)


#------------------------------------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------------------------------
#------------------------- END ------------------------ Rules Implementation : Optimizer ---------------------------- END -----------------------
#------------------------------------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------------------------------
# -----------------------------------------------------------------------------------------------------------------------------------------------#
# -----------------------------------------------------------------------------------------------------------------------------------------------#
# -----------------------------------------------------------------------------------------------------------------------------------------------#
#                                                   || Rules Implementation : Post Processing Rules ||
# -----------------------------------------------------------------------------------------------------------------------------------------------#
# -----------------------------------------------------------------------------------------------------------------------------------------------#
# -----------------------------------------------------------------------------------------------------------------------------------------------#

rm(list=ls())
load("~/OD Dynamic Pricing/OD_DynamicPricing/2.Rules/1.Handsoaps/Optimizer_v2.3.RData")
rm(list=setdiff(ls(),c("OD.Handsoap_Rule_1234","OD.Handsoap_Sales","OD.Handsoap_Price_Prev_Week","OD.Handsoap_Price_Latest",
                       "OD.Handsoap_Cost","OD.Handsoap_Threshold","OD.Handsoap_Optimizer","OD.Handsoap_Intercept")))

library(plyr)

# Line rule

setwd("C:/Yashwanth/Pricing/1.OfficeDepot/10.Rules/1.Rules_data_files/5.Data_v2.3/1.Handsoaps/")

OD.Line_Family2 <- read.table("Post_Proc_Line_Family2.csv",header = TRUE, sep = ",", quote = "\"", dec = ".",
                              fill=TRUE, comment.char="", as.is=TRUE)

OD.Line_Family3 <- read.table("Post_Proc_Line_Family3.csv",header = TRUE, sep = ",", quote = "\"", dec = ".",
                              fill=TRUE, comment.char="", as.is=TRUE)

OD.Line_Family4 <- read.table("Post_Proc_Line_Family4.csv",header = TRUE, sep = ",", quote = "\"", dec = ".",
                              fill=TRUE, comment.char="", as.is=TRUE)

OD.Line_Family6 <- read.table("Post_Proc_Line_Family6.csv",header = TRUE, sep = ",", quote = "\"", dec = ".",
                              fill=TRUE, comment.char="", as.is=TRUE)

OD.Line_Family7 <- read.table("Post_Proc_Line_Family7.csv",header = TRUE, sep = ",", quote = "\"", dec = ".",
                              fill=TRUE, comment.char="", as.is=TRUE)

OD.Line_Family8 <- read.table("Post_Proc_Line_Family8.csv",header = TRUE, sep = ",", quote = "\"", dec = ".",
                              fill=TRUE, comment.char="", as.is=TRUE)

OD.Line_Family9 <- read.table("Post_Proc_Line_Family9.csv",header = TRUE, sep = ",", quote = "\"", dec = ".",
                              fill=TRUE, comment.char="", as.is=TRUE)

OD.Line_Brand_Family_12 <- read.table("Post_Proc_Brand_Family_12.csv",header = TRUE, sep = ",", quote = "\"", dec = ".",
                                      fill=TRUE, comment.char="", as.is=TRUE)

# -----------------------------------------------------------------------------------------------------------------------------------------------#
#                                               ||  Initial Recommended Price & Final Recommendation ||
# -----------------------------------------------------------------------------------------------------------------------------------------------#

# Family 2
OD.Line_Family2 <- merge(OD.Line_Family2,OD.Handsoap_Optimizer[,c("SKU","Initial_Rec_Price")],by="SKU",all.x=T)
OD.Line_Family2$Number_SKU <- OD.Line_Family2$SKU

OD.Line_Family2$Final_Recommended_Price <- NA
for(i in 1:nrow(OD.Line_Family2)){
  OD.Line_Family2$Final_Recommended_Price[i] <- ifelse(OD.Line_Family2$Conflict_Resolution[i]=="Primary",
                                                       OD.Line_Family2$Initial_Rec_Price[3],OD.Line_Family2$Initial_Rec_Price[3])
}

# Family 3
OD.Line_Family3 <- merge(OD.Line_Family3,OD.Handsoap_Optimizer[,c("SKU","Initial_Rec_Price")],by="SKU",all.x=T)
OD.Line_Family3$Number_SKU <- OD.Line_Family3$SKU

OD.Line_Family3$Final_Recommended_Price <- NA
for(i in 1:nrow(OD.Line_Family3)){
  OD.Line_Family3$Final_Recommended_Price[i] <- ifelse(OD.Line_Family3$Conflict_Resolution[i]=="Primary",
                                                       OD.Line_Family3$Initial_Rec_Price[2],OD.Line_Family3$Initial_Rec_Price[2])
}

# Family 4
OD.Line_Family4 <- merge(OD.Line_Family4,OD.Handsoap_Optimizer[,c("SKU","Initial_Rec_Price")],by="SKU",all.x=T)
OD.Line_Family4$Number_SKU <- OD.Line_Family4$SKU

OD.Line_Family4$Final_Recommended_Price <- NA
for(i in 1:nrow(OD.Line_Family4)){
  OD.Line_Family4$Final_Recommended_Price[i] <- ifelse(OD.Line_Family4$Conflict_Resolution[i]=="Primary",
                                                       OD.Line_Family4$Initial_Rec_Price[2],OD.Line_Family4$Initial_Rec_Price[2])
}

# Family 6
OD.Line_Family6 <- merge(OD.Line_Family6,OD.Handsoap_Optimizer[,c("SKU","Initial_Rec_Price")],by="SKU",all.x=T)
OD.Line_Family6$Number_SKU <- OD.Line_Family6$SKU

OD.Line_Family6$Final_Recommended_Price <- NA
for(i in 1:nrow(OD.Line_Family6)){
  OD.Line_Family6$Final_Recommended_Price[i] <- ifelse(OD.Line_Family6$Conflict_Resolution[i]=="Primary",
                                                       OD.Line_Family6$Initial_Rec_Price[2],OD.Line_Family6$Initial_Rec_Price[2])
}

# Family 7
OD.Line_Family7 <- merge(OD.Line_Family7,OD.Handsoap_Optimizer[,c("SKU","Initial_Rec_Price")],by="SKU",all.x=T)
OD.Line_Family7$Number_SKU <- OD.Line_Family7$SKU

OD.Line_Family7$Final_Recommended_Price <- NA
for(i in 1:nrow(OD.Line_Family7)){
  OD.Line_Family7$Final_Recommended_Price[i] <- ifelse(OD.Line_Family7$Conflict_Resolution[i]=="Primary",
                                                       OD.Line_Family7$Initial_Rec_Price[1],OD.Line_Family7$Initial_Rec_Price[1])
}

# Family 8
OD.Line_Family8 <- merge(OD.Line_Family8,OD.Handsoap_Optimizer[,c("SKU","Initial_Rec_Price")],by="SKU",all.x=T)
OD.Line_Family8$Number_SKU <- OD.Line_Family8$SKU

OD.Line_Family8$Final_Recommended_Price <- NA
for(i in 1:nrow(OD.Line_Family8)){
  OD.Line_Family8$Final_Recommended_Price[i] <- ifelse(OD.Line_Family8$Conflict_Resolution[i]=="Primary",
                                                       OD.Line_Family8$Initial_Rec_Price[3],OD.Line_Family8$Initial_Rec_Price[3])
}

# Family 9  : Ignore Error
OD.Line_Family9 <- merge(OD.Line_Family9,OD.Handsoap_Optimizer[,c("SKU","Initial_Rec_Price")],by="SKU",all.x=T)
OD.Line_Family9$Number_SKU <- OD.Line_Family9$SKU

OD.Line_Family9$Final_Recommended_Price <- NA
for(i in 1:nrow(OD.Line_Family9)){
  OD.Line_Family9$Final_Recommended_Price[i] <- ifelse(OD.Line_Family9$Conflict_Resolution[i]=="Primary",
                                                       OD.Line_Family9$Initial_Rec_Price[2],OD.Line_Family9$Initial_Rec_Price[2])
}

# Brand Family1 & Family 2  
OD.Line_Brand_Family_12 <- merge(OD.Line_Brand_Family_12,OD.Handsoap_Optimizer[,c("SKU","Initial_Rec_Price")],by="SKU",all.x=T)
OD.Line_Brand_Family_12$Number_SKU <- OD.Line_Brand_Family_12$SKU

attach(OD.Line_Brand_Family_12)
OD.Line_Brand_Family_12$Final_Recommended_Price <- ifelse(Brand_Class=="PRIVATE",
                                                          ifelse(Initial_Rec_Price<Initial_Rec_Price[2] & Initial_Rec_Price<Initial_Rec_Price[3],Initial_Rec_Price,
                                                                 0.99*min(Initial_Rec_Price[2],Initial_Rec_Price[3])),Initial_Rec_Price)
detach(OD.Line_Brand_Family_12)

OD.Line_Brand_Family_12 <- rename(OD.Line_Brand_Family_12,c("Brand_Class"="Conflict_Resolution"))

# -----------------------------------------------------------------------------------------------------------------------------------------------#
#                                                       ||  Export to Excel ||
# -----------------------------------------------------------------------------------------------------------------------------------------------#

library(xlsx)

OD.DP_Post_Proc <- rbind(OD.Line_Family2,OD.Line_Family3,OD.Line_Family4,OD.Line_Family6,OD.Line_Family7,OD.Line_Family8,
                         OD.Line_Family9,OD.Line_Brand_Family_12)
OD.DP_Post_Proc$Initial_Rec_Price <- round(OD.DP_Post_Proc$Initial_Rec_Price,2)
OD.DP_Post_Proc$Final_Recommended_Price <- round(OD.DP_Post_Proc$Final_Recommended_Price,2)

save.image("~/OD Dynamic Pricing/OD_DynamicPricing/2.Rules/1.Handsoaps/Post_Proc_Rules_v2.3.RData")

save.xlsx <- function (file, ...)
{
  require(xlsx, quietly = TRUE)
  objects <- list(...)
  fargs <- as.list(match.call(expand.dots = TRUE))
  objnames <- as.character(fargs)[-c(1, 2)]
  nobjects <- length(objects)
  for (i in 1:nobjects) {
    if (i == 1)
      write.xlsx(objects[[i]], file, sheetName = objnames[i],row.names=FALSE)
    else write.xlsx(objects[[i]], file, sheetName = objnames[i],row.names=FALSE,append = TRUE)
  }
  print(paste("Workbook", file, "has", nobjects, "worksheets."))
}

save.xlsx("C:/Yashwanth/Pricing/1.OfficeDepot/10.Rules/1.Rules_data_files/5.Data_v2.3/1.Handsoaps/Handsoaps_Post_Proc2.3.xlsx",
          OD.Line_Family2,OD.Line_Family3,OD.Line_Family4,OD.Line_Family6,OD.Line_Family7,OD.Line_Family8,OD.Line_Family9,
          OD.Line_Brand_Family_12,OD.DP_Post_Proc)

write.xlsx(OD.DP_Post_Proc,"C:/Yashwanth/Pricing/1.OfficeDepot/10.Rules/3.Execution/1.Handsoaps/Handsoap_execution_R_v2.3.xlsx",
           row.names=FALSE,sheetName="Post Processing",append=TRUE)

#------------------------------------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------------------------------
#-------------------- END ------------------------ Rules Implementation : Post Processing Rules ---------------------------- END ----------------
#------------------------------------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------------------------------
# -----------------------------------------------------------------------------------------------------------------------------------------------#
# -----------------------------------------------------------------------------------------------------------------------------------------------#
# -----------------------------------------------------------------------------------------------------------------------------------------------#
#                                                   || Rules Implementation : Final Delivery ||
# -----------------------------------------------------------------------------------------------------------------------------------------------#
# -----------------------------------------------------------------------------------------------------------------------------------------------#
# -----------------------------------------------------------------------------------------------------------------------------------------------#

rm(list=ls())
load("~/OD Dynamic Pricing/OD_DynamicPricing/2.Rules/1.Handsoaps/Post_Proc_Rules_v2.3.RData")
rm(list=setdiff(ls(),c("OD.Handsoap_Rule_1234","OD.Handsoap_Sales","OD.Handsoap_Price_Prev_Week","OD.Handsoap_Price_Latest",
                       "OD.Handsoap_Cost","OD.Handsoap_Threshold","OD.Handsoap_Optimizer","OD.DP_Post_Proc","OD.Handsoap_Intercept")))

setwd("C:/Yashwanth/Pricing/1.OfficeDepot/10.Rules/1.Rules_data_files/5.Data_v2.3/1.Handsoaps/")
OD.Handsoap_Promotions <- read.table("Promotions.csv",header = TRUE, sep = ",", quote = "\"", dec = ".",
                                     fill=TRUE, comment.char="", as.is=TRUE)
OD.Handsoap_Final_Del <- read.table("Final_Delivery.csv",header = TRUE, sep = ",", quote = "\"", dec = ".",
                                    fill=TRUE, comment.char="", as.is=TRUE)
OD.Handsoap_Final_Del$Date_changed[is.na(OD.Handsoap_Final_Del$Date_changed)] <- " "

# Current OD Price & Final Action
library(plyr)
OD.Handsoap_Final_Del <- merge(OD.Handsoap_Final_Del,OD.Handsoap_Price_Latest[,c("SKU","od_final_price")],by="SKU",all.x=T)
OD.Handsoap_Final_Del <- merge(OD.Handsoap_Final_Del,OD.Handsoap_Optimizer[,c("SKU","Final_Action")],by="SKU",all.x=T)
OD.Handsoap_Final_Del <- rename(OD.Handsoap_Final_Del,c("od_final_price"="Current_OD_Price"))
# OD.Handsoap_Final_Del$Final_Action <- ifelse(OD.Handsoap_Final_Del$SKU=="521845",OD.Handsoap_Final_Del$Final_Action==NA,
#                                              OD.Handsoap_Final_Del$Final_Action)

# Post Processing Price
OD.Handsoap_Final_Del <- merge(OD.Handsoap_Final_Del,OD.DP_Post_Proc[,c("SKU","Final_Recommended_Price")],by="SKU",all.x=T)
OD.Handsoap_Final_Del <- rename(OD.Handsoap_Final_Del,c("Final_Recommended_Price"="Post_Processing_Price"))
OD.Handsoap_Final_Del$Post_Processing_Price[is.na(OD.Handsoap_Final_Del$Post_Processing_Price)] <- " "

# Final Recommendation Price
OD.Handsoap_Final_Del <-  merge(OD.Handsoap_Final_Del,OD.Handsoap_Optimizer[,c("SKU","Initial_Rec_Price")],by="SKU",all.x=T)
OD.Handsoap_Final_Del$Final_Recommendation <- as.numeric(ifelse(OD.Handsoap_Final_Del$Post_Processing_Price!=" ",
                                                                OD.Handsoap_Final_Del$Post_Processing_Price,OD.Handsoap_Final_Del$Initial_Rec_Price))

OD.Handsoap_Final_Del <- OD.Handsoap_Final_Del[,grep("Initial",names(OD.Handsoap_Final_Del),value=T,invert=T)]

# -----------------------------------------------------------------------------------------------------------------------------------------------#
#                                                             ||  Output  ||
#                           =IF(ISERROR(B2),"Current OD Price Not Avaiable",IF(OR(Final Action="R",Final Action="I"),
#   IF(Final Recommendation=Current OD Price,"Rule Triggered, No Price Change","Rule Triggered, Price Change"),"Rule Not Triggered"))
# -----------------------------------------------------------------------------------------------------------------------------------------------#

OD.Handsoap_Final_Del$New_Current_OD_Price <- as.numeric(OD.Handsoap_Final_Del$Current_OD_Price)
OD.Handsoap_Final_Del$New_Current_OD_Price[is.na(OD.Handsoap_Final_Del$New_Current_OD_Price)] <- 1000

# IF(ISERROR(B2),"Current OD Price Not Avaiable",IF(OR(C2="R",C2="I"),
# IF(E2=B2,"Rule Triggered, No Price Change","Rule Triggered, Price Change"),"Rule Not Triggered"))
OD.Handsoap_Final_Del$Output <- NA
attach(OD.Handsoap_Final_Del)
for(i in 1:nrow(OD.Handsoap_Final_Del)){
  OD.Handsoap_Final_Del$Output[i] <-ifelse(New_Current_OD_Price[i]==1000,"Current OD Price Not Avaiable",
                                           ifelse(Final_Action[i]=="R" | Final_Action[i]=="I",
                                                  ifelse(Final_Recommendation[i]==New_Current_OD_Price[i],"Rule Triggered, No Price Change","Rule Triggered, Price Change"),
                                                  "Rule Not Triggered"))
}
detach(OD.Handsoap_Final_Del)

OD.Handsoap_Final_Del <- OD.Handsoap_Final_Del[,grep("New",names(OD.Handsoap_Final_Del),value=T,invert=T)]
OD.Handsoap_Final_Del$Final_Recommendation <- round(OD.Handsoap_Final_Del$Final_Recommendation,1)-0.01

# -----------------------------------------------------------------------------------------------------------------------------------------------#
#                                                       ||  Other Attributes ||
# -----------------------------------------------------------------------------------------------------------------------------------------------#

# Promotions Flag
OD.Handsoap_Final_Del <- merge(OD.Handsoap_Final_Del,OD.Handsoap_Promotions,all.x=T)
OD.Handsoap_Final_Del <- rename(OD.Handsoap_Final_Del,c("Max_Week"="Promotion_Flag"))
OD.Handsoap_Final_Del$Promotion_Flag[is.na(OD.Handsoap_Final_Del$Promotion_Flag)] <- " "

# Fiscal Week
OD.Handsoap_Final_Del <- merge(OD.Handsoap_Final_Del,OD.Handsoap_Promotions[,c("SKU","Max_Week")],by="SKU",all.x=T)
OD.Handsoap_Final_Del <- rename(OD.Handsoap_Final_Del,c("Max_Week"="Fiscal_Week"))

for(i in 1:nrow(OD.Handsoap_Final_Del)){
  OD.Handsoap_Final_Del$Fiscal_Week[i] <-ifelse(OD.Handsoap_Final_Del$Promotion_Flag[i]!=" ",OD.Handsoap_Final_Del$Fiscal_Week[i]," ")
}

# Price given to office depot
OD.Handsoap_Final_Del$Price_given_to_Office_Depot <- NA
attach(OD.Handsoap_Final_Del)
for(i in 1:nrow(OD.Handsoap_Final_Del)){
  OD.Handsoap_Final_Del$Price_given_to_Office_Depot[i] <- ifelse(Test_Control_Flag[i]=="C",Current_OD_Price[i],
                                                                 ifelse(Fiscal_Week[i]!=" ",Current_OD_Price[i],Final_Recommendation[i]))
}
detach(OD.Handsoap_Final_Del)

# Intial Recommendation 
OD.Handsoap_Final_Del <- merge(OD.Handsoap_Final_Del,OD.Handsoap_Optimizer[,c("SKU","Initial_Rec_Price")],by="SKU",all.x=T)
OD.Handsoap_Final_Del$Initial_Rec_Price <- round(OD.Handsoap_Final_Del$Initial_Rec_Price,3)

# Volume Change
OD.Handsoap_Final_Del <- merge(OD.Handsoap_Final_Del,OD.Handsoap_Optimizer[,c("SKU","S1_Last_weeks_sales","S0_Last_to_last_weeks_sales",
                                                                              "ODP_1_Last_weeks_OD_Price","STP_2_Latest_Staples_Price","STP_1_Last_weeks_Staples_price")],by="SKU",all.x=T)

OD.Handsoap_Final_Del$Volume_Change <- NA
attach(OD.Handsoap_Final_Del)
for(i in 1:nrow(OD.Handsoap_Final_Del)){
  OD.Handsoap_Final_Del$Volume_Change[i] <- ifelse(Current_OD_Price[i]==Price_given_to_Office_Depot[i],0,(exp(log(S1_Last_weeks_sales[i])+
                                                                                                                0.03*(ODP_1_Last_weeks_OD_Price[i]+STP_2_Latest_Staples_Price[i]-STP_1_Last_weeks_Staples_price[i]-Price_given_to_Office_Depot[i])+
                                                                                                                0.0013*(S1_Last_weeks_sales[i]-S0_Last_to_last_weeks_sales[i]))/S1_Last_weeks_sales[i]))
}
detach(OD.Handsoap_Final_Del)
OD.Handsoap_Final_Del$Volume_Change <- paste(round(100*OD.Handsoap_Final_Del$Volume_Change,0),"%",sep="")
OD.Handsoap_Final_Del$Volume_Change <- ifelse(OD.Handsoap_Final_Del$Volume_Change=="NA%" | OD.Handsoap_Final_Del$Volume_Change=="NaN%",
                                              "#N/A",OD.Handsoap_Final_Del$Volume_Change)

# Price Change
OD.Handsoap_Final_Del$Price_Change <- OD.Handsoap_Final_Del$Price_given_to_Office_Depot/OD.Handsoap_Final_Del$Current_OD_Price
OD.Handsoap_Final_Del$Price_Change <- paste(round(100*OD.Handsoap_Final_Del$Price_Change,0),"%",sep="")
OD.Handsoap_Final_Del$Price_Change <- ifelse(OD.Handsoap_Final_Del$Price_Change=="NA%"| OD.Handsoap_Final_Del$Price_Change=="NaN%",
                                             "#N/A",OD.Handsoap_Final_Del$Price_Change)

# Cost & Rule Triggered
OD.Handsoap_Final_Del <- merge(OD.Handsoap_Final_Del,OD.Handsoap_Cost[,c("SKU","WTD_UNIT_COST")],by="SKU",all.x=T)
OD.Handsoap_Final_Del <- merge(OD.Handsoap_Final_Del,OD.Handsoap_Rule_1234[,c("SKU","Rules_Triggered")],by="SKU",all.x=T)
OD.Handsoap_Final_Del <- rename(OD.Handsoap_Final_Del,c("WTD_UNIT_COST"="Cost"))
OD.Handsoap_Final_Del$Cost <- round(OD.Handsoap_Final_Del$Cost,2)

# Weekwise Volume
OD.Handsoap_Final_Del$Last_Week_Volume <- OD.Handsoap_Final_Del$S1_Last_weeks_sales

OD.Handsoap_Final_Del$This_Week_Volume <- NA
attach(OD.Handsoap_Final_Del)
for(i in 1:nrow(OD.Handsoap_Final_Del)){
  OD.Handsoap_Final_Del$This_Week_Volume[i] <- 
    ifelse(is.na(exp(log(S1_Last_weeks_sales[i])+
                       0.03*(as.numeric(ODP_1_Last_weeks_OD_Price)[i]+as.numeric(STP_2_Latest_Staples_Price)[i]-as.numeric(STP_1_Last_weeks_Staples_price)[i]-as.numeric(Price_given_to_Office_Depot)[i])+
                       0.0013*(as.numeric(S1_Last_weeks_sales)[i]-as.numeric(S0_Last_to_last_weeks_sales)[i]))),S1_Last_weeks_sales[i],exp(log(as.numeric(S1_Last_weeks_sales)[i])+
                                                                                                                                             0.03*(as.numeric(ODP_1_Last_weeks_OD_Price)[i]+as.numeric(STP_2_Latest_Staples_Price)[i]-as.numeric(STP_1_Last_weeks_Staples_price)[i]-as.numeric(Price_given_to_Office_Depot)[i])+
                                                                                                                                             0.0013*(as.numeric(S1_Last_weeks_sales)[i]-as.numeric(S0_Last_to_last_weeks_sales)[i])))
}
detach(OD.Handsoap_Final_Del)

# Weekwise Revenue
OD.Handsoap_Final_Del$Last_Week_Revenue <- OD.Handsoap_Final_Del$Last_Week_Volume*OD.Handsoap_Final_Del$ODP_1_Last_weeks_OD_Price
OD.Handsoap_Final_Del$Last_Week_Revenue[is.na(OD.Handsoap_Final_Del$Last_Week_Revenue)] <- 0
OD.Handsoap_Final_Del$This_Week_Revenue <- OD.Handsoap_Final_Del$This_Week_Volume*OD.Handsoap_Final_Del$Price_given_to_Office_Depot
OD.Handsoap_Final_Del$This_Week_Revenue[is.na(OD.Handsoap_Final_Del$This_Week_Revenue)] <- 0

# Weekwise Margin
OD.Handsoap_Final_Del$Last_Week_Margin <- OD.Handsoap_Final_Del$Last_Week_Volume*(OD.Handsoap_Final_Del$ODP_1_Last_weeks_OD_Price-OD.Handsoap_Final_Del$Cost)
OD.Handsoap_Final_Del$Last_Week_Margin[is.na(OD.Handsoap_Final_Del$Last_Week_Margin)] <- 0
OD.Handsoap_Final_Del$This_Week_Margin <- OD.Handsoap_Final_Del$This_Week_Volume*(OD.Handsoap_Final_Del$Price_given_to_Office_Depot-OD.Handsoap_Final_Del$Cost)
OD.Handsoap_Final_Del$This_Week_Margin[is.na(OD.Handsoap_Final_Del$This_Week_Margin)] <- 0

# Weekwise Price
OD.Handsoap_Final_Del$Last_Week_Price <- ifelse(is.na(OD.Handsoap_Final_Del$ODP_1_Last_weeks_OD_Price)," ",
                                                OD.Handsoap_Final_Del$ODP_1_Last_weeks_OD_Price)
OD.Handsoap_Final_Del$This_Week_Price <- ifelse(is.na(OD.Handsoap_Final_Del$Price_given_to_Office_Depot)," ",
                                                OD.Handsoap_Final_Del$Price_given_to_Office_Depot)


# Arrange Columns

OD.Handsoap_Final_Del <- OD.Handsoap_Final_Del[,c("SKU","Current_OD_Price","Final_Action","Post_Processing_Price","Final_Recommendation",
                                                  "Output","Promotion_Flag","Fiscal_Week","Test_Control_Flag","Price_given_to_Office_Depot",
                                                  "Date_given","Date_changed","Initial_Rec_Price","Volume_Change","Price_Change",
                                                  "S1_Last_weeks_sales","S0_Last_to_last_weeks_sales","ODP_1_Last_weeks_OD_Price",
                                                  "STP_2_Latest_Staples_Price","STP_1_Last_weeks_Staples_price","Cost","Rules_Triggered",
                                                  "Last_Week_Volume","This_Week_Volume","Last_Week_Revenue","This_Week_Revenue",
                                                  "Last_Week_Margin","This_Week_Margin","Last_Week_Price","This_Week_Price")]

OD.Handsoap_Final_Del[is.na(OD.Handsoap_Final_Del)] <- "#N/A"

# -----------------------------------------------------------------------------------------------------------------------------------------------#
#                                                       ||  Frequency MIS ||
# -----------------------------------------------------------------------------------------------------------------------------------------------#

OD.Handsoap_Freq_MIS <- aggregate(OD.Handsoap_Final_Del$Output,
                                  list(OD.Handsoap_Final_Del$Output,OD.Handsoap_Final_Del$Final_Action),length)
names(OD.Handsoap_Freq_MIS) <- c("Output","Final_Action","Count")

# -----------------------------------------------------------------------------------------------------------------------------------------------#
#                                                       ||  Export to Excel ||
# -----------------------------------------------------------------------------------------------------------------------------------------------#

OD.Handsoap_Last_Fiscal_Week <- read.table("Last_Fiscal_Week.csv",header = TRUE, sep = ",", quote = "\"", dec = ".",
                                           fill=TRUE, comment.char="", as.is=TRUE)

save.image("~/OD Dynamic Pricing/OD_DynamicPricing/2.Rules/1.Handsoaps/Final_Delivery_Freq_MIS_v2.3.RData")

library(xlsx)
write.xlsx(OD.Handsoap_Final_Del,"C:/Yashwanth/Pricing/1.OfficeDepot/10.Rules/3.Execution/1.Handsoaps/Handsoap_execution_R_v2.3.xlsx",
           row.names=FALSE,sheetName="Final Delivery",append=TRUE)

write.xlsx(OD.Handsoap_Last_Fiscal_Week,"C:/Yashwanth/Pricing/1.OfficeDepot/10.Rules/3.Execution/1.Handsoaps/Handsoap_execution_R_v2.3.xlsx",
           row.names=FALSE,sheetName="Last Fiscal Week",append=TRUE)

write.xlsx(OD.Handsoap_Sales,"C:/Yashwanth/Pricing/1.OfficeDepot/10.Rules/3.Execution/1.Handsoaps/Handsoap_execution_R_v2.3.xlsx",
           row.names=FALSE,sheetName="Sales",append=TRUE)

write.xlsx(OD.Handsoap_Cost,"C:/Yashwanth/Pricing/1.OfficeDepot/10.Rules/3.Execution/1.Handsoaps/Handsoap_execution_R_v2.3.xlsx",
           row.names=FALSE,sheetName="Cost",append=TRUE)

write.xlsx(OD.Handsoap_Price_Latest,"C:/Yashwanth/Pricing/1.OfficeDepot/10.Rules/3.Execution/1.Handsoaps/Handsoap_execution_R_v2.3.xlsx",
           row.names=FALSE,sheetName="Price Latest",append=TRUE)

write.xlsx(OD.Handsoap_Price_Prev_Week,"C:/Yashwanth/Pricing/1.OfficeDepot/10.Rules/3.Execution/1.Handsoaps/Handsoap_execution_R_v2.3.xlsx",
           row.names=FALSE,sheetName="Price Prev Week",append=TRUE)

write.xlsx(OD.Handsoap_Threshold,"C:/Yashwanth/Pricing/1.OfficeDepot/10.Rules/3.Execution/1.Handsoaps/Handsoap_execution_R_v2.3.xlsx",
           row.names=FALSE,sheetName="Thresholds",append=TRUE)

write.xlsx(OD.Handsoap_Promotions,"C:/Yashwanth/Pricing/1.OfficeDepot/10.Rules/3.Execution/1.Handsoaps/Handsoap_execution_R_v2.3.xlsx",
           row.names=FALSE,sheetName="Promotions",append=TRUE)

write.xlsx(OD.Handsoap_Intercept,"C:/Yashwanth/Pricing/1.OfficeDepot/10.Rules/3.Execution/1.Handsoaps/Handsoap_execution_R_v2.3.xlsx",
           row.names=FALSE,sheetName="Intercept",append=TRUE)

write.xlsx(OD.Handsoap_Freq_MIS,"C:/Yashwanth/Pricing/1.OfficeDepot/10.Rules/3.Execution/1.Handsoaps/Handsoap_execution_R_v2.3.xlsx",
           row.names=FALSE,sheetName="Handsoap Process MIS",append=TRUE)

#------------------------------------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------------------------------
#------------------------- END ------------------------ Rules Implementation : Final Delivery---------------------------- END -------------------
#------------------------------------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------------------------------
