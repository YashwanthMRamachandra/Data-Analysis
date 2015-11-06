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

# -----------------------------------------------------------------------------------------------------------------------------------------------#
# -----------------------------------------------------------------------------------------------------------------------------------------------#
# -----------------------------------------------------------------------------------------------------------------------------------------------#
#                                                   || Rules Implementation : Optimizer ||
# -----------------------------------------------------------------------------------------------------------------------------------------------#
# -----------------------------------------------------------------------------------------------------------------------------------------------#
# -----------------------------------------------------------------------------------------------------------------------------------------------#

# Model Ln Sales = Int -0.03 (OD Final Price - Staples Final Price) + 0.0013 * Sales of Previous Week       #
#       Ln S2 - Ln S1 = 0.03 (ODP 1 - STP 1 - ODP2 + STP2) + 0.0013 * (S1 - S0)                             #
#       Ln S2 = ln S1 + 0.03(ODP 1 + STP2 - STP 1 - ODP2) + 0.0013 * (S1 - S0)                              #
#       S2 = exp(ln S1 + 0.03(ODP1 + STP 2 - STP 1 - ODP2) + 0.0013 * (S1 - S0) )                           #

#       Margin2 = S2 (ODP2 - Cost)
#       Margin 2 = exp[ln S1 + 0.03 (ODP 1 + STP 2 - STP 1 - ODP2) + 0.0013 * (S1 - S0) ]*(ODP2-Cost)
rm(list=ls())
load("~/OD Dynamic Pricing/OD_DynamicPricing/2.Rules/1.Handsoaps/Rules_Implemetation_v2.RData")
rm(list=setdiff(ls(),"OD.Handsoap_Rule_1234"))

setwd("C:/Yashwanth/Pricing/1.OfficeDepot/10.Rules/1.Rules_data_files/3.Data_v2.1/")
OD.Handsoap_Optimizer <- read.table("Optimizer.csv",header = TRUE, sep = ",", quote = "\"", dec = ".",
                                    fill=TRUE, comment.char="", as.is=TRUE)

OD.Handsoap_Optimizer$ODP_1_Last_weeks_OD_Price <- as.numeric(OD.Handsoap_Optimizer$ODP_1_Last_weeks_OD_Price)
OD.Handsoap_Optimizer$STP_2_Latest_Staples_Price <- as.numeric(OD.Handsoap_Optimizer$STP_2_Latest_Staples_Price)
OD.Handsoap_Optimizer$STP_1_Last_weeks_Staples_price <- as.numeric(OD.Handsoap_Optimizer$STP_1_Last_weeks_Staples_price)
OD.Handsoap_Optimizer$ODP_2_Floor <- as.numeric(OD.Handsoap_Optimizer$ODP_2_Floor)
OD.Handsoap_Optimizer$ODP_2_Cap <- as.numeric(OD.Handsoap_Optimizer$ODP_2_Cap)

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
#                                                   || OD Margin values : 1,2,3,4,5,6,7,8,9,10 ||
#                                               =  EXP(LN(Last week's sales)+0.03*($Last week's OD Price+
#                                                Latest Staples Price-Last week's Staples price-ODP 2 Value 1)+
#                                               0.0013*($Last week's sales-$Last to last week's sales))*(ODP 2 Value 1-$Cost)
# ------------------------------------------------------------------------------------------------------------------------------------------#

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
  OD.Handsoap_Optimizer$OD_Margin2_Value1[i]  <- exp(log(S1_Last_weeks_sales[i])+ 
                                                       0.03*(ODP_1_Last_weeks_OD_Price[i]+STP_2_Latest_Staples_Price[i]-STP_1_Last_weeks_Staples_price[i]-OD_ODP2_Value1[i])+
                                                       0.0013*(S1_Last_weeks_sales[i]-S0_Last_to_last_weeks_sales[i]))*(OD_ODP2_Value1[i]-Cost[i]);
  OD.Handsoap_Optimizer$OD_Margin2_Value2[i]  <- exp(log(S1_Last_weeks_sales[i])+ 
                                                       0.03*(ODP_1_Last_weeks_OD_Price[i]+STP_2_Latest_Staples_Price[i]-STP_1_Last_weeks_Staples_price[i]-OD_ODP2_Value2[i])+
                                                       0.0013*(S1_Last_weeks_sales[i]-S0_Last_to_last_weeks_sales[i]))*(OD_ODP2_Value2[i]-Cost[i]);    
  OD.Handsoap_Optimizer$OD_Margin2_Value3[i]  <- exp(log(S1_Last_weeks_sales[i])+ 
                                                       0.03*(ODP_1_Last_weeks_OD_Price[i]+STP_2_Latest_Staples_Price[i]-STP_1_Last_weeks_Staples_price[i]-OD_ODP2_Value3[i])+
                                                       0.0013*(S1_Last_weeks_sales[i]-S0_Last_to_last_weeks_sales[i]))*(OD_ODP2_Value3[i]-Cost[i]);
  OD.Handsoap_Optimizer$OD_Margin2_Value4[i]  <- exp(log(S1_Last_weeks_sales[i])+ 
                                                       0.03*(ODP_1_Last_weeks_OD_Price[i]+STP_2_Latest_Staples_Price[i]-STP_1_Last_weeks_Staples_price[i]-OD_ODP2_Value4[i])+
                                                       0.0013*(S1_Last_weeks_sales[i]-S0_Last_to_last_weeks_sales[i]))*(OD_ODP2_Value4[i]-Cost[i]);
  OD.Handsoap_Optimizer$OD_Margin2_Value5[i]  <- exp(log(S1_Last_weeks_sales[i])+ 
                                                       0.03*(ODP_1_Last_weeks_OD_Price[i]+STP_2_Latest_Staples_Price[i]-STP_1_Last_weeks_Staples_price[i]-OD_ODP2_Value5[i])+
                                                       0.0013*(S1_Last_weeks_sales[i]-S0_Last_to_last_weeks_sales[i]))*(OD_ODP2_Value5[i]-Cost[i]);
  OD.Handsoap_Optimizer$OD_Margin2_Value6[i]  <- exp(log(S1_Last_weeks_sales[i])+ 
                                                       0.03*(ODP_1_Last_weeks_OD_Price[i]+STP_2_Latest_Staples_Price[i]-STP_1_Last_weeks_Staples_price[i]-OD_ODP2_Value6[i])+
                                                       0.0013*(S1_Last_weeks_sales[i]-S0_Last_to_last_weeks_sales[i]))*(OD_ODP2_Value6[i]-Cost[i]);
  OD.Handsoap_Optimizer$OD_Margin2_Value7[i]  <- exp(log(S1_Last_weeks_sales[i])+ 
                                                       0.03*(ODP_1_Last_weeks_OD_Price[i]+STP_2_Latest_Staples_Price[i]-STP_1_Last_weeks_Staples_price[i]-OD_ODP2_Value7[i])+
                                                       0.0013*(S1_Last_weeks_sales[i]-S0_Last_to_last_weeks_sales[i]))*(OD_ODP2_Value7[i]-Cost[i]);
  OD.Handsoap_Optimizer$OD_Margin2_Value8[i]  <- exp(log(S1_Last_weeks_sales[i])+ 
                                                       0.03*(ODP_1_Last_weeks_OD_Price[i]+STP_2_Latest_Staples_Price[i]-STP_1_Last_weeks_Staples_price[i]-OD_ODP2_Value8[i])+
                                                       0.0013*(S1_Last_weeks_sales[i]-S0_Last_to_last_weeks_sales[i]))*(OD_ODP2_Value8[i]-Cost[i]);
  OD.Handsoap_Optimizer$OD_Margin2_Value9[i]  <- exp(log(S1_Last_weeks_sales[i])+ 
                                                       0.03*(ODP_1_Last_weeks_OD_Price[i]+STP_2_Latest_Staples_Price[i]-STP_1_Last_weeks_Staples_price[i]-OD_ODP2_Value9[i])+
                                                       0.0013*(S1_Last_weeks_sales[i]-S0_Last_to_last_weeks_sales[i]))*(OD_ODP2_Value9[i]-Cost[i]);
  OD.Handsoap_Optimizer$OD_Margin2_Value10[i]  <- exp(log(S1_Last_weeks_sales[i])+ 
                                                        0.03*(ODP_1_Last_weeks_OD_Price[i]+STP_2_Latest_Staples_Price[i]-STP_1_Last_weeks_Staples_price[i]-OD_ODP2_Value10[i])+
                                                        0.0013*(S1_Last_weeks_sales[i]-S0_Last_to_last_weeks_sales[i]))*(OD_ODP2_Value10[i]-Cost[i]);
  
}  
detach(OD.Handsoap_Optimizer)  

attach(OD.Handsoap_Optimizer)  
for(i in 1:nrow(OD.Handsoap_Optimizer)){
  OD.Handsoap_Optimizer$OD_Margin2_Value1[i] <- ifelse(OD_Margin2_Value1[i]==0,NA,OD_Margin2_Value1[i]);
  OD.Handsoap_Optimizer$OD_Margin2_Value2[i] <- ifelse(OD_Margin2_Value2[i]==0,NA,OD_Margin2_Value2[i]);
  OD.Handsoap_Optimizer$OD_Margin2_Value3[i] <- ifelse(OD_Margin2_Value3[i]==0,NA,OD_Margin2_Value3[i]);
  OD.Handsoap_Optimizer$OD_Margin2_Value4[i] <- ifelse(OD_Margin2_Value4[i]==0,NA,OD_Margin2_Value4[i]);
  OD.Handsoap_Optimizer$OD_Margin2_Value5[i] <- ifelse(OD_Margin2_Value5[i]==0,NA,OD_Margin2_Value5[i]);
  OD.Handsoap_Optimizer$OD_Margin2_Value6[i] <- ifelse(OD_Margin2_Value6[i]==0,NA,OD_Margin2_Value6[i]);
  OD.Handsoap_Optimizer$OD_Margin2_Value7[i] <- ifelse(OD_Margin2_Value7[i]==0,NA,OD_Margin2_Value7[i]);
  OD.Handsoap_Optimizer$OD_Margin2_Value8[i] <- ifelse(OD_Margin2_Value8[i]==0,NA,OD_Margin2_Value8[i]);
  OD.Handsoap_Optimizer$OD_Margin2_Value9[i] <- ifelse(OD_Margin2_Value9[i]==0,NA,OD_Margin2_Value9[i]);
  OD.Handsoap_Optimizer$OD_Margin2_Value10[i] <- ifelse(OD_Margin2_Value10[i]==0,NA,OD_Margin2_Value10[i]);
}  
detach(OD.Handsoap_Optimizer)

# -----------------------------------------------------------------------------------------------------------------------------------------------#
#                                                       ||  Margins ||
# -----------------------------------------------------------------------------------------------------------------------------------------------#

# Max margin
attach(OD.Handsoap_Optimizer)
for(j in 1:nrow(OD.Handsoap_Optimizer)){
  OD.Handsoap_Optimizer$Max_Margin[j] <- max(OD_Margin2_Value1[j],OD_Margin2_Value2[j],OD_Margin2_Value3[j],OD_Margin2_Value4[j],
                                             OD_Margin2_Value5[j],OD_Margin2_Value6[j],OD_Margin2_Value7[j],OD_Margin2_Value8[j],
                                             OD_Margin2_Value9[j],OD_Margin2_Value10[j])
}
detach(OD.Handsoap_Optimizer)

# ODP2 for Max Margin
OD.Handsoap_Optimizer_MM <- OD.Handsoap_Optimizer[,grep(paste(toMatch <- c("OD_Margin","Max_Margin"),collapse="|"),names(OD.Handsoap_Optimizer),value=T)]
OD.Handsoap_Optimizer_Value <- OD.Handsoap_Optimizer[,grep("OD_ODP2",names(OD.Handsoap_Optimizer),value=T)]
rownames(OD.Handsoap_Optimizer_MM) <- NULL

ODP_2_for_Max_Margin_2 <- NA

for(j in 1:nrow(OD.Handsoap_Optimizer_MM)){
  ODP_2_for_Max_Margin_2[j] <- list(OD.Handsoap_Optimizer_Value[j,
                                                                match(OD.Handsoap_Optimizer_MM$Max_Margin[j],OD.Handsoap_Optimizer_MM[j,grep("OD_Margin",names(OD.Handsoap_Optimizer_MM),value=T)])])
}

ODP_2_for_Max_Margin_2 <- as.numeric(ifelse(ODP_2_for_Max_Margin_2=="NULL",NA,ODP_2_for_Max_Margin_2))

OD.Handsoap_Optimizer <- cbind(OD.Handsoap_Optimizer,ODP_2_for_Max_Margin_2)
rm(OD.Handsoap_Optimizer_MM,OD.Handsoap_Optimizer_Value)

# -----------------------------------------------------------------------------------------------------------------------------------------------#
#                                                       ||  Initial Recommended Price ||
# -----------------------------------------------------------------------------------------------------------------------------------------------#

OD.Handsoap_Optimizer <- merge(OD.Handsoap_Optimizer,OD.Handsoap_Rule_1234[,c("SKU","Latest_OD_Price")],by="SKU",all.x=T)  
OD.Handsoap_Optimizer <- merge(OD.Handsoap_Optimizer,OD.Handsoap_Rule_1234[,c("SKU","Final_Action")],by="SKU",all.x=T)  
OD.Handsoap_Optimizer$Final_Action[is.na(OD.Handsoap_Optimizer$Final_Action)] <- " "

Initial_Rec_Price <- NA
for(i in 1:nrow(OD.Handsoap_Optimizer)){
  Initial_Rec_Price[i] <- ifelse(is.na(OD.Handsoap_Optimizer$ODP_2_for_Max_Margin_2[i]),
                                 OD.Handsoap_Optimizer$Latest_OD_Price[i],OD.Handsoap_Optimizer$ODP_2_for_Max_Margin_2[i])
}
# Initial_Rec_Price <- round(ifelse(Initial_Rec_Price=="-Inf",NA,Initial_Rec_Price),3)

OD.Handsoap_Optimizer <- cbind(OD.Handsoap_Optimizer,Initial_Rec_Price)

# -----------------------------------------------------------------------------------------------------------------------------------------------#
#                                                       ||  Export to Excel ||
# -----------------------------------------------------------------------------------------------------------------------------------------------#

library(xlsx)
write.xlsx(OD.Handsoap_Optimizer,"C:/Yashwanth/Pricing/1.OfficeDepot/10.Rules/3.Execution/1.Handsoaps/Handsoap_execution_R_v2.xlsx",
           row.names=FALSE,sheetName="Optimizer",append=TRUE)


#------------------------------------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------------------------------
#------------------------- END ------------------------ Rules Implementation : Optimizer ---------------------------- END -----------------------
#------------------------------------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------------------------------

#   Demo[1,match(Demo$Max_Margin[1],Demo[1,grep("Margin",names(Demo),value=T)])]
#   Demo <- subset(OD.Handsoap_Optimizer,Max_Margin!="NA",
#                  grep(paste(toMatch <- c("OD_ODP","Max_Margin"),collapse="|"),names(Demo),value=T));rownames(Demo) <- NULL
#   
#   Demo <- OD.Handsoap_Optimizer[c(1,2,7,12,16),c(1,10:30)];rownames(Demo) <- NULL
#   
#   
#   Demo_ODP2 <- NA
#   for(j in 1:nrow(Demo)){
#     Demo_ODP2[j] <- list(Demo[j,
#               match(Demo$Max_Margin[j],Demo[j,grep(paste(toMatch <- c("OD_ODP","Max_Margin"),collapse="|"),names(Demo),value=T)])-1])
#   }
#   Demo_ODP2 <- as.numeric(ifelse(Demo_ODP2=="NULL",NA,Demo_ODP2))
#     
#   

# -----------------------------------------------------------------------------------------------------------------------------------------------#
# -----------------------------------------------------------------------------------------------------------------------------------------------#
# -----------------------------------------------------------------------------------------------------------------------------------------------#
#                                                   || Rules Implementation : Post Processing Rules ||
# -----------------------------------------------------------------------------------------------------------------------------------------------#
# -----------------------------------------------------------------------------------------------------------------------------------------------#
# -----------------------------------------------------------------------------------------------------------------------------------------------#

rm(list=ls())
load("~/OD Dynamic Pricing/OD_DynamicPricing/2.Rules/1.Handsoaps/Post_Proc_Rules_v2.1.RData")
rm(list=setdiff(ls(),c("OD.Handsoap_Rule_1234","OD.Handsoap_Optimizer")))

# Line rule

setwd("C:/Yashwanth/Pricing/1.OfficeDepot/10.Rules/1.Rules_data_files/3.Data_v2.1/")

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
#                                                       ||  Initial Recommended Price ||
# -----------------------------------------------------------------------------------------------------------------------------------------------#

# Family 2
OD.Line_Family2 <- merge(OD.Line_Family2,OD.Handsoap_Optimizer[,c("SKU","Initial_Rec_Price")],by="SKU",all.x=T)
OD.Line_Family2$Number_SKU <- OD.Line_Family2$SKU

Final_Recommended_Price <- NA
for(i in 1:nrow(OD.Line_Family2)){
  Final_Recommended_Price[i] <- ifelse(OD.Line_Family2$Conflict_Resolution[i]=="Primary",
                                       OD.Line_Family2$Initial_Rec_Price[3],OD.Line_Family2$Initial_Rec_Price[3])
}
OD.Line_Family2 <- cbind(OD.Line_Family2,Final_Recommended_Price)


# Family 3
OD.Line_Family3 <- merge(OD.Line_Family3,OD.Handsoap_Optimizer[,c("SKU","Initial_Rec_Price")],by="SKU",all.x=T)
OD.Line_Family3$Number_SKU <- OD.Line_Family3$SKU

Final_Recommended_Price <- NA
for(i in 1:nrow(OD.Line_Family3)){
  Final_Recommended_Price[i] <- ifelse(OD.Line_Family3$Conflict_Resolution[i]=="Primary",
                                       OD.Line_Family3$Initial_Rec_Price[2],OD.Line_Family3$Initial_Rec_Price[2])
}
OD.Line_Family3 <- cbind(OD.Line_Family3,Final_Recommended_Price)

# Family 4
OD.Line_Family4 <- merge(OD.Line_Family4,OD.Handsoap_Optimizer[,c("SKU","Initial_Rec_Price")],by="SKU",all.x=T)
OD.Line_Family4$Number_SKU <- OD.Line_Family4$SKU

Final_Recommended_Price <- NA
for(i in 1:nrow(OD.Line_Family4)){
  Final_Recommended_Price[i] <- ifelse(OD.Line_Family4$Conflict_Resolution[i]=="Primary",
                                       OD.Line_Family4$Initial_Rec_Price[2],OD.Line_Family4$Initial_Rec_Price[2])
}
OD.Line_Family4 <- cbind(OD.Line_Family4,Final_Recommended_Price)


# Family 6
OD.Line_Family6 <- merge(OD.Line_Family6,OD.Handsoap_Optimizer[,c("SKU","Initial_Rec_Price")],by="SKU",all.x=T)
OD.Line_Family6$Number_SKU <- OD.Line_Family6$SKU

Final_Recommended_Price <- NA
for(i in 1:nrow(OD.Line_Family6)){
  Final_Recommended_Price[i] <- ifelse(OD.Line_Family6$Conflict_Resolution[i]=="Primary",
                                       OD.Line_Family6$Initial_Rec_Price[2],OD.Line_Family6$Initial_Rec_Price[2])
}
OD.Line_Family6 <- cbind(OD.Line_Family6,Final_Recommended_Price)


# Family 7
OD.Line_Family7 <- merge(OD.Line_Family7,OD.Handsoap_Optimizer[,c("SKU","Initial_Rec_Price")],by="SKU",all.x=T)
OD.Line_Family7$Number_SKU <- OD.Line_Family7$SKU

Final_Recommended_Price <- NA
for(i in 1:nrow(OD.Line_Family7)){
  Final_Recommended_Price[i] <- ifelse(OD.Line_Family7$Conflict_Resolution[i]=="Primary",
                                       OD.Line_Family7$Initial_Rec_Price[1],OD.Line_Family7$Initial_Rec_Price[1])
}
OD.Line_Family7 <- cbind(OD.Line_Family7,Final_Recommended_Price)

# Family 8
OD.Line_Family8 <- merge(OD.Line_Family8,OD.Handsoap_Optimizer[,c("SKU","Initial_Rec_Price")],by="SKU",all.x=T)
OD.Line_Family8$Number_SKU <- OD.Line_Family8$SKU

Final_Recommended_Price <- NA
for(i in 1:nrow(OD.Line_Family8)){
  Final_Recommended_Price[i] <- ifelse(OD.Line_Family8$Conflict_Resolution[i]=="Primary",
                                       OD.Line_Family8$Initial_Rec_Price[3],OD.Line_Family8$Initial_Rec_Price[3])
}
OD.Line_Family8 <- cbind(OD.Line_Family8,Final_Recommended_Price)

# Family 9  : Ignore Error
OD.Line_Family9 <- merge(OD.Line_Family9,OD.Handsoap_Optimizer[,c("SKU","Initial_Rec_Price")],by="SKU",all.x=T)
OD.Line_Family9$Number_SKU <- OD.Line_Family9$SKU

Final_Recommended_Price <- NA
for(i in 1:nrow(OD.Line_Family9)){
  Final_Recommended_Price[i] <- ifelse(OD.Line_Family9$Conflict_Resolution[i]=="Primary",
                                       OD.Line_Family9$Initial_Rec_Price[2],OD.Line_Family9$Initial_Rec_Price[2])
}
OD.Line_Family9 <- cbind(OD.Line_Family9,Final_Recommended_Price)

# Brand Family1 & Family 2  
OD.Line_Brand_Family_12 <- merge(OD.Line_Brand_Family_12,OD.Handsoap_Optimizer[,c("SKU","Initial_Rec_Price")],by="SKU",all.x=T)
OD.Line_Brand_Family_12$Number_SKU <- OD.Line_Brand_Family_12$SKU

attach(OD.Line_Brand_Family_12)
OD.Line_Brand_Family_12$Final_Recommended_Price <- ifelse(Brand_Class=="PRIVATE",
                                                          ifelse(Initial_Rec_Price<Initial_Rec_Price[2] & Initial_Rec_Price<Initial_Rec_Price[3],Initial_Rec_Price,
                                                                 0.99*min(Initial_Rec_Price[2],Initial_Rec_Price[3])),Initial_Rec_Price)
detach(OD.Line_Brand_Family_12)

library(plyr)
OD.Line_Brand_Family_12 <- rename(OD.Line_Brand_Family_12,c("Brand_Class"="Conflict_Resolution"))

# -----------------------------------------------------------------------------------------------------------------------------------------------#
#                                                       ||  Export to Excel ||
# -----------------------------------------------------------------------------------------------------------------------------------------------#

library(xlsx)

OD.DP_Post_Proc <- rbind(OD.Line_Family2,OD.Line_Family3,OD.Line_Family4,OD.Line_Family6,OD.Line_Family7,OD.Line_Family8,
                         OD.Line_Family9,OD.Line_Brand_Family_12)


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

save.xlsx("C:/Yashwanth/Pricing/1.OfficeDepot/10.Rules/3.Execution/1.Handsoaps/Handsoaps_Post_Proc.xlsx",
          OD.Line_Family2,OD.Line_Family3,OD.Line_Family4,OD.Line_Family6,OD.Line_Family7,OD.Line_Family8,OD.Line_Family9,
          OD.Line_Brand_Family_12,OD.DP_Post_Proc)

write.xlsx(OD.DP_Post_Proc,"C:/Yashwanth/Pricing/1.OfficeDepot/10.Rules/3.Execution/1.Handsoaps/Handsoap_execution_R_v2.xlsx",
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
load("~/OD Dynamic Pricing/OD_DynamicPricing/2.Rules/1.Handsoaps/Final_Delivery_Freq_MIS_v2.1.RData")
rm(list=setdiff(ls(),c("OD.Handsoap_Rule_1234","OD.Handsoap_Optimizer","OD.DP_Post_Proc")))

setwd("C:/Yashwanth/Pricing/1.OfficeDepot/10.Rules/1.Rules_data_files/3.Data_v2.1/")
OD.Handsoap_Final_Del <- read.table("Final_Delivery.csv",header = TRUE, sep = ",", quote = "\"", dec = ".",
                                    fill=TRUE, comment.char="", as.is=TRUE)

# Current OD Price & Final Action
library(plyr)
OD.Handsoap_Final_Del <- merge(OD.Handsoap_Final_Del,OD.Handsoap_Rule_1234[,c("SKU","Latest_OD_Price")],by="SKU",all.x=T)
OD.Handsoap_Final_Del <- merge(OD.Handsoap_Final_Del,OD.Handsoap_Optimizer[,c("SKU","Final_Action")],by="SKU",all.x=T)
OD.Handsoap_Final_Del <- rename(OD.Handsoap_Final_Del,c("Latest_OD_Price"="Current_OD_Price"))
OD.Handsoap_Final_Del$Final_Action <- ifelse(OD.Handsoap_Final_Del$SKU=="521845",OD.Handsoap_Final_Del$Final_Action==NA,
                                             OD.Handsoap_Final_Del$Final_Action)

# Post Processing Price
OD.Handsoap_Final_Del <- merge(OD.Handsoap_Final_Del,OD.DP_Post_Proc[,c("SKU","Final_Recommended_Price")],by="SKU",all.x=T)
OD.Handsoap_Final_Del <- rename(OD.Handsoap_Final_Del,c("Final_Recommended_Price"="Post_Processing_Price"))
OD.Handsoap_Final_Del$Post_Processing_Price[is.na(OD.Handsoap_Final_Del$Post_Processing_Price)] <- " "
OD.Handsoap_Final_Del$Post_Processing_Price <- ifelse(OD.Handsoap_Final_Del$SKU=="155130" | OD.Handsoap_Final_Del$SKU=="474353",
                                                      OD.Handsoap_Final_Del$Post_Processing_Price==NA,OD.Handsoap_Final_Del$Post_Processing_Price)

# Final Recommendation Price
OD.Handsoap_Final_Del <-  merge(OD.Handsoap_Final_Del,OD.Handsoap_Optimizer[,c("SKU","Initial_Rec_Price")],by="SKU",all.x=T)
OD.Handsoap_Final_Del$Final_Recommendation <- round(as.numeric(ifelse(OD.Handsoap_Final_Del$Post_Processing_Price!=" ",
                                                                      OD.Handsoap_Final_Del$Post_Processing_Price,OD.Handsoap_Final_Del$Initial_Rec_Price)),1)-0.01

OD.Handsoap_Final_Del <- OD.Handsoap_Final_Del[,-which(names(OD.Handsoap_Final_Del) %in% c("Initial_Rec_Price"))]

# -----------------------------------------------------------------------------------------------------------------------------------------------#
#                                                             ||  Output  ||
#                           =IF(ISERROR(B2),"Current OD Price Not Avaiable",IF(OR(Final Action="R",Final Action="I"),
#   IF(Final Recommendation=Current OD Price,"Rule Triggered, No Price Change","Rule Triggered, Price Change"),"Rule Not Triggered"))
# -----------------------------------------------------------------------------------------------------------------------------------------------#

OD.Handsoap_Final_Del$New_Latest_OD_Price <- OD.Handsoap_Final_Del$Current_OD_Price
OD.Handsoap_Final_Del$New_Latest_OD_Price[is.na(OD.Handsoap_Final_Del$New_Latest_OD_Price)] <- 1000

Output <- NA
attach(OD.Handsoap_Final_Del)
for(i in 1:nrow(OD.Handsoap_Final_Del)){
  Output[i] <-ifelse(New_Latest_OD_Price[i]==1000,"Current OD Price Not Avaiable",
                     ifelse(Final_Action[i]=="R" | Final_Action[i]=="I",
                            ifelse(Final_Recommendation[i]==New_Latest_OD_Price[i],"Rule Triggered, No Price Change","Rule Triggered, Price Change"),
                            "Rule Not Triggered"))
}
detach(OD.Handsoap_Final_Del)

OD.Handsoap_Final_Del <- cbind(OD.Handsoap_Final_Del,Output)
OD.Handsoap_Final_Del <- OD.Handsoap_Final_Del[,-which(names(OD.Handsoap_Final_Del) %in% c("New_Latest_OD_Price"))]

# -----------------------------------------------------------------------------------------------------------------------------------------------#
#                                                       ||  Frequency MIS ||
# -----------------------------------------------------------------------------------------------------------------------------------------------#

OD.Handsoap_Freq_MIS <- aggregate(OD.Handsoap_Final_Del$Output,
                                  list(OD.Handsoap_Final_Del$Output,OD.Handsoap_Final_Del$Final_Action),length)
names(OD.Handsoap_Freq_MIS) <- c("Output","Final_Action","Count")

# -----------------------------------------------------------------------------------------------------------------------------------------------#
#                                                       ||  Export to Excel ||
# -----------------------------------------------------------------------------------------------------------------------------------------------#

library(xlsx)
write.xlsx(OD.Handsoap_Final_Del,"C:/Yashwanth/Pricing/1.OfficeDepot/10.Rules/3.Execution/1.Handsoaps/Handsoap_execution_R_v2.xlsx",
           row.names=FALSE,sheetName="Final Delivery",append=TRUE)

write.xlsx(OD.Handsoap_Freq_MIS,"C:/Yashwanth/Pricing/1.OfficeDepot/10.Rules/3.Execution/1.Handsoaps/Handsoap_execution_R_v2.xlsx",
           row.names=FALSE,sheetName="Freqeuncy MIS",append=TRUE)

#------------------------------------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------------------------------
#------------------------- END ------------------------ Rules Implementation : Final Delivery---------------------------- END -------------------
#------------------------------------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------------------------------

