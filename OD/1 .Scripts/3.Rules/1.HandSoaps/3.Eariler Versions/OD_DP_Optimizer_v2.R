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
  