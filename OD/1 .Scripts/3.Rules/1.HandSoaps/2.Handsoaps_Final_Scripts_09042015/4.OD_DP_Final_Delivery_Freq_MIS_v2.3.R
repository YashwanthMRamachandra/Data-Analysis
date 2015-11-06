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
    
    attach(OD.Handsoap_Final_Del)
    OD.Handsoap_Final_Del$Volume_Change <- ifelse(is.na(exp(log(S1_Last_weeks_sales)+
          0.03*(ODP_1_Last_weeks_OD_Price+STP_2_Latest_Staples_Price-STP_1_Last_weeks_Staples_price-Price_given_to_Office_Depot)+
          0.0013*(S1_Last_weeks_sales-S0_Last_to_last_weeks_sales))/ifelse(is.na(S1_Last_weeks_sales),0,S1_Last_weeks_sales)),S1_Last_weeks_sales,
          exp(log(S1_Last_weeks_sales)+0.03*(ODP_1_Last_weeks_OD_Price+STP_2_Latest_Staples_Price-STP_1_Last_weeks_Staples_price-Price_given_to_Office_Depot)+
          0.0013*(S1_Last_weeks_sales-S0_Last_to_last_weeks_sales))/ifelse(is.na(S1_Last_weeks_sales),0,S1_Last_weeks_sales))
    detach(OD.Handsoap_Final_Del)
    OD.Handsoap_Final_Del$Volume_Change <- ifelse(OD.Handsoap_Final_Del$Volume_Change==0.0000000,1.0000000,
                                                  OD.Handsoap_Final_Del$Volume_Change)
    
    
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
#     OD.Handsoap_Final_Del$Cost <- round(OD.Handsoap_Final_Del$Cost,2)
    
    # Weekwise Volume
    OD.Handsoap_Final_Del$Last_Week_Volume <- ifelse(is.na(OD.Handsoap_Final_Del$S1_Last_weeks_sales),0,
                                                     OD.Handsoap_Final_Del$S1_Last_weeks_sales)
    attach(OD.Handsoap_Final_Del)
    OD.Handsoap_Final_Del$This_Week_Volume <- 
                  ifelse(is.na(exp(log(S1_Last_weeks_sales)+
                  0.03*(ODP_1_Last_weeks_OD_Price+STP_2_Latest_Staples_Price-STP_1_Last_weeks_Staples_price-Price_given_to_Office_Depot)+
                  0.0013*(S1_Last_weeks_sales-S0_Last_to_last_weeks_sales))),S1_Last_weeks_sales,
                  exp(log(S1_Last_weeks_sales)+
                  0.03*(ODP_1_Last_weeks_OD_Price+STP_2_Latest_Staples_Price-STP_1_Last_weeks_Staples_price-Price_given_to_Office_Depot)+
                  0.0013*(S1_Last_weeks_sales-S0_Last_to_last_weeks_sales)))
    detach(OD.Handsoap_Final_Del)
    
    # Weekwise Revenue
    OD.Handsoap_Final_Del$Last_Week_Revenue <- OD.Handsoap_Final_Del$Last_Week_Volume*OD.Handsoap_Final_Del$ODP_1_Last_weeks_OD_Price
    OD.Handsoap_Final_Del$Last_Week_Revenue[is.na(OD.Handsoap_Final_Del$Last_Week_Revenue)] <- 0
    OD.Handsoap_Final_Del$This_Week_Revenue <- OD.Handsoap_Final_Del$This_Week_Volume*OD.Handsoap_Final_Del$Price_given_to_Office_Depot
    OD.Handsoap_Final_Del$This_Week_Revenue[is.na(OD.Handsoap_Final_Del$This_Week_Revenue)] <- 0
    
    # Weekwise Margin
    attach(OD.Handsoap_Final_Del)
    OD.Handsoap_Final_Del$Last_Week_Margin <- ifelse(is.na(Last_Week_Volume*(ODP_1_Last_weeks_OD_Price-Cost)),0,
                                                     Last_Week_Volume*(ODP_1_Last_weeks_OD_Price-Cost))
    OD.Handsoap_Final_Del$This_Week_Margin <- ifelse(is.na(This_Week_Volume*(Price_given_to_Office_Depot-Cost)),0,
                                                     This_Week_Volume*(Price_given_to_Office_Depot-Cost))
    detach(OD.Handsoap_Final_Del)
    
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
