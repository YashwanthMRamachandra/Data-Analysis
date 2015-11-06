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
