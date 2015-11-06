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
library(plyr)

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

