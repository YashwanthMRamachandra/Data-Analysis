# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
#                                    Staples Price Elasticity : Aggregation
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------

rm(list=ls())
Staples_IP <- read.table("C:/Yashwanth/Staples PE/1. Input/Staples_Input_20150827.csv",
                         header = TRUE, sep = ",", quote = "\"", dec = "." ,fill=TRUE, comment.char="", as.is=TRUE)
str(Staples_IP)
Staples_IP$Extraction_Date <- as.Date(Staples_IP$Extraction_Date)
Staples_IP$Product_ID <- as.factor(Staples_IP$Product_ID)

#---------------------------------------------------------------------------------------------------------------
#                                       Aggregation of Data
#---------------------------------------------------------------------------------------------------------------

# Weekly Aggregation
library(lubridate)
library(data.table)
Staples_IP <- data.table(Staples_IP)
Staples_IP$Weekly <- floor_date(Staples_IP$Extraction_Date,"week")
Staples_Aggr_Weekly1 <- Staples_IP[,lapply(.SD,mean),by=c("Weekly","Product_ID"),.SDcols=c("Staples_Final_Price","Amz_Final_Price")]
Staples_Aggr_Weekly2 <- Staples_IP[,lapply(.SD,sum),by=c("Weekly","Product_ID"),.SDcols=-c("Staples_Final_Price","Amz_Final_Price")]
Staples_Aggr_Weekly <- cbind(data.frame(Staples_Aggr_Weekly1),data.frame(Staples_Aggr_Weekly2)[,-c(1,2,3)])
Staples_Aggr_Weekly <- Staples_Aggr_Weekly[with(Staples_Aggr_Weekly,order(Staples_Aggr_Weekly$Product_ID)),]
rm(Staples_Aggr_Weekly1,Staples_Aggr_Weekly2)

# Monthly Aggregation
Staples_IP$Monthly <- floor_date(Staples_IP$Extraction_Date,"month")
Staples_Aggr_Monthly1 <- Staples_IP[,lapply(.SD,mean),by=c("Monthly","Product_ID"),.SDcols=c("Staples_Final_Price","Amz_Final_Price")]
Staples_Aggr_Monthly2 <- Staples_IP[,lapply(.SD,sum),by=c("Monthly","Product_ID"),
                                    .SDcols=-c("Staples_Final_Price","Amz_Final_Price","Extraction_Date")]
Staples_Aggr_Monthly <- cbind(data.frame(Staples_Aggr_Monthly1),data.frame(Staples_Aggr_Monthly2)[,-c(1,2)])
Staples_Aggr_Monthly <- Staples_Aggr_Monthly[with(Staples_Aggr_Monthly,order(Staples_Aggr_Monthly$Product_ID)),]
rm(Staples_Aggr_Monthly1,Staples_Aggr_Monthly2)

#---------------------------------------------------------------------------------------------------------------
#                                       Define Attributes : Weekly
#---------------------------------------------------------------------------------------------------------------

# OCPVisit & OCPVisitor
Staples_Aggr_Weekly$Google_OCPVisit <- Staples_Aggr_Weekly$Google_Order/Staples_Aggr_Weekly$Google_Visits
Staples_Aggr_Weekly$Google_OCPVisit[is.nan(Staples_Aggr_Weekly$Google_OCPVisit)] <- 0
Staples_Aggr_Weekly$SC_OCPVisit <- Staples_Aggr_Weekly$SC_Order/Staples_Aggr_Weekly$SC_Visits
Staples_Aggr_Weekly$SC_OCPVisit[is.nan(Staples_Aggr_Weekly$SC_OCPVisit)] <- 0
Staples_Aggr_Weekly$Direct_OCPVisit <- Staples_Aggr_Weekly$Direct_Order/Staples_Aggr_Weekly$Direct_Visits
Staples_Aggr_Weekly$Direct_OCPVisit[is.nan(Staples_Aggr_Weekly$Direct_OCPVisit)] <- 0

Staples_Aggr_Weekly$Google_OCPVisitor <- Staples_Aggr_Weekly$Google_Order/Staples_Aggr_Weekly$Google_Unique_Visitors
Staples_Aggr_Weekly$Google_OCPVisitor[is.nan(Staples_Aggr_Weekly$Google_OCPVisitor)] <- 0
Staples_Aggr_Weekly$SC_OCPVisitor <- Staples_Aggr_Weekly$SC_Order/Staples_Aggr_Weekly$SC_Unique_Visitors
Staples_Aggr_Weekly$SC_OCPVisitor[is.nan(Staples_Aggr_Weekly$SC_OCPVisitor)] <- 0
Staples_Aggr_Weekly$Direct_OCPVisitor <- Staples_Aggr_Weekly$Direct_Order/Staples_Aggr_Weekly$Direct_Unique_Visitors
Staples_Aggr_Weekly$Direct_OCPVisitor[is.nan(Staples_Aggr_Weekly$Direct_OCPVisitor)] <- 0

# Overall 
attach(Staples_Aggr_Weekly)
Staples_Aggr_Weekly$OA_Visits <- Google_Visits+SC_Visits+Direct_Visits
Staples_Aggr_Weekly$OA_Revenue <- Google_Revenue+SC_Revenue+Direct_Revenue 
Staples_Aggr_Weekly$OA_Orders <- Google_Order+SC_Order+Direct_Order
Staples_Aggr_Weekly$OA_Uniq_Visitor <- Google_Unique_Visitors+SC_Unique_Visitors+Direct_Unique_Visitors
detach(Staples_Aggr_Weekly)

Staples_Aggr_Weekly$OA_OCPVisit <- Staples_Aggr_Weekly$OA_Orders/Staples_Aggr_Weekly$OA_Visits
Staples_Aggr_Weekly$OA_OCPVisit[is.nan(Staples_Aggr_Weekly$OA_OCPVisit)] <- 0
Staples_Aggr_Weekly$OA_OCPVisitor <- Staples_Aggr_Weekly$OA_Orders/Staples_Aggr_Weekly$OA_Uniq_Visitor
Staples_Aggr_Weekly$OA_OCPVisitor[is.na(Staples_Aggr_Weekly$OA_OCPVisitor)] <- 0

# Result
attach(Staples_Aggr_Weekly)
Staples_Aggr_Weekly$Goog_Per_Visit <- round((Google_Unique_Visitors+SC_Unique_Visitors)/OA_Uniq_Visitor,2)
Staples_Aggr_Weekly$Goog_Per_Visit[is.nan(Staples_Aggr_Weekly$Goog_Per_Visit)] <- 0
Staples_Aggr_Weekly$Goog_Per_Visit[is.infinite(Staples_Aggr_Weekly$Goog_Per_Visit)] <- 0
Staples_Aggr_Weekly$Dir_Per_Visit <- Direct_Unique_Visitors/OA_Uniq_Visitor
Staples_Aggr_Weekly$Dir_Per_Visit[is.nan(Staples_Aggr_Weekly$Dir_Per_Visit)] <- 0
detach(Staples_Aggr_Weekly)

Staples_Aggr_Weekly$Per_Visit_Diff <- Staples_Aggr_Weekly$Dir_Per_Visit-Staples_Aggr_Weekly$Goog_Per_Visit

#---------------------------------------------------------------------------------------------------------------
#                                       Define Loyalties : Weekly
#---------------------------------------------------------------------------------------------------------------

Staples_Aggr_Weekly$Loyalty <- Staples_Aggr_Weekly$Dir_Per_Visit
Staples_Aggr_Weekly$Conversion <- Staples_Aggr_Weekly$OA_OCPVisitor
Staples_Aggr_Weekly$Price_Comp <-  round(Staples_Aggr_Weekly$Staples_Final_Price/Staples_Aggr_Weekly$Amz_Final_Price,2)

Staples_Aggr_Weekly$Loyalty_Cat <-  ifelse(Staples_Aggr_Weekly$Loyalty > 0.75,"HIGH",ifelse(Staples_Aggr_Weekly$Loyalty < 0.60,"MID","LOW"))
Staples_Aggr_Weekly$Conversion_Cat <- ifelse(Staples_Aggr_Weekly$Conversion > 0.40,"HIGH",ifelse(Staples_Aggr_Weekly$Conversion < 0.25,"MID","LOW"))
Staples_Aggr_Weekly$Price_Comp_Cat <- ifelse(Staples_Aggr_Weekly$Price_Comp > 1,"HIGH",ifelse(Staples_Aggr_Weekly$Price_Comp < 0.9,"MID","LOW"))

Staples_Aggr_Weekly <- data.table(Staples_Aggr_Weekly)
cols <- grep("Visits|Order|Uniq|Cat|ID|ly",names(Staples_Aggr_Weekly),value = T,invert = T)
Staples_Aggr_Weekly <- Staples_Aggr_Weekly[,(cols) := round(.SD,2), .SDcols=cols]
cols <- grep("Visits|Order|Uniq",names(Staples_Aggr_Weekly),value = T)
Staples_Aggr_Weekly <- Staples_Aggr_Weekly[,(cols) := ceiling(.SD), .SDcols=cols];rm(cols)

Staples_Aggr_Weekly <- data.frame(Staples_Aggr_Weekly)
Staples_Aggr_Weekly <- Staples_Aggr_Weekly[,c(grep("Week|Prod",names(Staples_Aggr_Weekly)),grep("Final_Price",names(Staples_Aggr_Weekly)),
                                              grep("Google",names(Staples_Aggr_Weekly)),grep("SC",names(Staples_Aggr_Weekly)),
                                              grep("Direct",names(Staples_Aggr_Weekly)),grep("OA",names(Staples_Aggr_Weekly)),
                                              grep("Week|Prod|Final|Google|SC|Direct|OA",names(Staples_Aggr_Weekly),invert = TRUE))]

#---------------------------------------------------------------------------------------------------------------
#                                           Correlation Test
#---------------------------------------------------------------------------------------------------------------

# Correlation Matrix : Price Competitiveness : Staples/Amazon
Staples_Aggr_Weekly <- data.table(Staples_Aggr_Weekly)
cor(Staples_Aggr_Weekly[Loyalty_Cat=="HIGH",Conversion,Price_Comp])
table(Staples_Aggr_Weekly$Loyalty_Cat)[1]
cor(Staples_Aggr_Weekly[Loyalty_Cat=="MID",Conversion,Price_Comp])
table(Staples_Aggr_Weekly$Loyalty_Cat)[3]
cor(Staples_Aggr_Weekly[Loyalty_Cat=="LOW",Conversion,Price_Comp])
table(Staples_Aggr_Weekly$Loyalty_Cat)[2]

#---------------------------------------------------------------------------------------------------------------
#                                           Chi-Square Test
#---------------------------------------------------------------------------------------------------------------

# NH is something that "assumed/believed" to be FALSE.
# NH : Conversion Category is NOT Dependent(Independent) of Loyalty Cateogry.
attach(Staples_Aggr_Weekly)
chisq.test(Loyalty_Cat,Conversion_Cat)
detach(Staples_Aggr_Weekly)

# Conclusion : Since p<0.05, we reject the NH(Having enough evidence) & conclude that, Loyalty Category is Independent of 
#              Conversion Category.


# NH : Conversion Category is NOT Dependent(Independent) of Price Competitiveness Category.
attach(Staples_Aggr_Weekly)
chisq.test(Price_Comp_Cat,Conversion_Cat)
detach(Staples_Aggr_Weekly)

# Conclusion : Since p<0.05(but Test-Statistic is lesser then above Hypothesis), we reject the NH(Having enough evidence) & conclude
#             that, Price Competitiveness Category is Independent of Conversion Category.


#---------------------------------------------------------------------------------------------------------------
#                                       Define Attributes : Monthly
#---------------------------------------------------------------------------------------------------------------

# OCPVisit & OCPVisitor
Staples_Aggr_Monthly$Google_OCPVisit <- Staples_Aggr_Monthly$Google_Order/Staples_Aggr_Monthly$Google_Visits
Staples_Aggr_Monthly$Google_OCPVisit[is.nan(Staples_Aggr_Monthly$Google_OCPVisit)] <- 0
Staples_Aggr_Monthly$SC_OCPVisit <- Staples_Aggr_Monthly$SC_Order/Staples_Aggr_Monthly$SC_Visits
Staples_Aggr_Monthly$SC_OCPVisit[is.nan(Staples_Aggr_Monthly$SC_OCPVisit)] <- 0
Staples_Aggr_Monthly$Direct_OCPVisit <- Staples_Aggr_Monthly$Direct_Order/Staples_Aggr_Monthly$Direct_Visits
Staples_Aggr_Monthly$Direct_OCPVisit[is.nan(Staples_Aggr_Monthly$Direct_OCPVisit)] <- 0

Staples_Aggr_Monthly$Google_OCPVisitor <- Staples_Aggr_Monthly$Google_Order/Staples_Aggr_Monthly$Google_Unique_Visitors
Staples_Aggr_Monthly$Google_OCPVisitor[is.nan(Staples_Aggr_Monthly$Google_OCPVisitor)] <- 0
Staples_Aggr_Monthly$SC_OCPVisitor <- Staples_Aggr_Monthly$SC_Order/Staples_Aggr_Monthly$SC_Unique_Visitors
Staples_Aggr_Monthly$SC_OCPVisitor[is.nan(Staples_Aggr_Monthly$SC_OCPVisitor)] <- 0
Staples_Aggr_Monthly$Direct_OCPVisitor <- Staples_Aggr_Monthly$Direct_Order/Staples_Aggr_Monthly$Direct_Unique_Visitors
Staples_Aggr_Monthly$Direct_OCPVisitor[is.nan(Staples_Aggr_Monthly$Direct_OCPVisitor)] <- 0

# Overall 
attach(Staples_Aggr_Monthly)
Staples_Aggr_Monthly$OA_Visits <- Google_Visits+SC_Visits+Direct_Visits
Staples_Aggr_Monthly$OA_Revenue <- Google_Revenue+SC_Revenue+Direct_Revenue 
Staples_Aggr_Monthly$OA_Orders <- Google_Order+SC_Order+Direct_Order
Staples_Aggr_Monthly$OA_Uniq_Visitor <- Google_Unique_Visitors+SC_Unique_Visitors+Direct_Unique_Visitors
Staples_Aggr_Monthly$OA_Aggr_Search_Vol <- Google_Aggr_Search_Vol+SC_Aggr_Search_Vol+Direct_Aggr_Search_Vol
Staples_Aggr_Monthly$OA_Bounce_rate <- Google_Bounce_rate+SC_Bounce_rate+Direct_Bounce_rate
Staples_Aggr_Monthly$OA_Cart_Views <- Google_Cart_Views+SC_Cart_Views+Direct_Cart_Views
Staples_Aggr_Monthly$OA_Shipping <- Google_Shipping+SC_Shipping+Direct_Shipping
Staples_Aggr_Monthly$OA_Visit_Freq <- Google_Visit_Freq+SC_Visit_Freq+Direct_Visit_Freq
detach(Staples_Aggr_Monthly)

Staples_Aggr_Monthly$OA_OCPVisit <- Staples_Aggr_Monthly$OA_Orders/Staples_Aggr_Monthly$OA_Visits
Staples_Aggr_Monthly$OA_OCPVisit[is.nan(Staples_Aggr_Monthly$OA_OCPVisit)] <- 0
Staples_Aggr_Monthly$OA_OCPVisitor <- Staples_Aggr_Monthly$OA_Orders/Staples_Aggr_Monthly$OA_Uniq_Visitor
Staples_Aggr_Monthly$OA_OCPVisitor[is.na(Staples_Aggr_Monthly$OA_OCPVisitor)] <- 0

# Result
attach(Staples_Aggr_Monthly)
Staples_Aggr_Monthly$Goog_Per_Visit <- round((Google_Unique_Visitors+SC_Unique_Visitors)/OA_Uniq_Visitor,2)
Staples_Aggr_Monthly$Goog_Per_Visit[is.nan(Staples_Aggr_Monthly$Goog_Per_Visit)] <- 0
Staples_Aggr_Monthly$Goog_Per_Visit[is.infinite(Staples_Aggr_Monthly$Goog_Per_Visit)] <- 0
Staples_Aggr_Monthly$Dir_Per_Visit <- Direct_Unique_Visitors/OA_Uniq_Visitor
Staples_Aggr_Monthly$Dir_Per_Visit[is.nan(Staples_Aggr_Monthly$Dir_Per_Visit)] <- 0
detach(Staples_Aggr_Monthly)

Staples_Aggr_Monthly$Per_Visit_Diff <- Staples_Aggr_Monthly$Dir_Per_Visit-Staples_Aggr_Monthly$Goog_Per_Visit

#---------------------------------------------------------------------------------------------------------------
#                                       Define Loyalties : Monthly
#---------------------------------------------------------------------------------------------------------------

Staples_Aggr_Monthly$Loyalty <- Staples_Aggr_Monthly$Dir_Per_Visit
Staples_Aggr_Monthly$Conversion <- Staples_Aggr_Monthly$OA_OCPVisit
Staples_Aggr_Monthly$Conversion_Visitor <- Staples_Aggr_Monthly$OA_OCPVisitor
Staples_Aggr_Monthly$Price_Comp <-  round(Staples_Aggr_Monthly$Staples_Final_Price/Staples_Aggr_Monthly$Amz_Final_Price,2)

Staples_Aggr_Monthly$Loyalty_Cat <-  ifelse(Staples_Aggr_Monthly$Loyalty > 0.75,"HIGH",ifelse(Staples_Aggr_Monthly$Loyalty < 0.60,"MID","LOW"))
Staples_Aggr_Monthly$Conversion_Cat <- ifelse(Staples_Aggr_Monthly$Conversion > 0.40,"HIGH",ifelse(Staples_Aggr_Monthly$Conversion < 0.25,"MID","LOW"))
Staples_Aggr_Monthly$Price_Comp_Cat <- ifelse(Staples_Aggr_Monthly$Price_Comp > 1,"HIGH",ifelse(Staples_Aggr_Monthly$Price_Comp < 0.9,"MID","LOW"))

Staples_Aggr_Monthly <- data.table(Staples_Aggr_Monthly)
cols <- grep("Visits|Order|Uniq|Cat|ID|ly|cart",names(Staples_Aggr_Monthly),value = T,invert = T)
Staples_Aggr_Monthly <- Staples_Aggr_Monthly[,(cols) := round(.SD,2), .SDcols=cols]
cols <- grep("Visits|Order|Uniq|cart",names(Staples_Aggr_Monthly),value = T)
Staples_Aggr_Monthly <- Staples_Aggr_Monthly[,(cols) := ceiling(.SD), .SDcols=cols];rm(cols)

Staples_Aggr_Monthly <- data.frame(Staples_Aggr_Monthly)
Staples_Aggr_Monthly <- Staples_Aggr_Monthly[,c(grep("Month|Prod",names(Staples_Aggr_Monthly)),grep("Final_Price",names(Staples_Aggr_Monthly)),
                                                grep("Google",names(Staples_Aggr_Monthly)),grep("SC",names(Staples_Aggr_Monthly)),
                                                grep("Direct",names(Staples_Aggr_Monthly)),grep("OA",names(Staples_Aggr_Monthly)),
                                                grep("Month|Prod|Final|Google|SC|Direct|OA",names(Staples_Aggr_Monthly),invert = TRUE))]

#---------------------------------------------------------------------------------------------------------------
#                                           Aggregate By Product
#---------------------------------------------------------------------------------------------------------------

Staples_Aggr_Monthly <- data.table(Staples_Aggr_Monthly)
Staples_Aggr_Monthly_by_Prod1 <- Staples_Aggr_Monthly[,lapply(.SD,mean),by="Product_ID",.SDcols=c("Staples_Final_Price","Amz_Final_Price")]
Staples_Aggr_Monthly_by_Prod2 <- Staples_Aggr_Monthly[,lapply(.SD,sum),by="Product_ID",
                            .SDcols=-c("Staples_Final_Price","Amz_Final_Price","Loyalty_Cat","Conversion_Cat","Price_Comp_Cat",
                            "Goog_Per_Visit","Dir_Per_Visit","Per_Visit_Diff","Loyalty","Conversion","Conversion_Visitor","Price_Comp")]
Staples_Aggr_Monthly_by_Prod <- cbind(data.frame(Staples_Aggr_Monthly_by_Prod1),data.frame(Staples_Aggr_Monthly_by_Prod2)[,-c(1,2)])
rm(Staples_Aggr_Monthly_by_Prod1,Staples_Aggr_Monthly_by_Prod2)

Staples_Aggr_Monthly_by_Prod$Conversion <- Staples_Aggr_Monthly_by_Prod$OA_Orders/Staples_Aggr_Monthly_by_Prod$OA_Visits
Staples_Aggr_Monthly_by_Prod$Conversion_Visitor <- Staples_Aggr_Monthly_by_Prod$OA_Orders/Staples_Aggr_Monthly_by_Prod$OA_Uniq_Visitor
Staples_Aggr_Monthly_by_Prod$Price_Comp <- Staples_Aggr_Monthly_by_Prod$Staples_Final_Price/Staples_Aggr_Monthly_by_Prod$Amz_Final_Price

# Round off
Staples_Aggr_Monthly_by_Prod <- data.table(Staples_Aggr_Monthly_by_Prod)
cols <- grep("Revenue|Conversion|_Comp|Final",names(Staples_Aggr_Monthly_by_Prod),value = T)
Staples_Aggr_Monthly_by_Prod <- Staples_Aggr_Monthly_by_Prod[,(cols) := round(.SD,2), .SDcols=cols]
cols <- grep("Visits|Order|Uniq",names(Staples_Aggr_Monthly_by_Prod),value = T)
Staples_Aggr_Monthly_by_Prod <- Staples_Aggr_Monthly_by_Prod[,(cols) := ceiling(.SD), .SDcols=cols];rm(cols)

# Merge attributes with Correlation
# Staples_Corr_By_Prod_Cor_L.M30 <- merge(Staples_Corr_By_Prod_Cor_L.M30,Staples_Aggr_Monthly_by_Prod,by="Product_ID",all.x = TRUE)
# Staples_Corr_By_Prod_Cor_G.P30 <- merge(Staples_Corr_By_Prod_Cor_G.P30,Staples_Aggr_Monthly_by_Prod,by="Product_ID",all.x = TRUE)
# Staples_Corr_By_Prod_Cor_L.M30$Decision_Criterion <- NULL
# Staples_Corr_By_Prod_Cor_G.P30$Decision_Criterion <- NULL

# Round off
# cols <- grep("Revenue|Conversion",names(Staples_Corr_By_Prod_Cor_L.M30),value = T)
# Staples_Corr_By_Prod_Cor_L.M30 <- Staples_Corr_By_Prod_Cor_L.M30[,(cols) := round(.SD,2), .SDcols=cols]
# cols <- grep("Visits|Order|Uniq",names(Staples_Corr_By_Prod_Cor_L.M30),value = T)
# Staples_Corr_By_Prod_Cor_L.M30 <- Staples_Corr_By_Prod_Cor_L.M30[,(cols) := ceiling(.SD), .SDcols=cols];rm(cols)
# 
# cols <- grep("Revenue|Conversion",names(Staples_Corr_By_Prod_Cor_G.P30),value = T)
# Staples_Corr_By_Prod_Cor_G.P30 <- Staples_Corr_By_Prod_Cor_G.P30[,(cols) := round(.SD,2), .SDcols=cols]
# cols <- grep("Visits|Order|Uniq",names(Staples_Corr_By_Prod_Cor_G.P30),value = T)
# Staples_Corr_By_Prod_Cor_G.P30 <- Staples_Corr_By_Prod_Cor_G.P30[,(cols) := ceiling(.SD), .SDcols=cols];rm(cols)

#---------------------------------------------------------------------------------------------------------------
#                                       Correlation Test
#---------------------------------------------------------------------------------------------------------------
# Correlation Matrix : Price Competitiveness : Staples/Amazon
Staples_Aggr_Monthly <- data.table(Staples_Aggr_Monthly)
cor(Staples_Aggr_Monthly[Loyalty_Cat=="HIGH",Conversion,Price_Comp])
table(Staples_Aggr_Monthly$Loyalty_Cat)[1]
cor(Staples_Aggr_Monthly[Loyalty_Cat=="MID",Conversion,Price_Comp])
table(Staples_Aggr_Monthly$Loyalty_Cat)[3]
cor(Staples_Aggr_Monthly[Loyalty_Cat=="LOW",Conversion,Price_Comp])
table(Staples_Aggr_Monthly$Loyalty_Cat)[2]

# Correlation by Product
Staples_Aggr_Monthly <- data.table(Staples_Aggr_Monthly)
Staples_Corr_By_Prod <- Staples_Aggr_Monthly[,{Staples_Aggr_Monthly[,cor(Conversion,Price_Comp),by=Product_ID]}]
colnames(Staples_Corr_By_Prod) <- c("Product_ID","Correlation")
setkey(Staples_Corr_By_Prod,"Correlation")

Staples_Corr_By_Prod$Decision_Criterion <- ifelse(Staples_Corr_By_Prod$Correlation<=-0.30,1,
                                                  ifelse(Staples_Corr_By_Prod$Correlation>=0.30,2,0))

# Staples_Corr_By_Prod_Cor_G.P30 <- Staples_Corr_By_Prod[Decision_Criterion==2,]
# Staples_Corr_By_Prod_Cor_L.M30 <- Staples_Corr_By_Prod[Decision_Criterion==1,]

#---------------------------------------------------------------------------------------------------------------
#                                    Regress by DL : Across Quartiles & Deciles
#---------------------------------------------------------------------------------------------------------------

Staples_Reg_DL <- read.table("C:/Yashwanth/Staples PE/1. Input/Iterative_DL.csv",
                         header = TRUE, sep = ",", quote = "\"", dec = "." ,fill=TRUE, comment.char="", as.is=TRUE)

Price_Senitivity <- NA
for(i in c(1:(ncol(Staples_Reg_DL)-2))){
  Price_Senitivity[i] <- coef(lm(Staples_Reg_DL[,i] ~ Price_Comp, data = Staples_Reg_DL))[2]
}

Staples_Sen_Check <- data.frame(cbind(Price_Senitivity,Price_Comp=Staples_Reg_DL$Price_Comp,
                                      Demand_Loyalty=Staples_Reg_DL$Demand_Loyalty))

# Slope of DL
attach(Staples_Sen_Check)
par(mfrow=c(1,2),cex.main=1.2)
plot(Demand_Loyalty,Price_Senitivity,ylab="Price Sensitivity",xlab="Demand Loyalty",main="Price Sensitivity v/s Demand Loyalty")
abline(lm(Price_Senitivity ~ Demand_Loyalty))
detach(Staples_Sen_Check)

attach(Staples_Sen_Check)
par(mfrow=c(1,2),cex.main=1.2)
plot(Price_Senitivity,Price_Comp,ylab="Price Competitiveness",xlab="Price Sensitivity",main="Price Sensitivity v/s Price Competitiveness")
abline(lm(Price_Comp ~ Price_Senitivity))
detach(Staples_Sen_Check)

#---------------------------------------------------------------------------------------------------------------
#                                          Regress by DL : Across all data
#---------------------------------------------------------------------------------------------------------------

# Deciles
quantile(Staples_Aggr_Monthly_Dec$Loyalty,probs = seq(0,1,length = 11))
quantile(Staples_Aggr_Monthly_Dec$OA_Bounce_rate,probs = seq(0,1,length = 11))
quantile(Staples_Aggr_Monthly_Dec$OA_Cart_Views,probs = seq(0,1,length = 11))
quantile(Staples_Aggr_Monthly_Dec$OA_Visit_Freq,probs = seq(0,1,length = 11))

Staples_Aggr_Monthly_Dec <- data.table(Staples_Aggr_Monthly_Dec)
attach(Staples_Aggr_Monthly_Dec)
Staples_Aggr_Monthly_Deci1 <- Staples_Aggr_Monthly_Dec[OA_Visit_Freq<22.37 & Price_Comp<1.01]
Staples_Aggr_Monthly_Deci2 <- Staples_Aggr_Monthly_Dec[OA_Visit_Freq>=22.37 & OA_Visit_Freq<24.94 & Price_Comp>=1.01 & Price_Comp<1.06]
Staples_Aggr_Monthly_Deci3 <- Staples_Aggr_Monthly_Dec[OA_Visit_Freq>=24.94 & OA_Visit_Freq<41.39 & Price_Comp>=1.06 & Price_Comp<1.12]
Staples_Aggr_Monthly_Deci4 <- Staples_Aggr_Monthly_Dec[OA_Visit_Freq>=41.39 & OA_Visit_Freq<43.79 & Price_Comp>=1.12 & Price_Comp<1.17]
Staples_Aggr_Monthly_Deci5 <- Staples_Aggr_Monthly_Dec[OA_Visit_Freq>=43.79 & OA_Visit_Freq<45.43 & Price_Comp>=1.17 & Price_Comp<1.21]
Staples_Aggr_Monthly_Deci6 <- Staples_Aggr_Monthly_Dec[OA_Visit_Freq>=45.43 & OA_Visit_Freq<46.24 & Price_Comp>=1.21 & Price_Comp<1.26]
Staples_Aggr_Monthly_Deci7 <- Staples_Aggr_Monthly_Dec[OA_Visit_Freq>=46.24 & OA_Visit_Freq<47.12 & Price_Comp>=1.26 & Price_Comp<1.35]
Staples_Aggr_Monthly_Deci8 <- Staples_Aggr_Monthly_Dec[OA_Visit_Freq>=47.12 & OA_Visit_Freq<48.50 & Price_Comp>=1.35 & Price_Comp<1.47]
Staples_Aggr_Monthly_Deci9 <- Staples_Aggr_Monthly_Dec[OA_Visit_Freq>=48.50 & OA_Visit_Freq<50.57 & Price_Comp>=1.47 & Price_Comp<1.67]
Staples_Aggr_Monthly_Deci10 <- Staples_Aggr_Monthly_Dec[OA_Visit_Freq>=50.57 & OA_Visit_Freq<88.65 & Price_Comp>=1.67 & Price_Comp<2.49]
detach(Staples_Aggr_Monthly_Dec)

Staples_Sen_Check_All <- data.frame(rbind(coef(lm(Conversion ~ Price_Comp, data = Staples_Aggr_Monthly_Deci1))[2],
                                          coef(lm(Conversion ~ Price_Comp, data = Staples_Aggr_Monthly_Deci2))[2],
                                          coef(lm(Conversion ~ Price_Comp, data = Staples_Aggr_Monthly_Deci3))[2],
                                          coef(lm(Conversion ~ Price_Comp, data = Staples_Aggr_Monthly_Deci4))[2],
                                          coef(lm(Conversion ~ Price_Comp, data = Staples_Aggr_Monthly_Deci5))[2],
                                          coef(lm(Conversion ~ Price_Comp, data = Staples_Aggr_Monthly_Deci6))[2],
                                          coef(lm(Conversion ~ Price_Comp, data = Staples_Aggr_Monthly_Deci7))[2],
                                          coef(lm(Conversion ~ Price_Comp, data = Staples_Aggr_Monthly_Deci8))[2],
                                          coef(lm(Conversion ~ Price_Comp, data = Staples_Aggr_Monthly_Deci9))[2],
                                          coef(lm(Conversion ~ Price_Comp, data = Staples_Aggr_Monthly_Deci10))[2]))
                                    #Staples_Sen_Check$Price_Comp,Staples_Sen_Check$Demand_OA_Bounce_rate)
names(Staples_Sen_Check_All) <- c("Price_Sensitivity")#,"Price_Comp","OA_Bounce_rate")
rm(list = ls()[grep("Deci",ls())])

#---------------------------------------------------------------------------------------------------------------
#                                          Regress by DL : Across all data : Visits > 1000
#---------------------------------------------------------------------------------------------------------------

Staples_Aggr_Monthly_Dec_G1000 <- Staples_Aggr_Monthly_Dec[Staples_Aggr_Monthly_Dec$OA_Visits<=3000,]
describe(data.frame(Staples_Aggr_Monthly_Dec_G1000)[,sapply(data.frame(Staples_Aggr_Monthly_Dec_G1000),is.numeric)])
Staples_Aggr_Monthly_Dec_G1000 <- data.table(Staples_Aggr_Monthly_Dec_G1000)
attach(Staples_Aggr_Monthly_Dec_G1000)
Staples_Aggr_Monthly_Deci1 <- Staples_Aggr_Monthly_Dec_G1000[Loyalty<=0.64 & Price_Comp<=1.01]
Staples_Aggr_Monthly_Deci2 <- Staples_Aggr_Monthly_Dec_G1000[Loyalty>0.64 & Loyalty<=0.66 & Price_Comp>1.01 & Price_Comp<=1.06]
Staples_Aggr_Monthly_Deci3 <- Staples_Aggr_Monthly_Dec_G1000[Loyalty>0.66 & Loyalty<=0.68 & Price_Comp>1.06 & Price_Comp<=1.12]
Staples_Aggr_Monthly_Deci4 <- Staples_Aggr_Monthly_Dec_G1000[Loyalty>0.68 & Loyalty<=0.70 & Price_Comp>1.12 & Price_Comp<=1.17]
Staples_Aggr_Monthly_Deci5 <- Staples_Aggr_Monthly_Dec_G1000[Loyalty>0.70 & Loyalty<=0.71 & Price_Comp>1.17 & Price_Comp<=1.21]
Staples_Aggr_Monthly_Deci6 <- Staples_Aggr_Monthly_Dec_G1000[Loyalty>0.71 & Loyalty<=0.72 & Price_Comp>1.21 & Price_Comp<=1.26]
Staples_Aggr_Monthly_Deci7 <- Staples_Aggr_Monthly_Dec_G1000[Loyalty>0.72 & Loyalty<=0.74 & Price_Comp>1.26 & Price_Comp<=1.35]
Staples_Aggr_Monthly_Deci8 <- Staples_Aggr_Monthly_Dec_G1000[Loyalty>0.74 & Loyalty<=0.76 & Price_Comp>1.35 & Price_Comp<=1.47]
Staples_Aggr_Monthly_Deci9 <- Staples_Aggr_Monthly_Dec_G1000[Loyalty>0.76 & Loyalty<=0.78 & Price_Comp>1.47 & Price_Comp<=1.67]
Staples_Aggr_Monthly_Deci10 <- Staples_Aggr_Monthly_Dec_G1000[Loyalty>0.78 & Loyalty<=0.97 & Price_Comp>1.67 & Price_Comp<=2.49]
detach(Staples_Aggr_Monthly_Dec_G1000)

Staples_Sen_Check_All_G1000 <- data.frame(rbind(coef(lm(Conversion ~ Price_Comp, data = Staples_Aggr_Monthly_Deci1))[2],
                                          coef(lm(Conversion ~ Price_Comp, data = Staples_Aggr_Monthly_Deci2))[2],
                                          coef(lm(Conversion ~ Price_Comp, data = Staples_Aggr_Monthly_Deci3))[2],
                                          coef(lm(Conversion ~ Price_Comp, data = Staples_Aggr_Monthly_Deci4))[2],
                                          coef(lm(Conversion ~ Price_Comp, data = Staples_Aggr_Monthly_Deci5))[2],
                                          coef(lm(Conversion ~ Price_Comp, data = Staples_Aggr_Monthly_Deci6))[2],
                                          coef(lm(Conversion ~ Price_Comp, data = Staples_Aggr_Monthly_Deci7))[2],
                                          coef(lm(Conversion ~ Price_Comp, data = Staples_Aggr_Monthly_Deci8))[2],
                                          coef(lm(Conversion ~ Price_Comp, data = Staples_Aggr_Monthly_Deci9))[2],
                                          coef(lm(Conversion ~ Price_Comp, data = Staples_Aggr_Monthly_Deci10))[2]
                                          ),
                                    Staples_Sen_Check$Price_Comp,Staples_Sen_Check$Demand_Loyalty)
names(Staples_Sen_Check_All_G1000) <- c("Price_Sensitivity","Price_Comp","Demand_Loyalty")
rm(list = ls()[grep("Deci",ls())])

#---------------------------------------------------------------------------------------------------------------
#                                  Regress by DL : Across all data : Visits <= 3000 & collate data
#---------------------------------------------------------------------------------------------------------------

Staples_Sen_Check$DL_Cat <- with(Staples_Sen_Check,ifelse(Demand_Loyalty<=0.71,"LOW","HIGH"))
Staples_Aggr_Monthly_Dec_G1000 <- Staples_Aggr_Monthly_Dec[Staples_Aggr_Monthly_Dec$OA_Visits<=3000,]
describe(data.frame(Staples_Aggr_Monthly_Dec_G1000)[,sapply(data.frame(Staples_Aggr_Monthly_Dec_G1000),is.numeric)])
Staples_Aggr_Monthly_Dec_G1000 <- data.table(Staples_Aggr_Monthly_Dec_G1000)
Staples_Aggr_Monthly_Dec_G1000$DL_Cat <- with(Staples_Aggr_Monthly_Dec_G1000,ifelse(Loyalty<=0.71,"LOW","HIGH"))

attach(Staples_Aggr_Monthly_Dec_G1000)
Staples_Aggr_Monthly_Deci1 <- Staples_Aggr_Monthly_Dec_G1000[DL_Cat=="HIGH",]
Staples_Aggr_Monthly_Deci2 <- Staples_Aggr_Monthly_Dec_G1000[DL_Cat=="LOW",]
detach(Staples_Aggr_Monthly_Dec_G1000)

Staples_Sen_Check_All_G1000 <- data.frame(rbind(coef(lm(Conversion ~ Price_Comp, data = Staples_Aggr_Monthly_Deci1))[2],
  coef(lm(Conversion ~ Price_Comp, data = Staples_Aggr_Monthly_Deci2))[2]),
      unique(Staples_Aggr_Monthly_Dec_G1000$DL_Cat))
names(Staples_Sen_Check_All_G1000) <- c("Price_Sensitivity","Demand_Loyalty")
rm(list = ls()[grep("Deci",ls())])

save.image("C:/Yashwanth/Staples PE/2. Scripts/0. R_data/1. Staples_PE_Aggr.sav.RData")

#---------------------------------------------------------------------------------------------------------------
#                                  Filter Products which are Negatively Correlated
#---------------------------------------------------------------------------------------------------------------

# Correlation by Product
Staples_Corr_By_Prod <- Staples_Aggr_Monthly_Dec[,{Staples_Aggr_Monthly_Dec[,cor(Conversion,Price_Comp),by=Product_ID]}]
colnames(Staples_Corr_By_Prod) <- c("Product_ID","Correlation")
setkey(Staples_Corr_By_Prod,"Correlation")

# Consider Products which has correlation < -0.25.
Staples_Corr_By_Prod_LE0.25 <- Staples_Corr_By_Prod[Correlation<=-0.25]

# Filter data based on filtered products
Staples_Aggr_Monthly_Dec_Filtrd <- merge(Staples_Aggr_Monthly_Dec,Staples_Corr_By_Prod_LE0.25,by="Product_ID")

# Summarise the data
describe(data.frame(Staples_Aggr_Monthly_Dec_Filtrd)[,sapply(data.frame(Staples_Aggr_Monthly_Dec_Filtrd), is.numeric)])

# Price Sensetivity
Staples_Aggr_Monthly_Dec_Filtrd <- Staples_Aggr_Monthly_Dec_Filtrd[Staples_Aggr_Monthly_Dec_Filtrd$OA_Visits<=3000,]
Staples_Aggr_Monthly_Dec_Filtrd <- data.table(Staples_Aggr_Monthly_Dec_Filtrd)
Staples_Aggr_Monthly_Dec_Filtrd$DL_Cat <- with(Staples_Aggr_Monthly_Dec_Filtrd,ifelse(Loyalty<=0.71,"LOW","HIGH"))

attach(Staples_Aggr_Monthly_Dec_Filtrd)
Staples_Aggr_Monthly_Deci1 <- Staples_Aggr_Monthly_Dec_Filtrd[DL_Cat=="HIGH",]
Staples_Aggr_Monthly_Deci2 <- Staples_Aggr_Monthly_Dec_Filtrd[DL_Cat=="LOW",]
detach(Staples_Aggr_Monthly_Dec_Filtrd)

Staples_Aggr_Monthly_Dec_Filtrd <- data.frame(rbind(coef(lm(Conversion ~ Price_Comp, data = Staples_Aggr_Monthly_Deci1))[2],
                                                coef(lm(Conversion ~ Price_Comp, data = Staples_Aggr_Monthly_Deci2))[2]),
                                          unique(Staples_Aggr_Monthly_Dec_Filtrd$DL_Cat))
names(Staples_Aggr_Monthly_Dec_Filtrd) <- c("Price_Sensitivity","Demand_Loyalty")
rm(list = ls()[grep("Deci",ls())])

save.image("C:/Yashwanth/Staples PE/2. Scripts/0. R_data/2. Staples_PE_Aggr.RData")


#---------------------------------------------------------------------------------------------------------------
#                                         T-test : Mean difference
#---------------------------------------------------------------------------------------------------------------

attach(Staples_Aggr_Monthly_Dec)
t.test(Staples_Final_Price,Amz_Final_Price,alternative = "two.sided",mu = 0)         # Reject NH
t.test(Staples_Final_Price,Amz_Final_Price,alternative = "less",mu = 0)              # Fail to reject NH
t.test(Staples_Final_Price,Amz_Final_Price,alternative = "greater",mu = 0)           # Reject NH
t.test(Staples_Final_Price,Amz_Final_Price,alternative = "greater",mu = 4.3)  # Reject NH
detach(Staples_Aggr_Monthly_Dec)

# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
# ---------------- END ---------------- Staples Price Elasticity : Aggregation -------- END --------------------
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
