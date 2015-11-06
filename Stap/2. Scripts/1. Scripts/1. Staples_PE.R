# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
#                                           Staples : Price Elasticity
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------

rm(list=ls())
Staples_IP <- read.table("C:/Yashwanth/Staples PE/1. Input/Staples_Input_20150827.csv",
                  header = TRUE, sep = ",", quote = "\"", dec = "." ,fill=TRUE, comment.char="", as.is=TRUE)
str(Staples_IP)
Staples_IP$Extraction_Date <- as.Date(Staples_IP$Extraction_Date)
Staples_IP$Product_ID <- as.factor(Staples_IP$Product_ID)

#---------------------------------------------------------------------------------------------------------------
#                                       Define Attributes
#---------------------------------------------------------------------------------------------------------------

# OCPVisit & OCPVisitor
Staples_IP$Google_OCPVisit <- Staples_IP$Google_Order/Staples_IP$Google_Visits
Staples_IP$Google_OCPVisit[is.nan(Staples_IP$Google_OCPVisit)] <- 0
Staples_IP$SC_OCPVisit <- Staples_IP$SC_Order/Staples_IP$SC_Visits
Staples_IP$SC_OCPVisit[is.nan(Staples_IP$SC_OCPVisit)] <- 0
Staples_IP$Direct_OCPVisit <- Staples_IP$Direct_Order/Staples_IP$Direct_Visits
Staples_IP$Direct_OCPVisit[is.nan(Staples_IP$Direct_OCPVisit)] <- 0

Staples_IP$Google_OCPVisitor <- Staples_IP$Google_Order/Staples_IP$Google_Unique_Visitors
Staples_IP$Google_OCPVisitor[is.nan(Staples_IP$Google_OCPVisitor)] <- 0
Staples_IP$SC_OCPVisitor <- Staples_IP$SC_Order/Staples_IP$SC_Unique_Visitors
Staples_IP$SC_OCPVisitor[is.nan(Staples_IP$SC_OCPVisitor)] <- 0
Staples_IP$Direct_OCPVisitor <- Staples_IP$Direct_Order/Staples_IP$Direct_Unique_Visitors
Staples_IP$Direct_OCPVisitor[is.nan(Staples_IP$Direct_OCPVisitor)] <- 0

# Overall 
library(data.table)
attach(Staples_IP)
Staples_IP$OA_Visits <- Google_Visits+SC_Visits+Direct_Visits
Staples_IP$OA_Revenue <- Google_Revenue+SC_Revenue+Direct_Revenue 
Staples_IP$OA_Orders <- Google_Order+SC_Order+Direct_Order
Staples_IP$OA_Uniq_Visitor <- Google_Unique_Visitors+SC_Unique_Visitors+Direct_Unique_Visitors
detach(Staples_IP)

Staples_IP$OA_OCPVisit <- Staples_IP$OA_Orders/Staples_IP$OA_Visits
Staples_IP$OA_OCPVisit[is.nan(Staples_IP$OA_OCPVisit)] <- 0
Staples_IP$OA_OCPVisitor <- Staples_IP$OA_Orders/Staples_IP$OA_Uniq_Visitor
Staples_IP$OA_OCPVisitor[is.na(Staples_IP$OA_OCPVisitor)] <- 0

# Result
attach(Staples_IP)
Staples_IP$Google_Per_Visit <- round((Google_Unique_Visitors+SC_Unique_Visitors)/OA_Uniq_Visitor,2)
Staples_IP$Google_Per_Visit[is.nan(Staples_IP$Google_Per_Visit)] <- 0
Staples_IP$Google_Per_Visit[is.infinite(Staples_IP$Google_Per_Visit)] <- 0
Staples_IP$Dir_Per_Visit <- Direct_Unique_Visitors/OA_Uniq_Visitor
Staples_IP$Dir_Per_Visit[is.nan(Staples_IP$Dir_Per_Visit)] <- 0
detach(Staples_IP)

Staples_IP$Per_Visit_Diff <- Staples_IP$Dir_Per_Visit-Staples_IP$Google_Per_Visit

#---------------------------------------------------------------------------------------------------------------
#                                       Define Loyalties
#---------------------------------------------------------------------------------------------------------------

Staples_IP$Loyalty <- Staples_IP$Dir_Per_Visit
Staples_IP$Conversion <- Staples_IP$OA_OCPVisitor
Staples_IP$Price_Comp <-  round(Staples_IP$Staples_Final_Price/Staples_IP$Amz_Final_Price,2)

Staples_IP$Loyalty_Cat <-  ifelse(Staples_IP$Loyalty > 0.75,"HIGH",ifelse(Staples_IP$Loyalty < 0.60,"MID","LOW"))
Staples_IP$Conversion_Cat <- ifelse(Staples_IP$Conversion > 0.40,"HIGH",ifelse(Staples_IP$Conversion < 0.25,"MID","LOW"))
Staples_IP$Price_Comp_Cat <- ifelse(Staples_IP$Price_Comp > 1,"HIGH",ifelse(Staples_IP$Price_Comp < 0.9,"MID","LOW"))

#---------------------------------------------------------------------------------------------------------------
#                                       Descriptive Statistics
#---------------------------------------------------------------------------------------------------------------

library(psych)
View(describe(Staples_IP[,sapply(Staples_IP,is.numeric)]))
#summary(Staples_IP)
#summary(Staples_IP[,sapply(Staples_IP,is.numeric)])

# Correlation Matrix : All
cor(Staples_IP[,sapply(Staples_IP,is.numeric)])

#---------------------------------------------------------------------------------------------------------------
#                                       Correlation Test
#---------------------------------------------------------------------------------------------------------------
# Correlation Matrix : Price Competitiveness : Staples/Amazon
Staples_IP <- data.table(Staples_IP)
cor(Staples_IP[Loyalty_Cat=="HIGH",Conversion,Price_Comp])
table(Staples_IP$Loyalty_Cat)[1]
cor(Staples_IP[Loyalty_Cat=="MID",Conversion,Price_Comp])
table(Staples_IP$Loyalty_Cat)[3]
cor(Staples_IP[Loyalty_Cat=="LOW",Conversion,Price_Comp])
table(Staples_IP$Loyalty_Cat)[2]

# Correlation Matrix : Loyalty Collapse
Staples_IP$Loyalty_Cat_Collapse <- ifelse(Staples_IP$Loyalty_Cat=="MID","HIGH",Staples_IP$Loyalty_Cat)
cor(Staples_IP[Loyalty_Cat_Collapse=="HIGH",Conversion,PC_SA])
table(Staples_IP$Loyalty_Cat_Collapse)[1]
cor(Staples_IP[Loyalty_Cat_Collapse=="LOW",Conversion,PC_SA])
table(Staples_IP$Loyalty_Cat_Collapse)[2]


# Correlation Matrix : Loyalty Collapse Cut-off : HIGH > 70% , LOW Otherwise
Staples_IP$Loyalty_Cat_Collapse_Cut <- ifelse(Staples_IP$Loyalty > 0.85,"HIGH",ifelse(Staples_IP$Loyalty<0.70,"LOW","MID"))
Staples_IP$Loyalty_Cat_Collapse_Cut <- ifelse(Staples_IP$Loyalty_Cat_Collapse_Cut=="MID","HIGH",Staples_IP$Loyalty_Cat_Collapse_Cut)
cor(Staples_IP[Loyalty_Cat_Collapse_Cut=="HIGH",Conversion,PC_SA])
table(Staples_IP$Loyalty_Cat_Collapse_Cut)[1]
cor(Staples_IP[Loyalty_Cat_Collapse_Cut=="LOW",Conversion,PC_SA])
table(Staples_IP$Loyalty_Cat_Collapse_Cut)[2]

#---------------------------------------------------------------------------------------------------------------
#                                           Correlation Test
#---------------------------------------------------------------------------------------------------------------

# Correlation Matrix : Price Competitiveness : Staples/Amazon
Staples_IP <- data.table(Staples_IP)
cor.test(Staples_IP[Loyalty_Cat=="HIGH",Conversion,Price_Comp])
table(Staples_IP$Loyalty_Cat)[1]
cor.test(Staples_IP[Loyalty_Cat=="MID",Conversion,Price_Comp])
table(Staples_IP$Loyalty_Cat)[3]
cor.test(Staples_IP[Loyalty_Cat=="LOW",Conversion,Price_Comp])
table(Staples_IP$Loyalty_Cat)[2]

#---------------------------------------------------------------------------------------------------------------
#                                       Weight of Evidence & Information Value
#---------------------------------------------------------------------------------------------------------------

library(woe)
Staples_IP$Conv_Ind <- with(Staples_IP,ifelse(Conversion > 0.3,1,0))
WOE_Visits <- woe(Staples_IP,"OA_Visits",FALSE,"Conv_Ind",10,Bad=0,Good=1)
WOE_Visits$IV_Dec <- with(WOE_Visits,ifelse(IV == Inf | IV >= 0.0 & IV <= 0.02, "Unpredictive",
                              ifelse(IV > 0.02 & IV < 0.1,"Weak Predictive",
                                     ifelse(IV >= 0.1 & IV <= 0.3, "Medium Predictive","Strong Predictive"))))

# Ref : http://ucanalytics.com/blogs/information-value-and-weight-of-evidencebanking-case/

#---------------------------------------------------------------------------------------------------------------
#                                       Descriptive Statistics : Scatter plot
#---------------------------------------------------------------------------------------------------------------

dev.off()
par(mfrow=c(1,2),mar=c(3,2,2,1))
plot(Staples_IP[Loyalty_Cat_Collapse=="HIGH",PC_SA],Staples_IP[Loyalty_Cat_Collapse=="HIGH",Conversion],
     type = "p",xlab="Price Competitiveness",ylab="Conversion",main="Conversion vs Price Competitiveness with HIGH DL")
abline(lm(Staples_IP[Loyalty_Cat_Collapse=="HIGH",Conversion] ~ Staples_IP[Loyalty_Cat_Collapse=="HIGH",PC_SA],
          data = Staples_IP))


plot(Staples_IP[Loyalty_Cat_Collapse=="LOW",PC_SA],Staples_IP[Loyalty_Cat_Collapse=="LOW",Conversion],
     type = "p",xlab="Price Competitiveness",ylab="Conversion",main="Conversion vs Price Competitiveness with LOW DL")
abline(lm(Staples_IP[Loyalty_Cat_Collapse=="LOW",Conversion] ~ Staples_IP[Loyalty_Cat_Collapse=="LOW",PC_SA],
          data = Staples_IP))


# Alternate Cut-off : Increase the cut-off to 85% in defining Loyalty
dev.off()
par(mfrow=c(1,2),mar=c(3,2,2,1))
plot(Staples_IP[Loyalty_Cat_Collapse_Cut=="HIGH",PC_SA],Staples_IP[Loyalty_Cat_Collapse_Cut=="HIGH",Conversion],
     type = "p",xlab="Price Competitiveness",ylab="Conversion",main="Conversion vs Price Competitiveness with HIGH DL")
abline(lm(Staples_IP[Loyalty_Cat_Collapse_Cut=="HIGH",Conversion] ~ Staples_IP[Loyalty_Cat_Collapse_Cut=="HIGH",PC_SA],
          data = Staples_IP))


plot(Staples_IP[Loyalty_Cat_Collapse_Cut=="LOW",PC_SA],Staples_IP[Loyalty_Cat_Collapse_Cut=="LOW",Conversion],
     type = "p",xlab="Price Competitiveness",ylab="Conversion",main="Conversion vs Price Competitiveness with LOW DL")
abline(lm(Staples_IP[Loyalty_Cat_Collapse_Cut=="LOW",Conversion] ~ Staples_IP[Loyalty_Cat_Collapse_Cut=="LOW",PC_SA],
          data = Staples_IP))

# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
# ------------- END --------------------- Staples : Price Elasticity -------------------- END ------------------
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------

# #---------------------------------------------------------------------------------------------------------------
# #                                       Order Conversion Per Visit & Per Visitor
# #---------------------------------------------------------------------------------------------------------------
# 
# attach(Staples_IP)
# Staples_IP$Google_OCPVisit <- Google_Order/Google_Visits
# Staples_IP$Google_OCPVisit[is.nan(Staples_IP$Google_OCPVisit)] <- 0
# Staples_IP$Google_OCPVisitor <- Google_Order/Google_Unique_Visitors
# Staples_IP$Google_OCPVisitor[is.nan(Staples_IP$Google_OCPVisitor)] <- 0
# 
# Staples_IP$SC_OCPVisit <- SC_Order/SC_Visits
# Staples_IP$SC_OCPVisit[is.nan(Staples_IP$SC_OCPVisit)] <- 0
# Staples_IP$SC_OCPVisitor <- SC_Order/SC_Unique_Visitors
# Staples_IP$SC_OCPVisitor[is.nan(Staples_IP$SC_OCPVisitor)] <- 0
#   
# Staples_IP$Direct_OCPVisit <- Direct_Order/Direct_Visits
# Staples_IP$Direct_OCPVisit[is.na(Staples_IP$Direct_OCPVisit)] <- 0
# Staples_IP$Direct_OCPVisitor <- Direct_Order/Direct_Unique_Visitors
# Staples_IP$Direct_OCPVisitor[is.nan(Staples_IP$Direct_OCPVisitor)] <- 0
# detach(Staples_IP)
# 
# Staples_IP <- cbind(Staples_IP[,grep(paste(toMatch<-c("Goog","SC","Dir"),collapse = "|"),names(Staples_IP),value = TRUE,invert = TRUE)],
#                 Staples_IP[,grep("Google",names(Staples_IP))],Staples_IP[,grep("SC",names(Staples_IP))],
#                 Staples_IP[,grep("Direct",names(Staples_IP))])
