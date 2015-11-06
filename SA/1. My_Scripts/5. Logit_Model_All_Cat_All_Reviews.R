# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
#                                   Best Seller Model ;  Logistic Ression
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------------
#                                         Input Data and data types
#---------------------------------------------------------------------------------------------------------------

rm(list=setdiff(ls(),"Review_Aggr_Data"))
Review_Aggr_Model_Data <- read.table("C:/Yashwanth/Sports Authority/3. Model/3. Model_Input_by_Cat_All_Reviews.csv",
                            header = TRUE, sep = ",", quote = "\"", dec = "." ,fill=TRUE, comment.char="", as.is=TRUE)

Review_Aggr_Model_Data <- with(Review_Aggr_Model_Data,Review_Aggr_Model_Data[order(Sales_Units,decreasing = TRUE),])
Review_Aggr_Model_Data$Sales <- c(rep(1,100),rep(0,length(101:nrow(Review_Aggr_Model_Data))))
Review_Aggr_Model_Data$Sales <- as.factor(Review_Aggr_Model_Data$Sales)
Review_Aggr_Model_Data$Product_ID <- as.factor(Review_Aggr_Model_Data$Product_ID);str(Review_Aggr_Model_Data)
table(Review_Aggr_Model_Data$Sales)

#---------------------------------------------------------------------------------------------------------------
#                                        Descriptive Statistics & Correlation
#---------------------------------------------------------------------------------------------------------------

library(psych)
describe(Review_Aggr_Model_Data[,sapply(Review_Aggr_Model_Data,is.numeric)])

cor(Review_Aggr_Model_Data[,sapply(Review_Aggr_Model_Data,is.numeric)])
cor(Review_Aggr_Model_Data[,sapply(Review_Aggr_Model_Data,is.numeric)],use = "na.or.complete")

Split_Review_Aggr_Model_Data <- split(Review_Aggr_Model_Data[sapply(Review_Aggr_Model_Data, is.numeric)],Review_Aggr_Model_Data$Category)
lapply(Split_Review_Aggr_Model_Data, cor, use = "na.or.complete");rm(Split_Review_Aggr_Model_Data)

# Review_Aggr_Model_Data <- data.table(Review_Aggr_Model_Data)
# Review_Aggr_Model_Data[,{Review_Aggr_Model_Data[,cor(Reviews_Q1,Reviews_Q2),by=Category]}]

#---------------------------------------------------------------------------------------------------------------
#                                         Weight Of Evidence & Information Value
#---------------------------------------------------------------------------------------------------------------

library(woe)
C_Bin <- 10
WOE_Q1 <- woe(Review_Aggr_Model_Data,"Reviews_Q1",FALSE,"Sales",C_Bin,Bad = 0,Good = 1)
WOE_Q1 <- cbind(WOE_Q1,Quarters=rep("Reviews Q1",nrow(WOE_Q1)))

WOE_Q2 <- woe(Review_Aggr_Model_Data,"Reviews_Q2",FALSE,"Sales",C_Bin,Bad = 0,Good = 1)
WOE_Q2 <- cbind(WOE_Q2,Quarters=rep("Reviews Q2",nrow(WOE_Q2)))

WOE_Q3 <- woe(Review_Aggr_Model_Data,"Reviews_Q3",FALSE,"Sales",C_Bin,Bad = 0,Good = 1)
WOE_Q3 <- cbind(WOE_Q3,Quarters=rep("Reviews Q3",nrow(WOE_Q3)))

WOE_Q4 <- woe(Review_Aggr_Model_Data,"Reviews_Q4",FALSE,"Sales",C_Bin,Bad = 0,Good = 1)
WOE_Q4 <- cbind(WOE_Q4,Quarters=rep("Reviews Q4",nrow(WOE_Q4)))

WOE <- rbind(WOE_Q1,WOE_Q2,WOE_Q3,WOE_Q4);rm(WOE_Q1,WOE_Q2,WOE_Q3,WOE_Q4)

WOE$IV_Dec <- with(WOE,ifelse(IV == Inf | IV >= 0.0 & IV <= 0.02, "Unpredictive",
                        ifelse(IV > 0.02 & IV < 0.1,"Weak Predictive",
                               ifelse(IV >= 0.1 & IV <= 0.3, "Medium Predictive","Strong Predictive"))))

# Conclusion : As IV > 0.1(Medium Predictive) when most of the Reviews are 1, drill down data when Reviews = 1 across all Quarters.

#---------------------------------------------------------------------------------------------------------------
#                                        Logistic Regression : Baseline
#---------------------------------------------------------------------------------------------------------------

# Data Sampling
set.seed(100)
SA_IND  <- sample(nrow(Review_Aggr_Model_Data),size=round(((nrow(Review_Aggr_Model_Data)/100)*70)+1,0))
SA_TRAIN <- Review_Aggr_Model_Data[SA_IND,]
SA_TEST <- Review_Aggr_Model_Data[-SA_IND,]

SA_LR <- glm(Sales ~ Reviews_Q1+Reviews_Q2+Reviews_Q3+Reviews_Q4,data=SA_TRAIN,family=binomial(link="logit"))
summary(SA_LR)
par(mfrow=c(2,2),mar=c(3,2,2,1))
plot(SA_LR)

# logLikelihood & BIC
library(lmtest)
logLik(SA_LR)
BIC(logLik(step(SA_LR,direction='forward')))
BIC(logLik(step(SA_LR,direction='backward')))

# Wald's test : Overall variance explained
library(aod)
wald.test(Sigma = vcov(SA_LR),b = coef(SA_LR), Terms = 2:length(coef(SA_LR)))

# Prediction & Odds ratio
SA_TEST$Prob <-  predict.glm(SA_LR,SA_TEST,type="response")
SA_TEST$Pred <- ifelse(SA_TEST$Prob>0.5,1,0)
SA_TEST$Odds <- round(SA_TEST$Prob/(1-SA_TEST$Prob),2)
View(exp(cbind(OR = coef(SA_LR), confint(SA_LR))))

# Classification Accuracy/ Confusion Matrix
library(xtable)
library(reshape)
library(data.table)
table(Actual=SA_TEST$Sales,Predict=SA_TEST$Pred)
DT <- cbind(melt(table(Actual=SA_TEST$Sales,Predict=SA_TEST$Pred)),
            Decision=c("False Positive","True Negative","False Negative","True Positive"))
DT <- data.table(DT)
setorderv(DT,"Decision",order = -1);DT

Class_Table <- xtable(table(Actual=SA_TEST$Sales,Predict=SA_TEST$Pred))
cbind(Class_Table <- xtable(table(Actual=SA_TEST$Sales,Predict=SA_TEST$Pred)),
      Per_Correct = c((Class_Table[1,1]/nrow(SA_TEST)*100),(Class_Table[2,2]/nrow(SA_TEST)*100)))
sum(cbind(Class_Table <- xtable(table(Actual=SA_TEST$Sales,Predict=SA_TEST$Pred)),
          Per_Correct = c((Class_Table[1,1]/nrow(SA_TEST)*100),(Class_Table[2,2]/nrow(SA_TEST)*100)))[3])

#---------------------------------------------------------------------------------------------------------------
#                                        Identifying Outliers
#           Note :  it is NOT acceptable to drop an observation just because it is an outlier # Check Ref URL
#---------------------------------------------------------------------------------------------------------------

# Standardised Residuals
library(MASS)
SA_SR <- cbind(SA_TRAIN,Std_Res=stdres(SA_LR))
SA_SR <- cbind(SA_SR,Std_Res_Dec = ifelse(SA_SR$Std_Res>3 | SA_SR$Std_Res < -3,"YES","NO"))

# Cook's distance : Cook's distance or Cook's D is a commonly used estimate of the influence of a data point when
#                   performing least squares regression analysis.

infl <- influence(SA_LR, do.coef = FALSE)
SA_SR_CD <- cbind(SA_SR, Cook_D=cooks.distance(SA_LR, infl = influence(SA_LR, do.coef = FALSE),
                                               res = infl$pear.res,
                                               dispersion = summary(SA_LR)$dispersion,
                                               hat = infl$hat))

SA_SR_CD <- subset(SA_SR_CD,Std_Res_Dec=="NO" & Cook_D<1,names(SA_SR_CD))
cor(SA_SR_CD[,c(2,4:7)])

#---------------------------------------------------------------------------------------------------------------
#                                        Revised Logistic Regression
#---------------------------------------------------------------------------------------------------------------

table(SA_SR_CD$Sales)
SA_LR_Rev <- glm(Sales ~ Reviews_Q1+Reviews_Q2+Reviews_Q3+Reviews_Q4,
                 data=SA_SR_CD,family=binomial(link="logit"))

summary(SA_LR_Rev)
View(exp(cbind(OR = coef(SA_LR_Rev), confint(SA_LR_Rev))))
par(mfrow=c(2,2),mar=c(3,2,2,1))
plot(SA_LR_Rev)

# logLikelihood & BIC
library(lmtest)
logLik(SA_LR_Rev)
BIC(logLik(step(SA_LR_Rev,direction='forward')))
BIC(logLik(step(SA_LR_Rev,direction='backward')))

# Wald's test : Overall variance explained
wald.test(Sigma = vcov(SA_LR_Rev),b = coef(SA_LR_Rev), Terms = 2:length(coef(SA_LR_Rev)))

# Prediction & Odds
SA_TEST$Prob <-  predict.glm(SA_LR_Rev,SA_TEST,type="response")
SA_TEST$Pred <- ifelse(SA_TEST$Prob>0.5,1,0)
SA_TEST$Odds <- round(SA_TEST$Prob/(1-SA_TEST$Prob),2)

# Classification Accuracy/ Confusion Matrix
table(Actual=SA_TEST$Sales,Predict=SA_TEST$Pred)
DT <- cbind(melt(table(Actual=SA_TEST$Sales,Predict=SA_TEST$Pred)),
            Decision=c("False Positive","True Negative","False Negative","True Positive"))
DT <- data.table(DT)
setorderv(DT,"Decision",order = -1);DT

Class_Table <- xtable(table(Actual=SA_TEST$Sales,Predict=SA_TEST$Pred))
cbind(Class_Table <- xtable(table(Actual=SA_TEST$Sales,Predict=SA_TEST$Pred)),
      Per_Correct = c((Class_Table[1,1]/nrow(SA_TEST)*100),(Class_Table[2,2]/nrow(SA_TEST)*100)))
sum(cbind(Class_Table <- xtable(table(Actual=SA_TEST$Sales,Predict=SA_TEST$Pred)),
          Per_Correct = c((Class_Table[1,1]/nrow(SA_TEST)*100),(Class_Table[2,2]/nrow(SA_TEST)*100)))[3])

# Conclusion : Since logistic regression with removing Outliers & influential cases has less than 2% more accuracy in classifying cases
#               than logistic regression with all cases, we conclude that "Baseline model is better than Revised model".

#---------------------------------------------------------------------------------------------------------------
#                                           Complete data operation
#---------------------------------------------------------------------------------------------------------------

# Prediction & Odds ratio
Review_Aggr_Model_Data$Prob <-  predict.glm(SA_LR,Review_Aggr_Model_Data,type="response")
Review_Aggr_Model_Data$Pred_30 <- ifelse(Review_Aggr_Model_Data$Prob>0.3,1,0)
Review_Aggr_Model_Data$Pred_50 <- ifelse(Review_Aggr_Model_Data$Prob>0.5,1,0)
Review_Aggr_Model_Data$Odds <- round(Review_Aggr_Model_Data$Prob/(1-Review_Aggr_Model_Data$Prob),2)
View(exp(cbind(OR = coef(SA_LR), confint(SA_LR))))
# Odds Interpretation : Considering an Item is a TOP-Product is 2.40 times more likely than to consider an item is NOT a TOP-Product.
# Odds Ratio : If there is one unit increase in Review then there will be ~0.6% increase in Sales.
#              If there is one unit increase in Search then there will be ~0.000000% increase in Sales.
#              If there is one unit increase in Social Signal then there will be ~0.00000% increase in Sales.
#              If there is one unit increase in Backlinks then there will be ~0.2% increase in Sales.

# 50%
table(Actual=Review_Aggr_Model_Data$Sales,Predict=Review_Aggr_Model_Data$Pred_50)
Class_Table <- xtable(table(Actual=Review_Aggr_Model_Data$Sales,Predict=Review_Aggr_Model_Data$Pred_50))
cbind(Class_Table <- xtable(table(Actual=Review_Aggr_Model_Data$Sales,Predict=Review_Aggr_Model_Data$Pred_50)),
      Per_Correct = c((Class_Table[1,1]/nrow(Review_Aggr_Model_Data)*100),(Class_Table[2,2]/nrow(Review_Aggr_Model_Data)*100)))
sum(cbind(Class_Table <- xtable(table(Actual=Review_Aggr_Model_Data$Sales,Predict=Review_Aggr_Model_Data$Pred_50)),
          Per_Correct = c((Class_Table[1,1]/nrow(Review_Aggr_Model_Data)*100),(Class_Table[2,2]/nrow(Review_Aggr_Model_Data)*100)))[3])

# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
# -------------- END --------------------- Best Seller Model ;  Logistic Ression ----------- END ---------------
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------

# setwd("C:/Yashwanth/Sports Authority/1. Inputs/2. Review All/9. All Category files/")
# Review_Aggr_Model_Data <- list.files(pattern="*.csv")
# for (i in 1:length(Review_Aggr_Model_Data)) {
#   assign(Review_Aggr_Model_Data[i], read.csv(Review_Aggr_Model_Data[i],stringsAsFactors = FALSE,header = TRUE, sep = ",", quote = "\"", fill=TRUE, 
#                                comment.char="", as.is=TRUE))
# }
# 
# Activity_Monitors <- Activity_Monitors.csv ; BaseballBats <- BaseballBats.csv ; Cleats <- Cleats.csv;
# Golf_Complete_Sets <- Golf_Complete_Sets.csv ; NFL_Apparel <- NFL_Apparel.csv ; Training_Aid <- Training_Aid.csv ;
# Treadmill <- Treadmill.csv ; Womens_running_shoes <- Womens_running_shoes.csv
# rm(list=ls(pattern = ".csv"));ls() ; Activity_Monitors$ReviewID <- NULL
# 
# 
# Review_Aggr_Model_Data <- rbind(Activity_Monitors,BaseballBats,Cleats,Golf_Complete_Sets,NFL_Apparel,Training_Aid,Treadmill,Womens_running_shoes)
# str(Review_Aggr_Model_Data);# write.csv(Review_Aggr_Model_Data,"Review_Aggr_Model_Data.csv",row.names = F)
