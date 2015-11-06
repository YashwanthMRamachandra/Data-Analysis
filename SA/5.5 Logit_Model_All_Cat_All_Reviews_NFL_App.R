
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
#                                   Best Seller Model ;  Logistic Ression
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------------
#                                         Input Data and data types
#---------------------------------------------------------------------------------------------------------------

load("C:/Yashwanth/Sports Authority/3. Output/Review_Aggr_v2.RData")
rm(list=setdiff(ls(),c("Review_Aggr_Data","Review_Aggr_Model_Data")))
library(data.table)
Review_Aggr_Model_Data <- data.table(Review_Aggr_Model_Data)
Review_Aggr_NFL_App_Model_Data <- Review_Aggr_Model_Data[Category=="NFL Apparel"]
Review_Aggr_NFL_App_Model_Data <- with(Review_Aggr_NFL_App_Model_Data,Review_Aggr_NFL_App_Model_Data[order(Sales_Units,decreasing = TRUE),])
Review_Aggr_NFL_App_Model_Data$Sales <- c(rep(1,10),rep(0,length(11:nrow(Review_Aggr_NFL_App_Model_Data))))
Review_Aggr_NFL_App_Model_Data$Sales <- as.factor(Review_Aggr_NFL_App_Model_Data$Sales)
Review_Aggr_NFL_App_Model_Data$Product_ID <- as.factor(Review_Aggr_NFL_App_Model_Data$Product_ID);str(Review_Aggr_NFL_App_Model_Data)
table(Review_Aggr_NFL_App_Model_Data$Sales)

#---------------------------------------------------------------------------------------------------------------
#                                        Descriptive Statistics & Correlation
#---------------------------------------------------------------------------------------------------------------

Review_Aggr_NFL_App_Model_Data <- data.frame(Review_Aggr_NFL_App_Model_Data)

library(psych)
describe(Review_Aggr_NFL_App_Model_Data[,sapply(Review_Aggr_NFL_App_Model_Data,is.numeric)])

cor(Review_Aggr_NFL_App_Model_Data[,sapply(Review_Aggr_NFL_App_Model_Data,is.numeric)])
cor(Review_Aggr_NFL_App_Model_Data[,sapply(Review_Aggr_NFL_App_Model_Data,is.numeric)],use = "na.or.complete")

#---------------------------------------------------------------------------------------------------------------
#                                        Logistic Regression : Baseline
#---------------------------------------------------------------------------------------------------------------

Review_Aggr_NFL_App_Model_Data <- data.table(Review_Aggr_NFL_App_Model_Data)

# Data Sampling
set.seed(100)
SA_IND  <- sample(nrow(Review_Aggr_NFL_App_Model_Data),size=round(((nrow(Review_Aggr_NFL_App_Model_Data)/100)*70)+1,0))
SA_TRAIN <- Review_Aggr_NFL_App_Model_Data[SA_IND,]
SA_TEST <- Review_Aggr_NFL_App_Model_Data[-SA_IND,]

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
#cor(SA_SR_CD[,c(2,4:7)])

#---------------------------------------------------------------------------------------------------------------
#                                        Revised Logistic Regression
#---------------------------------------------------------------------------------------------------------------

table(SA_SR_CD$Sales)
SA_LR_Rev <- glm(Sales ~ Reviews_Q1+Reviews_Q2+Reviews_Q3+Reviews_Q4,data=SA_SR_CD,family=binomial(link="logit"))
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

# Conclusion : Since logistic regression with removing Outliers & influential cases has more than 2% more accuracy in classifying cases
#               than logistic regression with all cases, we conclude that "Revised model is better than Baseline model".

#---------------------------------------------------------------------------------------------------------------
#                                           Complete data operation
#---------------------------------------------------------------------------------------------------------------

# Prediction & Odds ratio
Review_Aggr_NFL_App_Model_Data$Prob <-  predict.glm(SA_LR_Rev,Review_Aggr_NFL_App_Model_Data,type="response")
Review_Aggr_NFL_App_Model_Data$Pred_30 <- ifelse(Review_Aggr_NFL_App_Model_Data$Prob>0.3,1,0)
Review_Aggr_NFL_App_Model_Data$Pred_50 <- ifelse(Review_Aggr_NFL_App_Model_Data$Prob>0.5,1,0)
Review_Aggr_NFL_App_Model_Data$Odds <- round(Review_Aggr_NFL_App_Model_Data$Prob/(1-Review_Aggr_NFL_App_Model_Data$Prob),2)
View(exp(cbind(OR = coef(SA_LR), confint(SA_LR))))
# Odds Interpretation : Considering an Item is a TOP-Product is 2.40 times more likely than to consider an item is NOT a TOP-Product.
# Odds Ratio : If there is one unit increase in Review then there will be ~0.6% increase in Sales.
#              If there is one unit increase in Search then there will be ~0.000000% increase in Sales.
#              If there is one unit increase in Social Signal then there will be ~0.00000% increase in Sales.
#              If there is one unit increase in Backlinks then there will be ~0.2% increase in Sales.

# 50%
table(Actual=Review_Aggr_NFL_App_Model_Data$Sales,Predict=Review_Aggr_NFL_App_Model_Data$Pred_50)
Class_Table <- xtable(table(Actual=Review_Aggr_NFL_App_Model_Data$Sales,Predict=Review_Aggr_NFL_App_Model_Data$Pred_50))
cbind(Class_Table <- xtable(table(Actual=Review_Aggr_NFL_App_Model_Data$Sales,Predict=Review_Aggr_NFL_App_Model_Data$Pred_50)),
      Per_Correct = c((Class_Table[1,1]/nrow(Review_Aggr_NFL_App_Model_Data)*100),(Class_Table[2,2]/nrow(Review_Aggr_NFL_App_Model_Data)*100)))
sum(cbind(Class_Table <- xtable(table(Actual=Review_Aggr_NFL_App_Model_Data$Sales,Predict=Review_Aggr_NFL_App_Model_Data$Pred_50)),
          Per_Correct = c((Class_Table[1,1]/nrow(Review_Aggr_NFL_App_Model_Data)*100),(Class_Table[2,2]/nrow(Review_Aggr_NFL_App_Model_Data)*100)))[3])

# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
# -------------- END --------------------- Best Seller Model ;  Logistic Ression ----------- END ---------------
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
