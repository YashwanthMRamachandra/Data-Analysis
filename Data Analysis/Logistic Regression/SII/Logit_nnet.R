# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
#                                           Logistic Ression
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------

#---------------------------------------------------------------------------------------------------------------
#                                         Data and data types
#---------------------------------------------------------------------------------------------------------------

rm(list=ls())
setwd("C:/Yashwanth/Data Analysis/Logistic Regression/SII/")
SII <- read.table("Output_all.csv",
                      header = TRUE, sep = ",", quote = "\"", dec = "." ,fill=TRUE, comment.char="", as.is=TRUE)
str(SII)
SII$L_unitsales <- as.factor(SII$L_unitsales)

#---------------------------------------------------------------------------------------------------------------
#                                       Descriptive Statistics
#---------------------------------------------------------------------------------------------------------------

library(psych)
describe(SII[,sapply(SII,is.numeric)])

#summary(SII)
#summary(SII[,sapply(SII,is.numeric)])
cor(SII[,sapply(SII,is.numeric)])

# Inference : All the variables are less positively correlated.

# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
#                                           Exploratory Data Analysis
# --------------------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------------
#                                       Distribution of Variables : Histogram
#---------------------------------------------------------------------------------------------------------------

dev.off()
par(mfrow=c(2,1),mar=c(3,2,2,1))
hist(SII$Unit_sales,prob=T,xlab="Sales Units",main="Sales distribution")
curve(dnorm(x,mean=mean(SII$Unit_sales),sd=sd(SII$Unit_sales)),add=TRUE)

hist(log(SII$Unit_sales+1-min(SII$Unit_sales)),prob=T,xlab="Sales Units",main="Sales distribution")
curve(dnorm(x,mean=mean(log(SII$Unit_sales+1-min(SII$Unit_sales))),sd=sd(log(SII$Unit_sales+1-min(SII$Unit_sales)))),add=TRUE)

dev.off()
par(mfrow=c(2,2),mar=c(3,2,2,1))
hist(SII$Review_all,prob=T,xlab="Reviews",main="Review distribution")
curve(dnorm(x,mean=mean(SII$Review_all),sd=sd(SII$Review_all)),add=TRUE)

hist(log(SII$Review_all+1-min(SII$Review_all)),prob=T,xlab="Reviews",main="Review distribution")
curve(dnorm(x,mean=mean(log(SII$Review_all+1-min(SII$Review_all))),sd=sd(log(SII$Review_all+1-min(SII$Review_all)))),add=TRUE)

hist(SII$search,prob=T,xlab="Search",main="Search distribution")
curve(dnorm(x,mean=mean(SII$search),sd=sd(SII$search)),add=TRUE)

hist(log(SII$search+1-min(SII$search)),prob=T,xlab="Search",main="Search distribution")
curve(dnorm(x,mean=mean(log(SII$search+1-min(SII$search))),sd=sd(log(SII$search+1-min(SII$search)))),add=TRUE)

dev.off()
par(mfrow=c(2,2),mar=c(3,2,2,1))
hist(SII$SocialSignal_all,prob=T,xlab="Social Signal",main="Social Signal distribution")
curve(dnorm(x,mean=mean(SII$Review_all),sd=sd(SII$Review_all)),add=TRUE)

hist(log(SII$SocialSignal_all+1-min(SII$SocialSignal_all)),prob=T,xlab="Social Signal",main="Social Signal distribution")
curve(dnorm(x,mean=mean(log(SII$SocialSignal_all+1-min(SII$SocialSignal_all))),sd=sd(log(SII$SocialSignal_all+1-min(SII$SocialSignal_all)))),add=TRUE)

hist(SII$Backlinks_all,prob=T,xlab="Backlinks",main="Backlinks distribution")
curve(dnorm(x,mean=mean(SII$Backlinks_all),sd=sd(SII$Backlinks_all)),add=TRUE)

hist(log(SII$Backlinks_all+1-min(SII$Backlinks_all)),prob=T,xlab="Backlinks",main="Backlinks distribution")
curve(dnorm(x,mean=mean(log(SII$Backlinks_all+1-min(SII$Backlinks_all))),sd=sd(log(SII$Backlinks_all+1-min(SII$Backlinks_all)))),add=TRUE)

# Note : 1.The most attractive feature of a logistic regression model is neither assumes the linearity in the relationship
#            between the covariates and the outcome variable, nor does it require normally distributed variables.
#       2. However continue EDA as usaul for better understanding of behaviour of variables.

#---------------------------------------------------------------------------------------------------------------
#                                      Relationship among variables : Scatterplot
#---------------------------------------------------------------------------------------------------------------

# Reviews
dev.off()
attach(SII)
range(Unit_sales);seq(range(Unit_sales)[1],range(Unit_sales)[2],by = 500)
range(Review_all);seq(range(Review_all)[1],range(Review_all)[2],by = 500)
par(mfrow=c(3,2),mar=c(3,2,2,1))
plot(Review_all,Unit_sales,type = "p",xlab="Review",ylab="Sales",main="Sales vs Review",ylim = c(0,500),xlim=c(0,500))
abline(lm(Unit_sales ~ Review_all, data = SII))

plot(Review_all,Unit_sales,type = "p",xlab="Review",ylab="Sales",main="Sales vs Review",ylim = c(500,1000))
abline(lm(Unit_sales ~ Review_all, data = SII))

plot(Review_all,Unit_sales,type = "p",xlab="Review",ylab="Sales",main="Sales vs Review",ylim = c(1000,1500))
abline(lm(Unit_sales ~ Review_all, data = SII))

plot(Review_all,Unit_sales,type = "p",xlab="Review",ylab="Sales",main="Sales vs Review",ylim = c(1500,2000))
abline(lm(Unit_sales ~ Review_all, data = SII))

plot(Review_all,Unit_sales,type = "p",xlab="Review",ylab="Sales",main="Sales vs Review",ylim = c(2000,2500))
abline(lm(Unit_sales ~ Review_all, data = SII))

plot(Review_all,Unit_sales,type = "p",xlab="Review",ylab="Sales",main="Sales vs Review",ylim = c(2500,3000))
abline(lm(Unit_sales ~ Review_all, data = SII))

# Search
dev.off()
range(Unit_sales);seq(range(Unit_sales)[1],range(Unit_sales)[2],by = 500)
range(search);seq(range(search)[1],range(search)[2],by = 100000)
par(mfrow=c(3,2),mar=c(3,2,2,1))
plot(search,Unit_sales,type = "p",xlab="Search",ylab="Sales",main="Sales vs Search",ylim = c(0,500))
abline(lm(Unit_sales ~ search, data = SII))

plot(search,Unit_sales,type = "p",xlab="Search",ylab="Sales",main="Sales vs Search",ylim = c(500,1000))
abline(lm(Unit_sales ~ search, data = SII))

plot(search,Unit_sales,type = "p",xlab="Search",ylab="Sales",main="Sales vs Search",ylim = c(1000,1500))
abline(lm(Unit_sales ~ search, data = SII))

plot(search,Unit_sales,type = "p",xlab="Search",ylab="Sales",main="Sales vs Search",ylim = c(1500,2000))
abline(lm(Unit_sales ~ search, data = SII))

plot(search,Unit_sales,type = "p",xlab="Search",ylab="Sales",main="Sales vs Search",ylim = c(2000,2500))
abline(lm(Unit_sales ~ search, data = SII))

plot(search,Unit_sales,type = "p",xlab="Search",ylab="Sales",main="Sales vs Search",ylim = c(2500,3000))
abline(lm(Unit_sales ~ search, data = SII))

# Reviews
dev.off()
range(Unit_sales);seq(range(Unit_sales)[1],range(Unit_sales)[2],by = 500)
range(Review_all);seq(range(Review_all)[1],range(Review_all)[2],by = 300)
par(mfrow=c(3,2),mar=c(3,2,2,1))
plot(Review_all,Unit_sales,type = "p",xlab="Reviews",ylab="Sales",main="Sales vs Reviews",ylim = c(0,500))
abline(lm(Unit_sales ~ Review_all, data = SII))

plot(Review_all,Unit_sales,type = "p",xlab="Reviews",ylab="Sales",main="Sales vs Reviews",ylim = c(500,1000))
abline(lm(Unit_sales ~ Review_all, data = SII))

plot(Review_all,Unit_sales,type = "p",xlab="Reviews",ylab="Sales",main="Sales vs Reviews",ylim = c(1000,1500))
abline(lm(Unit_sales ~ Review_all, data = SII))

plot(Review_all,Unit_sales,type = "p",xlab="Reviews",ylab="Sales",main="Sales vs Reviews",ylim = c(1500,2000))
abline(lm(Unit_sales ~ Review_all, data = SII))

plot(Review_all,Unit_sales,type = "p",xlab="Reviews",ylab="Sales",main="Sales vs Reviews",ylim = c(2000,2500))
abline(lm(Unit_sales ~ Review_all, data = SII))

plot(Review_all,Unit_sales,type = "p",xlab="Reviews",ylab="Sales",main="Sales vs Reviews",ylim = c(2500,3000))
abline(lm(Unit_sales ~ Review_all, data = SII))

detach(SII)

# Inference : 1. As Review increases, there seems to be increase in sales though it is not strongly correlated.
#            2. As Search increases, there seems to be increase in sales though it is not strongly correlated but if search limit
#               reduces to limit 100000 to 500000 it seems less correlated.

# --------------------------------------------------------------------------------------------------------------
#                            Relationship b/n Mean & Standard deviation : Chebyshev's Theorem
# --------------------------------------------------------------------------------------------------------------

describe(data.table(SII)[Unit_sales<1000,Unit_sales]) # Sales_units < 1000 ; mean = 77.25 & sd = 140.08
describe(data.table(SII)[Unit_sales>1000,Unit_sales]) # Sales_units > 1000 ; mean = 2315.75 & sd = 695.06

# What proportion of data that have sales units between 100 & 1000
K <- length(seq(from=mean(SII$Unit_sales),to=1000,by=sd(SII$Unit_sales))) # Arithmatic Progression = a+(n-1)d

# Chebyshev's inequality : 1-(1/K^2)
Cheb.In <- paste0("~",ceiling((1-(1/K^2))*100),"%")
Cheb.In

# Interpretation : ~89% of the data have sales units between 100 to 1000. (OR)
# Approximately 89% of observations will lie within 3*standard deviation of the mean.

#---------------------------------------------------------------------------------------------------------------
#                                        Logistic Regression : Baseline
#---------------------------------------------------------------------------------------------------------------

# Data Sampling
set.seed(100)
SII_IND  <- sample(nrow(SII),size=round(((nrow(SII)/100)*70)+1,0))
SII_TRAIN <- SII[SII_IND,]
SII_TEST <- SII[-SII_IND,]

SII_LR <- glm(L_unitsales ~ Review_all+search+SocialSignal_all+Backlinks_all,data=SII_TRAIN,family=binomial(link="logit"))
summary(SII_LR)
par(mfrow=c(2,2),mar=c(3,2,2,1))
plot(SII_LR)

# Stepwise Regression
step(SII_LR,direction='forward',trace = 0)
summary(step(SII_LR,direction='forward',trace = 0))
step(SII_LR,direction='backward',trace = 0)
summary(step(SII_LR,direction='backward',trace=0))
formula(step(SII_LR,direction='backward',trace=0))

# logLikelihood & BIC
library(lmtest)
logLik(SII_LR)
BIC(logLik(step(SII_LR,direction='forward')))
BIC(logLik(step(SII_LR,direction='backward')))

# Wald's test : Overall variance explained
library(aod)
wald.test(Sigma = vcov(SII_LR),b = coef(SII_LR), Terms = 2:length(coef(SII_LR)))

# Prediction & Odds ratio
SII_TEST$Prob <-  predict.glm(SII_LR,SII_TEST,type="response")
SII_TEST$Pred <- ifelse(SII_TEST$Prob>0.5,1,0)
SII_TEST$Odds <- round(SII_TEST$Prob/(1-SII_TEST$Prob),2)
View(exp(cbind(OR = coef(SII_LR), confint(SII_LR))))

# Classification Accuracy/ Confusion Matrix
library(xtable)
library(reshape)
table(Actual=SII_TEST$L_unitsales,Predict=SII_TEST$Pred)
DT <- cbind(melt(table(Actual=SII_TEST$L_unitsales,Predict=SII_TEST$Pred)),
            Decision=c("False Negative","True Negative","False Positive","True Positive"))
DT <- data.table(DT)
setorderv(DT,"Decision",order = -1);DT

Class_Table <- xtable(table(Actual=SII_TEST$L_unitsales,Predict=SII_TEST$Pred))
cbind(Class_Table <- xtable(table(Actual=SII_TEST$L_unitsales,Predict=SII_TEST$Pred)),
      Per_Correct = c((Class_Table[1,1]/nrow(SII_TEST)*100),(Class_Table[2,2]/nrow(SII_TEST)*100)))
sum(cbind(Class_Table <- xtable(table(Actual=SII_TEST$L_unitsales,Predict=SII_TEST$Pred)),
          Per_Correct = c((Class_Table[1,1]/nrow(SII_TEST)*100),(Class_Table[2,2]/nrow(SII_TEST)*100)))[3])

#---------------------------------------------------------------------------------------------------------------
#                                        Identifying Outliers
#           Note :  it is NOT acceptable to drop an observation just because it is an outlier # Check Ref URL
#---------------------------------------------------------------------------------------------------------------

# Standardised Residuals

library(MASS)
SII_SR <- cbind(SII_TRAIN,Std_Res=stdres(SII_LR))
SII_SR <- cbind(SII_SR,Std_Res_Dec = ifelse(SII_SR$Std_Res>3 | SII_SR$Std_Res < -3,"YES","NO"))

# Cook's distance : Cook's distance or Cook's D is a commonly used estimate of the influence of a data point when
#                   performing least squares regression analysis.

infl <- influence(SII_LR, do.coef = FALSE)
SII_SR_CD <- cbind(SII_SR, Cook_D=cooks.distance(SII_LR, infl = influence(SII_LR, do.coef = FALSE),
                res = infl$pear.res,
               dispersion = summary(SII_LR)$dispersion,
               hat = infl$hat))

SII_SR_CD <- subset(SII_SR_CD,Std_Res_Dec=="NO" & Cook_D<1,names(SII_SR_CD))
cor(SII_SR_CD[,c(2,4:7)])

#---------------------------------------------------------------------------------------------------------------
#                                        Revised Logistic Regression
#---------------------------------------------------------------------------------------------------------------

#  SII_SR_CD$Review_all <- SII_SR_CD$Review_all+1-min(SII_SR_CD$Review_all)
# SII_SR_CD$search <- SII_SR_CD$search+1-min(SII_SR_CD$search)
# SII_SR_CD$search <- 1/SII_SR_CD$search
# SII_SR_CD$SocialSignal_all <- SII_SR_CD$SocialSignal_all+1-min(SII_SR_CD$SocialSignal_all)
#  SII_SR_CD$Backlinks_all <- SII_SR_CD$Backlinks_all+1-min(SII_SR_CD$Backlinks_all)

SII_LR_Rev <- glm(L_unitsales ~ Review_all+search+SocialSignal_all+Backlinks_all,
                  data=SII_SR_CD,family=binomial(link="logit"))

# SII_LR_Rev <- glm(L_unitsales ~ sqrt(Review_all)+search+sqrt(SocialSignal_all)*sqrt(Backlinks_all),
#                   data=SII_SR_CD,family=binomial(link="logit"))
summary(SII_LR_Rev)
View(exp(cbind(OR = coef(SII_LR_Rev), confint(SII_LR_Rev))))
par(mfrow=c(2,2),mar=c(3,2,2,1))
plot(SII_LR_Rev)

# Stepwise Regression
step(SII_LR_Rev,direction='forward',trace = 0)
summary(step(SII_LR_Rev,direction='forward',trace = 0))
step(SII_LR_Rev,direction='backward',trace = 0)
summary(step(SII_LR_Rev,direction='backward',trace=0))
formula(step(SII_LR_Rev,direction='backward',trace=0))

# logLikelihood & BIC
library(lmtest)
logLik(SII_LR_Rev)
BIC(logLik(step(SII_LR_Rev,direction='forward')))
BIC(logLik(step(SII_LR_Rev,direction='backward')))

# Wald's test : Overall variance explained
wald.test(Sigma = vcov(SII_LR_Rev),b = coef(SII_LR_Rev), Terms = 2:length(coef(SII_LR_Rev)))

# Prediction & Odds
SII_TEST$Prob <-  predict.glm(SII_LR_Rev,SII_TEST,type="response")
SII_TEST$Pred <- ifelse(SII_TEST$Prob>0.5,1,0)
SII_TEST$Odds <- round(SII_TEST$Prob/(1-SII_TEST$Prob),2)

# Classification Accuracy/ Confusion Matrix
table(Actual=SII_TEST$L_unitsales,Predict=SII_TEST$Pred)
DT <- cbind(melt(table(Actual=SII_TEST$L_unitsales,Predict=SII_TEST$Pred)),
            Decision=c("False Negative","True Negative","False Positive","True Positive"))
DT <- data.table(DT)
setorderv(DT,"Decision",order = -1);DT

Class_Table <- xtable(table(Actual=SII_TEST$L_unitsales,Predict=SII_TEST$Pred))
cbind(Class_Table <- xtable(table(Actual=SII_TEST$L_unitsales,Predict=SII_TEST$Pred)),
      Per_Correct = c((Class_Table[1,1]/nrow(SII_TEST)*100),(Class_Table[2,2]/nrow(SII_TEST)*100)))
sum(cbind(Class_Table <- xtable(table(Actual=SII_TEST$L_unitsales,Predict=SII_TEST$Pred)),
          Per_Correct = c((Class_Table[1,1]/nrow(SII_TEST)*100),(Class_Table[2,2]/nrow(SII_TEST)*100)))[3])

#---------------------------------------------------------------------------------------------------------------
#                                        Receiver Operating Characteristics : ROC
#---------------------------------------------------------------------------------------------------------------

# Prediction & Odds ratio
SII_TEST$Prob <-  predict.glm(SII_LR_Rev,SII_TEST,type="response")
SII_TEST$Pred <- ifelse(SII_TEST$Prob>0.5,1,0)
SII_TEST$Odds <- round(SII_TEST$Prob/(1-SII_TEST$Prob),2)

library(ROCR)
Prediction <- prediction(SII_TEST$Pred,SII_TEST$L_unitsales)
par(mfrow=c(1,2),mar=c(3,2,2,1))
Perf1 <- performance(Prediction,measure="tpr",x.measure="fpr")    # True versus False
plot(Perf1,main="ROC Curve")

Perf2 <- performance(Prediction,measure="sens",x.measure="spec") # True versus True
plot(Perf2,main="Sensitivity & Specificity")

dev.off()
library(pROC)
ROC <- roc(SII_TEST$Pred,as.numeric(SII_TEST$L_unitsales),plot=TRUE)                     # True versus True

# Reference : http://www.graphpad.com/guides/prism/6/statistics/index.htm?sensitivity_and_specificity.htm

# Classification Accuracy/ Confusion Matrix & Odds ratio
table(Actual=SII_TEST$L_unitsales,Predict=SII_TEST$Pred)
Class_Table <- xtable(table(Actual=SII_TEST$L_unitsales,Predict=SII_TEST$Pred))
cbind(Class_Table <- xtable(table(Actual=SII_TEST$L_unitsales,Predict=SII_TEST$Pred)),
      Per_Correct = c((Class_Table[1,1]/nrow(SII_TEST)*100),(Class_Table[2,2]/nrow(SII_TEST)*100)))
sum(cbind(Class_Table <- xtable(table(Actual=SII_TEST$L_unitsales,Predict=SII_TEST$Pred)),
          Per_Correct = c((Class_Table[1,1]/nrow(SII_TEST)*100),(Class_Table[2,2]/nrow(SII_TEST)*100)))[3])
# Conclusion : Since logistic regression with removing Outliers & influential cases has more than 2% more accuracy in classifying cases
#               than logistic regression with all cases, we conclude that "Revised model is better than Baseline model".

#---------------------------------------------------------------------------------------------------------------
#                                           Complete data operation
#---------------------------------------------------------------------------------------------------------------

# Prediction & Odds ratio
SII$Prob <-  predict.glm(SII_LR_Rev,SII,type="response")
SII$Pred_30 <- ifelse(SII$Prob>0.3,1,0)
SII$Pred_50 <- ifelse(SII$Prob>0.5,1,0)
SII$Odds <- round(SII$Prob/(1-SII$Prob),2)
View(exp(cbind(OR = coef(SII_LR_Rev), confint(SII_LR_Rev))))
# Odds Interpretation : Considering an Item is a TOP-Product is 2.40 times more likely than to consider an item is NOT a TOP-Product.
# Odds Ratio : If there is one unit increase in Review then there will be ~0.6% increase in Sales.
#              If there is one unit increase in Search then there will be ~0.000000% increase in Sales.
#              If there is one unit increase in Social Signal then there will be ~0.00000% increase in Sales.
#              If there is one unit increase in Backlinks then there will be ~0.2% increase in Sales.

Prediction <- prediction(SII$Pred_30,SII$L_unitsales)
Prediction <- prediction(SII$Pred_50,SII$L_unitsales)

par(mfrow=c(1,2),mar=c(2,2,2,1))
Perf1 <- performance(Prediction,measure="tpr",x.measure="fpr")    # True versus False
plot(Perf1,main="ROC Curve",xlab = "False Positive(1-Specificity)",ylab="True Positive")

Perf2 <- performance(Prediction,measure="sens",x.measure="spec") # True versus True
plot(Perf2,main="Sensitivity & Specificity")

dev.off()
par(mfrow=c(1,2),mar=c(2,2,2,1))
ROC_50 <- roc(SII$Pred_50,ifelse(as.numeric(SII$L_unitsales)==2,1,0),plot=TRUE)                     # True versus False
ROC_30 <- roc(SII$Pred_30,ifelse(as.numeric(SII$L_unitsales)==2,1,0),plot=TRUE)                     # True versus False
print(ROC_50) # AUC = 0.74(fair classifier)
print(ROC_30) # AUC = 0.76(fair classifier)

# Decision : As with 50%(before ROC) suggest to go with 30% cut-off(by ROC) which has 29-products considered to be better than
#            30%(before ROC) suggest to go with 58% cut-off(by ROC) which as 15-products.
# Reference : http://gim.unmc.edu/dxtests/roc3.htm

# 50%
table(Actual=SII$L_unitsales,Predict=SII$Pred_50)
Class_Table <- xtable(table(Actual=SII$L_unitsales,Predict=SII$Pred_50))
cbind(Class_Table <- xtable(table(Actual=SII$L_unitsales,Predict=SII$Pred_50)),
      Per_Correct = c((Class_Table[1,1]/nrow(SII)*100),(Class_Table[2,2]/nrow(SII)*100)))
sum(cbind(Class_Table <- xtable(table(Actual=SII$L_unitsales,Predict=SII$Pred_50)),
          Per_Correct = c((Class_Table[1,1]/nrow(SII)*100),(Class_Table[2,2]/nrow(SII)*100)))[3])

#---------------------------------------------------------------------------------------------------------------
#                                        Top Selling Products
#---------------------------------------------------------------------------------------------------------------
# save.image("~/Data Analysis/Data Analysis/Logit_nnet_v1.RData")

SII$Top_Prod_30 <- ifelse(SII$L_unitsales==1 & SII$Pred_30==1,"Yes","No")
SII$Top_Prod_50 <- ifelse(SII$L_unitsales==1 & SII$Pred_50==1,"Yes","No")
SII <- SII[with(SII,order(Top_Prod_30,Top_Prod_50,decreasing = TRUE)),]

write.csv(SII[,c(1,12:13)],"C:/Yashwanth/Data Analysis/Logistic Regression/SII Top Selling products.csv",row.names = FALSE)

rm(list=setdiff(ls(),c("SII","SII_SR_CD")))

# Reference : http://scialert.net/fulltext/?doi=jas.2011.26.35&org=11
#             http://connectmv.com/tutorials/r-tutorial/investigating-outliers-discrepancies-and-other-influential-points/
#             http://stats.stackexchange.com/questions/86024/accuracy-rate-in-naive-bayes-classification
# AIC : Akaike’s Information Criterion is a measure of how well a model# pproximates reality. Its most common use is to
#          compare models (based on the same dataset) that differ in their number of parameters. Models with more parameters
#          will generally have higher likelihood, and AIC provides a means to incorporate principles of parsimony in model comparison.

# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
# ----------------- END ----------------- Logistic Regression ----------------- END ------------------------------
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------

# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
#                                           Nueral network
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------

rm(list=ls())
SII <- read.table("C:/Yashwanth/Data Analysis/Logistic Regression/SII/Output_all.csv",
                  header = TRUE, sep = ",", quote = "\"", dec = "." ,fill=TRUE, comment.char="", as.is=TRUE)
SII$L_unitsales <- as.numeric(SII$L_unitsales)
# rm(list=setdiff(ls(),"SII"))
library(nnet)

# Data Sampling
set.seed(100)
SII_IND  <- sample(nrow(SII),size=round(((nrow(SII)/100)*70)+1,0))
SII_TRAIN <- SII[SII_IND,]
SII_TEST <- SII[-SII_IND,]

# Default S3 method
set.seed(100)
SII_Nnet_S3 <- nnet(SII_TRAIN[,-c(1,2)],class.ind(SII_TRAIN[,3]),size = 10, entropy =  TRUE, decay = 0.0005)
summary(SII_Nnet_S3)

# Formula method
# SII_Nnet <- nnet(L_unitsales ~ Review_all+search+SocialSignal_all+Backlinks_all, data=SII_TRAIN, size = 3)
# summary(nnet(L_unitsales ~Review_all+search+SocialSignal_all+Backlinks_all, data=SII_TRAIN, size = 3))

# Package "neural network"
library(neuralnet)
SII_NN <- neuralnet(L_unitsales ~ Review_all+search+SocialSignal_all+Backlinks_all, data = SII_TRAIN, hidden = 10)
dev.off()
plot(neuralnet(L_unitsales ~ Review_all+search+SocialSignal_all+Backlinks_all, data = SII_TRAIN, hidden = 10)) # plot.nn

# --------------------------------------------------------------------------------------------------------------
#                                           Predictions
# --------------------------------------------------------------------------------------------------------------

SII_TEST$Predict <- predict(SII_Nnet_S3,SII_TEST,type = "class")
SII$Predict <- predict(SII_Nnet_S3,SII[,-c(1,2)],type = "class")

table(Actual=SII$L_unitsales,Predict=SII$Predict)
Class_Table <- xtable(table(Actual=SII$L_unitsales,Predict=SII$Predict))
cbind(Class_Table <- xtable(table(Actual=SII$L_unitsales,Predict=SII$Predict)),
      Per_Correct = c((Class_Table[1,1]/nrow(SII)*100),(Class_Table[2,2]/nrow(SII)*100)))
sum(cbind(Class_Table <- xtable(table(Actual=SII$L_unitsales,Predict=SII$Predict)),
          Per_Correct = c((Class_Table[1,1]/nrow(SII)*100),(Class_Table[2,2]/nrow(SII)*100)))[3])


# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
# ----------------- END ------------------- Nueral Network ------------------ END ------------------------------
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
