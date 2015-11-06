---
title: "Shoppers Interest Index"
output: html_document
---

List of attributes included in the Analysis & their data types

```{r,echo=FALSE}
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
#                                          Logistic Ression
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------

#---------------------------------------------------------------------------------------------------------------
#                                         Data and data types
#---------------------------------------------------------------------------------------------------------------

rm(list=ls())
SII <- read.table("C:/Yashwanth/Data Analysis/Logistic Regression/SII/Output_all.csv",
                      header = TRUE, sep = ",", quote = "\"", dec = "." ,fill=TRUE, comment.char="", as.is=TRUE)
str(SII)
SII$L_unitsales <- as.factor(SII$L_unitsales)

```

Summary or Descriptives of all attributes


```{r,echo=FALSE}
#---------------------------------------------------------------------------------------------------------------
#                                       Descriptive Statistics
#---------------------------------------------------------------------------------------------------------------

library(psych)
describe(SII[,sapply(SII,is.numeric)])

#summary(SII)
#summary(SII[,sapply(SII,is.numeric)])
cor(SII[,sapply(SII,is.numeric)])
```


```{r,echo=FALSE}
#---------------------------------------------------------------------------------------------------------------
#                                       Distribution of Variables : Histogram
#---------------------------------------------------------------------------------------------------------------

# Note :The most attractive feature of a logistic regression model is neither assumes the linearity in the relationship
#       between the covariates and the outcome variable, nor does it require normally distributed variables.

# table(SII$L_unitsales)

# --------------------------------------------------------------------------------------------------------------
#                            Relationship b/n Mean & Standard deviation : Chebyshev's Theorem
# --------------------------------------------------------------------------------------------------------------

describe(subset(SII,Unit_sales<1000,names(SII))[2]) # Sales_units < 1000 ; mean = 77.25 & sd = 140.08
describe(subset(SII,Unit_sales>1000,names(SII))[2]) # Sales_units > 1000 ; mean = 2315.75 & sd = 695.06

# What proportion of data that have sales units between 100 & 500
K <-  length(seq(from=describe(subset(SII,Unit_sales<1000,names(SII))[2])$mean,to=500,
         by=(describe(subset(SII,Unit_sales<1000,names(SII))[2])$sd))) # Arithmatic Progression = a+(n-1)d

# Chebyshev's inequality : 1-(1/K^2)
Cheb.In <- paste0((1-(1/K^2))*100,"%")
Cheb.In
```
Interpretation : ~94% of the data have sales units between 100 to 500. (OR)
Approximately 94% of observations will lie within 4*standard deviation of the mean.


Logistic Regression with all cases : Baseline Model


```{r,echo=FALSE}

#---------------------------------------------------------------------------------------------------------------
#                                        Logistic Regression One
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
```

Stepwise Logistic Regression

```{r,echo=FALSE}
step(SII_LR,direction='forward',trace = 0)
summary(step(SII_LR,direction='forward',trace = 0))
step(SII_LR,direction='backward',trace = 0)
summary(step(SII_LR,direction='backward',trace=0))
formula(step(SII_LR,direction='backward',trace=0))
```

Loglikelihood Value

```{r,echo=FALSE}
# logLikelihood & BIC
library(lmtest)
logLik(SII_LR)
BIC(logLik(step(SII_LR,direction='forward')))
BIC(logLik(step(SII_LR,direction='backward')))
```

Overall Variance Explained & Significance

```{r,echo=FALSE}
# Wald's test : Overall variance explained
library(aod)
wald.test(Sigma = vcov(SII_LR),b = coef(SII_LR), Terms = 2:length(coef(SII_LR)))
```

Prediction & Odds Ratio

```{r,echo=FALSE}
# Prediction & Odds ratio
SII_TEST$Prob <-  predict.glm(SII_LR,SII_TEST,type="response")
SII_TEST$Pred <- ifelse(SII_TEST$Prob>0.5,1,0)
SII_TEST$Odds <- round(SII_TEST$Prob/(1-SII_TEST$Prob),2)
View(exp(cbind(OR = coef(SII_LR), confint(SII_LR))))
```

Confusion matrix

```{r,echo=FALSE}
# Classification Accuracy/ Confusion Matrix
library(xtable)
table(Actual=SII_TEST$L_unitsales,Predict=SII_TEST$Pred)
Class_Table <- xtable(table(Actual=SII_TEST$L_unitsales,Predict=SII_TEST$Pred))
cbind(Class_Table <- xtable(table(Actual=SII_TEST$L_unitsales,Predict=SII_TEST$Pred)),
      Per_Correct = c((Class_Table[1,1]/nrow(SII_TEST)*100),(Class_Table[2,2]/nrow(SII_TEST)*100)))
sum(cbind(Class_Table <- xtable(table(Actual=SII_TEST$L_unitsales,Predict=SII_TEST$Pred)),
          Per_Correct = c((Class_Table[1,1]/nrow(SII_TEST)*100),(Class_Table[2,2]/nrow(SII_TEST)*100)))[3])
```

Identifying & handling Outliers

```{r,echo=FALSE}

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
```

Revised model : Removing oultliers based on Standardised residuals & Influential cases(Cook's distance)

```{r,echo=FALSE}
SII_LR_Rev <- glm(L_unitsales ~ Review_all+search+SocialSignal_all+Backlinks_all,
                  data=SII_SR_CD,family=binomial(link="logit"))

# SII_LR_Rev <- glm(L_unitsales ~ sqrt(Review_all)+search+sqrt(SocialSignal_all)*sqrt(Backlinks_all),
#                   data=SII_SR_CD,family=binomial(link="logit"))
summary(SII_LR_Rev)
View(exp(cbind(OR = coef(SII_LR_Rev), confint(SII_LR_Rev))))
par(mfrow=c(2,2),mar=c(3,2,2,1))
plot(SII_LR_Rev)
```

Loglikelihood & BIC

```{r,echo=FALSE}
# logLikelihood & BIC
library(lmtest)
logLik(SII_LR_Rev)
BIC(logLik(step(SII_LR_Rev,direction='forward')))
BIC(logLik(step(SII_LR_Rev,direction='backward')))
```

Overall Variance Explained : Revised Model

```{r,echo=FALSE}
# Wald's test : Overall variance explained
wald.test(Sigma = vcov(SII_LR_Rev),b = coef(SII_LR_Rev), Terms = 2:length(coef(SII_LR_Rev)))
```

Revised Model Prediction & Odds

```{r,echo=FALSE}
# Prediction & Odds ratio
SII_TEST$Prob <-  predict.glm(SII_LR_Rev,SII_TEST,type="response")
SII_TEST$Pred <- ifelse(SII_TEST$Prob>0.5,1,0)
SII_TEST$Odds <- round(SII_TEST$Prob/(1-SII_TEST$Prob),2)
```

Revised Model confusion matrix

```{r,echo=FALSE}
# Classification Accuracy/ Confusion Matrix & Odds ratio
table(Actual=SII_TEST$L_unitsales,Predict=SII_TEST$Pred)
Class_Table <- xtable(table(Actual=SII_TEST$L_unitsales,Predict=SII_TEST$Pred))
cbind(Class_Table <- xtable(table(Actual=SII_TEST$L_unitsales,Predict=SII_TEST$Pred)),
      Per_Correct = c((Class_Table[1,1]/nrow(SII_TEST)*100),(Class_Table[2,2]/nrow(SII_TEST)*100)))
sum(cbind(Class_Table <- xtable(table(Actual=SII_TEST$L_unitsales,Predict=SII_TEST$Pred)),
          Per_Correct = c((Class_Table[1,1]/nrow(SII_TEST)*100),(Class_Table[2,2]/nrow(SII_TEST)*100)))[3])
```
 Conclusion : Since logistic regression with removing Outliers & influential cases has more than 2% more accuracy in classifying cases than logistic regression with all cases, we conclude that "Revised model is better than Baseline model".


Prediction & Odds for complete data.
  
```{r,echo=FALSE}
#---------------------------------------------------------------------------------------------------------------
#                                           Complete data operation
#---------------------------------------------------------------------------------------------------------------
# Prediction, Odds & Odds ratio
SII$Prob <-  predict.glm(SII_LR_Rev,SII,type="response")
SII$Pred_30 <- ifelse(SII$Prob>0.3,1,0)
SII$Pred_50 <- ifelse(SII$Prob>0.5,1,0)
View(exp(cbind(OR = coef(SII_LR_Rev), confint(SII_LR_Rev))))
SII$Odds <- round(SII$Prob/(1-SII$Prob),2)
```

Classification Accuracy/Confusion matrix for complete data with 30% & 50% cut-off

```{r,echo=FALSE}
# Classification Accuracy/ Confusion Matrix : 30%
table(Actual=SII$L_unitsales,Predict=SII$Pred_30)
Class_Table <- xtable(table(Actual=SII$L_unitsales,Predict=SII$Pred_30))
cbind(Class_Table <- xtable(table(Actual=SII$L_unitsales,Predict=SII$Pred_30)),
      Per_Correct = c((Class_Table[1,1]/nrow(SII)*100),(Class_Table[2,2]/nrow(SII)*100)))
sum(cbind(Class_Table <- xtable(table(Actual=SII$L_unitsales,Predict=SII$Pred_30)),
          Per_Correct = c((Class_Table[1,1]/nrow(SII)*100),(Class_Table[2,2]/nrow(SII)*100)))[3])

# 50%
table(Actual=SII$L_unitsales,Predict=SII$Pred_50)
Class_Table <- xtable(table(Actual=SII$L_unitsales,Predict=SII$Pred_50))
cbind(Class_Table <- xtable(table(Actual=SII$L_unitsales,Predict=SII$Pred_50)),
      Per_Correct = c((Class_Table[1,1]/nrow(SII)*100),(Class_Table[2,2]/nrow(SII)*100)))
sum(cbind(Class_Table <- xtable(table(Actual=SII$L_unitsales,Predict=SII$Pred_50)),
          Per_Correct = c((Class_Table[1,1]/nrow(SII)*100),(Class_Table[2,2]/nrow(SII)*100)))[3])
``` 

Classification Accuracy is 29-products out of 50 with Probability cut-off 30% & 15-products with Probability cut-off 50%.

```{r,echo=FALSE}
#---------------------------------------------------------------------------------------------------------------
#                                        Top Selling Products
#---------------------------------------------------------------------------------------------------------------
# save.image("~/Data Analysis/Data Analysis/Logit_nnet_v1.RData")

SII$Top_Prod_30 <- ifelse(SII$L_unitsales==1 & SII$Pred_30==1,"Yes","No")
SII$Top_Prod_50 <- ifelse(SII$L_unitsales==1 & SII$Pred_50==1,"Yes","No")
SII <- SII[with(SII,order(Top_Prod_30,Top_Prod_50,decreasing = TRUE)),]

print(SII[1:50,c(1,12:13)])

# Reference : http://scialert.net/fulltext/?doi=jas.2011.26.35&org=11
#             http://connectmv.com/tutorials/r-tutorial/investigating-outliers-discrepancies-and-other-influential-points/

# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
# ----------------- END ----------------- Logistic Regression ----------------- END ------------------------------
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------

```



