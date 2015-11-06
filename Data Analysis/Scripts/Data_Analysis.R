# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
#                                        || DATA ANALYSIS ||
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------

# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
#                                        Linear Regression
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------

# --------------------------------------------------------------------------------------------------------------
#                                      Order data from Tableau
# --------------------------------------------------------------------------------------------------------------

rm(list=ls())
TAB.ORDER_DUMMY <- read.table("C:/Yashwanth/Tableau Training/Orders.csv",
           header = TRUE, sep = ",", quote = "\"", dec = "." ,fill=TRUE, comment.char="", as.is=TRUE)
TAB.ORDER_DUMMY <- TAB.ORDER_DUMMY[,c("Row_ID","Order_Priority","Order_Date","Ship_Date","Customer_ID","Customer_Name",
                                    "Ship_Mode","Customer_Segment","Product_Category","Product_Sub_Category","Product_Container",
                                    "Product_Name","Region","State_or_Province","City","Postal_Code","Unit_Price","Shipping_Cost",
                                    "Discount","Product_Base_Margin","Profit","Sale_Units","Sales_Dollar")]

TAB.ORDER <- read.table("C:/Yashwanth/Tableau Training/Orders_new.csv",
                        header = TRUE, sep = ",", quote = "\"", dec = "." ,fill=TRUE, comment.char="", as.is=TRUE)
TAB.ORDER <- TAB.ORDER[,c(1:3,21,4,8,12:19,10,11,7,20,9,5,6)]

TAB.ORDER$Sales_Dollar <- as.numeric(TAB.ORDER$Sales_Dollar)
TAB.ORDER$Profit <- as.numeric(TAB.ORDER$Profit)

TAB.ORDER <- data.frame(with(TAB.ORDER,TAB.ORDER[order(Order_Date),]),row.names=NULL)
#TAB.ORDER$COGS <- round((TAB.ORDER[,21]-(TAB.ORDER[,21]*TAB.ORDER[,18])),2)
TAB.ORDER$Discount_price <- TAB.ORDER[,15]-TAB.ORDER[,22]
TAB.ORDER$Selling_Price_FP <- round(((TAB.ORDER[,21]-TAB.ORDER[,16])/TAB.ORDER[,20]),2)

TAB.ORDER$Unit_Margin <- round((TAB.ORDER[,18]*(TAB.ORDER[,22])),2)
TAB.ORDER$Cost_per_unit <- round((TAB.ORDER[,22]-TAB.ORDER[,24]),2)
TAB.ORDER$Cost_total <- round((TAB.ORDER[,25]*TAB.ORDER[,20]),2)


#---------------------------------------------------------------------------------------------------------------
#                                       Descriptive Statistics
#---------------------------------------------------------------------------------------------------------------

summary(TAB.ORDER[sapply(TAB.ORDER,is.numeric)]) # Select only numeric columns

# --------------------------------------------------------------------------------------------------------------
#                            Relationship b/n Mean & Standard deviation : Chebyshev's Theorem
# --------------------------------------------------------------------------------------------------------------

library(psych)
describe(TAB.ORDER$Sales_Units); # mean = 25.57 & sd = 14.48
# What proportion of data that have sales units between 0 & 40
K <-  1.5    # (30-mean(TAB.ORDER$Sales_Units))/sd(TAB.ORDER$Sales_Units)

# Chebyshev's inequality : 1-(1/K^2)
Cheb.In <- paste0((1-(1/K^2))*100,"%")

# Interpretation : ~56% of the data have sales units between 0 to 40. (OR)
# Approximately 56% of observations will lie within 1.5*standard deviation of the mean.

#---------------------------------------------------------------------------------------------------------------
#                                       Distribution of Variables : Histogram
#---------------------------------------------------------------------------------------------------------------

hist(log(TAB.ORDER$Sales_Dollar),prob=T,xlab="Sales_Dollar",main="Sales $ distribution")
curve(dnorm(x,mean=mean(log(TAB.ORDER$Sales)),sd=sd(log(TAB.ORDER$Sales))),add=TRUE)

hist(log(TAB.ORDER$Sale_Units),prob=T,xlab="Sale_Units",main="Sale Units distribution")
curve(dnorm(x,mean=mean(log(TAB.ORDER$Sales)),sd=sd(log(TAB.ORDER$Sales))),add=TRUE)

hist(TAB.ORDER$Discount,prob=T,xlab="Discount",main="Discount distribution")
curve(dnorm(x,mean=mean(TAB.ORDER$Discount),sd=sd(TAB.ORDER$Discount)),add=TRUE)

hist(log(TAB.ORDER$Unit_Price),prob=T,xlab="Unit_Price",main="Unit Price distribution")
curve(dnorm(x,mean=mean(log(TAB.ORDER$Unit_Price)),sd=sd(log(TAB.ORDER$Unit_Price))),add=TRUE)

hist(log(TAB.ORDER$Shipping_Cost),prob=T,xlab="Shipping_Cost",main="Shipping Cost distribution")
curve(dnorm(x,mean=mean(log(TAB.ORDER$Shipping_Cost)),sd=sd(log(TAB.ORDER$Shipping_Cost))),add=TRUE)

hist(TAB.ORDER$Product_Base_Margin,prob=T,xlab="Product_Base_Margin",main="Base Margin distribution")
curve(dnorm(x,mean=mean(TAB.ORDER$Product_Base_Margin,na.rm=T),sd=sd(TAB.ORDER$Product_Base_Margin,na.rm=T)),add=TRUE)

hist(TAB.ORDER$Profit,prob=T,xlab="Profit",main="Profit distribution")
curve(dnorm(x,mean=mean(TAB.ORDER$Profit),sd=sd(TAB.ORDER$Profit)),add=TRUE)

#---------------------------------------------------------------------------------------------------------------
#                                        Finding & Removing Outliers
#---------------------------------------------------------------------------------------------------------------
#           Note :  it is NOT acceptable to drop an observation just because it is an outlier # Check Ref URL
#---------------------------------------------------------------------------------------------------------------

# Identifying Outliers with Zeros
dev.off()
par(mfrow=c(2,2),mar=c(3,2,2,1))
boxplot(SII$Review_all,main="Review") # Right Skewed
boxplot(SII$search,main="Search") # Right Skewed
boxplot(SII$SocialSignal_all,main="Social Signal") # Right Skewed
boxplot(SII$Backlinks_all,main="Backlinks") # Right Skewed

Quantiles_WZ <- data.frame(cbind(Review=quantile(SII$Review_all),Search=quantile(SII$search),
                                 Social_Sig=quantile(SII$SocialSignal_all),Backlinks=quantile(SII$Backlinks_all)))
Quantiles_WZ <- rbind(Quantiles_WZ,IQR=c(IQR(SII$Review_all),IQR(SII$search),IQR(SII$SocialSignal_all),IQR(SII$Backlinks_all)))
Names <- row.names((Quantiles_WZ))
Quantiles_WZ <- data.frame(t(Quantiles_WZ))
names(Quantiles_WZ) <- Names

# Identifying Outliers without Zeros
dev.off()
par(mfrow=c(2,2),mar=c(3,2,2,1))
boxplot(SII[SII$Review_all>0,2],main="Review") # Right Skewed
boxplot(SII[SII$search>0,3],main="Search") # Right Skewed
boxplot(SII[SII$SocialSignal_all>0,4],main="Social Signal") # Right Skewed
boxplot(SII[SII$Backlinks_all>0,5],main="Backlinks") # Right Skewed

Quantiles_WOZ <- data.frame(cbind(Review=quantile(SII[SII$Review_all>0,2]),Search=quantile(SII[SII$search>0,3]),
                                  Social_Sig=quantile(SII[SII$SocialSignal_all>0,4]),Backlinks=quantile(SII[SII$Backlinks_all>0,5])))
Quantiles_WOZ <- rbind(Quantiles_WOZ,IQR=c(IQR(SII[SII$Review_all>0,2]),IQR(SII[SII$search>0,3]),IQR(SII[SII$SocialSignal_all>0,4]),
                                           IQR(SII[SII$Backlinks_all>0,5])))
Quantiles_WOZ <- data.frame(t(Quantiles_WOZ))
names(Quantiles_WOZ) <- Names
rm(Names)

# Inner fences
Quantiles_WOZ <- cbind(Quantiles_WOZ,rbind(c(quantile(SII[SII$Review_all>0,2])[2]-1.5*IQR(SII[SII$Review_all>0,2]),
                                             quantile(SII[SII$Review_all>0,2])[4]+1.5*IQR(SII[SII$Review_all>0,2])),
                                           c(quantile(SII[SII$search>0,3])[2]-1.5*IQR(SII[SII$search>0,3]),
                                             quantile(SII[SII$search>0,3])[4]+1.5*IQR(SII[SII$search>0,3])),
                                           c(quantile(SII[SII$SocialSignal_all>0,4])[2]-1.5*IQR(SII[SII$SocialSignal_all>0,4]),
                                             quantile(SII[SII$SocialSignal_all>0,4])[4]+1.5*IQR(SII[SII$SocialSignal_all>0,4])),
                                           c(quantile(SII[SII$Backlinks_all>0,5])[2]-1.5*IQR(SII[SII$Backlinks_all>0,5]),
                                             quantile(SII[SII$Backlinks_all>0,5])[4]+1.5*IQR(SII[SII$Backlinks_all>0,5]))))
colnames(Quantiles_WOZ)[7:8] <- as.character(c("Inner FenceQ1","Inner FenceQ3"))

# Outer fences
Quantiles_WOZ <- cbind(Quantiles_WOZ,rbind(c(quantile(SII[SII$Review_all>0,2])[2]-3*IQR(SII[SII$Review_all>0,2]),
                                             quantile(SII[SII$Review_all>0,2])[4]+3*IQR(SII[SII$Review_all>0,2])),
                                           c(quantile(SII[SII$search>0,3])[2]-3*IQR(SII[SII$search>0,3]),
                                             quantile(SII[SII$search>0,3])[4]+3*IQR(SII[SII$search>0,3])),
                                           c(quantile(SII[SII$SocialSignal_all>0,4])[2]-3*IQR(SII[SII$SocialSignal_all>0,4]),
                                             quantile(SII[SII$SocialSignal_all>0,4])[4]+3*IQR(SII[SII$SocialSignal_all>0,4])),
                                           c(quantile(SII[SII$Backlinks_all>0,5])[2]-3*IQR(SII[SII$Backlinks_all>0,5]),
                                             quantile(SII[SII$Backlinks_all>0,5])[4]+3*IQR(SII[SII$Backlinks_all>0,5]))))
colnames(Quantiles_WOZ)[9:10] <- c("Outer FenceQ1","Outer FenceQ3")

# Reference : http://www.theanalysisfactor.com/outliers-to-drop-or-not-to-drop/
#             http://www.purplemath.com/modules/boxwhisk3.htm
# Conclusion : As every observations are bound to occur in this scenario(in real), it may not be the ideal way to drop outliers &
#              perhaps use Transformation.

# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
# ----------------- END ------------------ Linear Regression ----------------- END -----------------------------
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------

# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
#                                             ANOVA , ANACOVA & MANOVA
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------

# https://explorable.com/anova
# http://www.physics.csbsju.edu/stats/anova.html

library(help=datasets)
data("airquality");airquality

anova(lm(Temp ~ Ozone+Solar.R+Wind, data = airquality))

# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
# ------------- END -------------------- ANOVA , ANACOVA & MANOVA ----------------- END ------------------------
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------


# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
#                                         Logistic Regression
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------

#---------------------------------------------------------------------------------------------------------------
#                                         Data and data types
#---------------------------------------------------------------------------------------------------------------

rm(list=ls())
Results <- read.csv("http://www.ats.ucla.edu/stat/data/binary.csv")

data(Results);Results
str(Results)
Results$rank <- as.factor(Results$rank)
Results$admit <- as.factor(Results$admit)

#---------------------------------------------------------------------------------------------------------------
#                                       Descriptive Statistics
#---------------------------------------------------------------------------------------------------------------

summary(Results[,sapply(Results,is.numeric)])

#---------------------------------------------------------------------------------------------------------------
#                                       Distribution of Variables : Histogram
#---------------------------------------------------------------------------------------------------------------

par(mfrow=c(2,1),mar=c(3,2,2,1))
hist(Results$gre,prob=T,xlab="gre",main="gre distribution")
curve(dnorm(x,mean=mean(Results$gre),sd=sd(Results$gre)),add=TRUE)
hist(log(Results$gre),prob=T,xlab="log(gre)",main="log(gre) distribution")
curve(dnorm(x,mean=mean(log(Results$gre)),sd=sd(log(Results$gre))),add=TRUE)

par(mfrow=c(2,1),mar=c(3,2,2,1))
hist(Results$gpa,prob=T,xlab="gpa",main="gpa distribution")
curve(dnorm(x,mean=mean(Results$gpa),sd=sd(Results$gpa)),add=TRUE)
hist(log(Results$gpa),prob=T,xlab="log(gpa)",main="log(gpa) distribution")
curve(dnorm(x,mean=mean(log(Results$gpa)),sd=sd(log(Results$gpa))),add=TRUE)

table(Results$admit);table(Results$rank)'table(Results$admit,Results$rank)'

#---------------------------------------------------------------------------------------------------------------
#                                        BootStrapping/Data Sampling
#---------------------------------------------------------------------------------------------------------------

# Set up the non-parametric bootstrap
Results$admit <- ifelse(Results$admit==1,0,1)
library(boot)
logit.bootstrap <- function(data, indices) {

  d <- data[indices, ]
  fit <- glm(admit ~ gre + gpa + rank, data = d, family = "binomial")

  return(coef(fit))
}

set.seed(12345) # seed for the RNG to ensure that you get exactly the same results as here

logit.boot <- boot(data=Results, statistic=logit.bootstrap, R=10000) # 10'000 samples


# Data Sampling  ##
set.seed(400)
Results_IND <- sample(nrow(Results),size=round(((nrow(Results)/100)*70)+1,0))

Results_TRAIN <- Results[Results_IND,]

Results_TEST <- Results[-Results_IND,]

#---------------------------------------------------------------------------------------------------------------
#                                         Model building
#---------------------------------------------------------------------------------------------------------------

My_Logit <- glm(admit ~ ., data=Results_TRAIN,family=binomial)
summary(My_Logit)
#Interpretation : For one unit change in gre, log odds of admit will increases by 0.0019
#                 For one unit change in gpa, log odds of admit will increases by 0.984439
#                 For rank2, log odds of admit will increases by -0.831731
#                 For rank3, log odds of admit will increases by -1.323668
#                 For rank4, log odds of admit will increases by -1.756235

#---------------------------------------------------------------------------------------------------------------
#                                         Odds ratio and CI
#---------------------------------------------------------------------------------------------------------------

exp(cbind(OR = coef(My_Logit), confint(My_Logit)))

#Interpretation :
# For one unit increase in gre, odds of admition to graduate school will increases by 1.00196667/0.001%
# For one unit increase in gpa, odds of admition to gpa school will increases by 2.67630890/167%
# For rank2, odds of admition to rank2 school will increases by 0.43529531/43%
# For rank3, odds of admition to rank3 school will increases by 0.26615726/27%
# For rank4, odds of admition to rank4 school will increases by 0.17269375/17%

#---------------------------------------------------------------------------------------------------------------
#                                         Predictions
#---------------------------------------------------------------------------------------------------------------

predict(My_Logit,newdata=Results_TEST,type="response")
Final <- data.frame(Results_TEST,actual=Results_TEST$admit,
            prob=round(predict(My_Logit,newdata=Results_TEST,type="response"),2),row.names=NULL)
Final$OddsRatio <- round(Final$prob/(1-Final$prob),2)
Final$Odds <- round(ifelse(Final$OddsRatio<=1,Final$OddsRatio*100,(Final$OddsRatio*100)-100),2)
#predict(My_Logit,newdata=Results_TEST,type="terms")
Final$Predict <- ifelse(Final$prob>=0.5,1,0)

#---------------------------------------------------------------------------------------------------------------
#                                         Confusion Matrix
#---------------------------------------------------------------------------------------------------------------

table(Final$admit)
table(Actual=Final$admit,Pred=Final$Predict)
# library(xtable)
# xtable(table(Actual=Final$admit,Pred=as.factor(Final$Predict)))

#---------------------------------------------------------------------------------------------------------------
#                                         ROC(Receiver Operating Characteristic) Curve
#---------------------------------------------------------------------------------------------------------------

dev.off()
library(pROC)
ROC <- roc(Final$admit,Final$Predict,plot=TRUE)                     # True versus True
# matrix(c(TP=ROC$sensitivities,TN=ROC$specificities,
#          FP=1-ROC$sensitivities,FN=1-ROC$specificities),nrow=3,ncol=4)

library(ROCR)
Prediction <- prediction(Final$Predict,Final$admit)
Perf1 <- performance(Prediction,measure="tpr",x.measure="fpr")    # True versus False
plot(Perf1,main="ROC Curve")

Perf2 <- performance(Prediction,measure="sens",x.measure="spec") # True versus True
plot(Perf2,main="Sensitivity & Specificity")

#  Bayes error rate is the lowest possible error rate for a given class of classifier.
#  The Bayes error rate of the dataset classifier is the probability of the classifier to incorrectly classify an instance.

# Reference : http://en.wikipedia.org/wiki/Receiver_operating_characteristic
#            https://en.wikipedia.org/wiki/Sensitivity_and_specificity
#            http://en.wikipedia.org/wiki/Bayes_error_rate
#            http://www.r-bloggers.com/illustrated-guide-to-roc-and-auc/
#            https://onlinecourses.science.psu.edu/stat504/node/150
#           http://stats.stackexchange.com/questions/3386/how-to-deal-with-non-binary-categorical-variables-in-logistic-regression-spss

# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
# ----------------- END ----------------- Logistic Regression ----------------- END ------------------------------
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------

# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
#                                       Learning SQL
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------

# --------------------------------------------------------------------------------------------------------------
#                                       Subset data
# --------------------------------------------------------------------------------------------------------------

sqldf('select * from CO2 where treatment=="nonchilled" and Plant=="Qn1"')
sqldf('select * from CO2 where treatment=="nonchilled"')

# --------------------------------------------------------------------------------------------------------------
#                                       Renaming variables
# --------------------------------------------------------------------------------------------------------------

Plant_new <- sqldf('select Plant from CO2')
CO2_New <- sqldf('ALTER TABLE CO2 ADD Plant_new nchar(30)')

# --------------------------------------------------------------------------------------------------------------
#                                       Merge two tables
# --------------------------------------------------------------------------------------------------------------

mtcars_Sub <- data.frame(mtcars[,2:5],row.names=NULL)
CO2_Merge <- sqldf('select * from CO2 LEFT JOIN mtcars_Sub on CO2.Plant=mtcars_Sub.cyl')

# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
# ----------------- END ----------------- Learning SQL ----------------- END ------------------------------
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------

# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
#                                       Chi-Sqaure Test for Independence
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------

library(MASS)
data(survey);survey
Survey_CT <- table(survey$Smoke,survey$Exer)

# --------------------------------------------------------------------------------------------------------------
#   Hypothesis H_0 : Students smoking habit is Independent of their Exercise level
#   Hypothesis H_1 : Students smoking habit is not Independent of their Exercise level
# --------------------------------------------------------------------------------------------------------------

# --------------------------------------------------------------------------------------------------------------
#                                 Solution : Chi-Squared test function
# --------------------------------------------------------------------------------------------------------------

chisq.test(Survey_CT)

# Conclusion/Interpretation : Since P-value is greater than 0.05, we fail to reject NH. We conclude
#   that, students smoking habit is not Independent of their Exercise level

# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
# ----------------- END -------------- Chi-Sqaure Test for Independence -------------- END ---------------------
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------

# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
#                                                 Market Segmentation
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
# Market segmentation is a decomposition process where we reverse engineer and break apart the whole into its constituent components.

library(bayesm)
data(Scotch)



# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
# ----------------- END ----------------------- Market Segmentation -------------- END -------------------------
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------

# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
#                                              Customer Segmentation
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
# Customer segmentation is as simple as it sounds: grouping customers by their characteristics –
#                       and why would you want to do that? To better serve their needs!
# Reference : http://www.salemmarafi.com/code/customer-segmentation-excel-and-r/
# Objective : What we want to do with K-Means clustering is classify customers based on offers they consume.

Offer_Info <- read.table("C:/Yashwanth/Data Analysis/Customer Segmentation/OfferInfo.csv",
                header = TRUE, sep = ",", quote = "\"", dec = "." ,fill=TRUE, comment.char="", as.is=TRUE)
Transactions <- read.table("C:/Yashwanth/Data Analysis/Customer Segmentation/Transactions.csv",
                         header = TRUE, sep = ",", quote = "\"", dec = "." ,fill=TRUE, comment.char="", as.is=TRUE)

#Load Library
library(reshape)

# Melt transactions, cast offer by customers
pivot<-melt(Transactions[1:2])
pivot1<-(cast(pivot,value~Customer.Last.Name,fill=0,fun.aggregate=function(x) length(x)))

# Bind to offers, we remove the first column of our new pivot because it's redundant.
pivot1<-cbind(Offer_Info,pivot1[-1])

# Clustering
library(fpc)

# Only use customer transaction data and we will rotate the matrix
cluster.data<-pivot1[,8:length(pivot1)]
cluster.data<-t(cluster.data)

# We will run KMeans using pamk (more robust) with 4 clusters.
cluster.kmeans<-pamk(cluster.data,k=4)
print(cluster.kmeans)

# Use this to view the clusters
View(cluster.kmeans$pamobject$clustering)

#Merge Data
cluster.deals<-merge(Transactions[1:2],cluster.kmeans$pamobject$clustering,by.x = "Customer.Last.Name", by.y = "row.names")
colnames(cluster.deals)<-c("Name","Offer","Cluster")


# Melt, cast, and bind
cluster.pivot<-melt(cluster.deals,id=c("Offer","Cluster"))
cluster.pivot<-cast(cluster.pivot,Offer~Cluster,fun.aggregate=length)
cluster.topDeals<-cbind(Offer_Info,cluster.pivot[-1])

# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
# ----------------- END ----------------------- Customer Segmentation -------------- END -----------------------
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------

# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
#                                             RFM Customer Segmentation
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------

library(lubridate)
library(data.table)



# Reference : http://www.marketingdistillery.com/2014/11/02/rfm-customer-segmentation-in-r-pandas-and-apache-spark/
#           http://www.dataapple.net/?p=133 ; http://www.dataapple.net/?p=84
#           http://www.simafore.com/blog/bid/159575/How-to-use-RFM-analysis-for-customer-segmentation-and-classification

# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
# ----------------- END ------------------- RFM Customer Segmentation -------------- END -----------------------
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------

# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
#                                    rpart(Recursive Partitioning & Regression Tree)
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
#             CART(Classification & Regression Tree) & CHAID(Chi-square Automatic Interaction Detector)
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
#                             Classification Tree / Conditional Inference Tree : Carseats
# --------------------------------------------------------------------------------------------------------------
rm(list=ls())
library(tree)
library(ISLR)
data(Carseats);Carseats
head(Carseats)
Carseats$High <- as.factor(ifelse(Carseats$Sales>=8,"Yes","No"))
Carseats <- Carseats[,-1]

# Split data into TRAIN & TEST
set.seed(2)
Train <- sample(1:nrow(Carseats),nrow(Carseats)/2)
Test <- -Train

Ctree.Train <- Carseats[Train,]
Ctree.Test <- Carseats[Test,]
Ctree.Testing_High <- Carseats$High[Test]

# fit the tree model using Train data
Ctree.Model <- tree(High ~ ., data=Ctree.Train)
plot(Ctree.Model)
text(Ctree.Model,pretty=0)

# check how model is doing using test data
Ctree.Pred <- predict(Ctree.Model,Ctree.Test,type="class")
mean(Ctree.Pred!=Ctree.Testing_High) # 28.5%

# Prune the tree
# cross validation to check where to stop pruning
set.seed(3)
Ctree.CV_Tree <- cv.tree(Ctree.Model,FUN=prune.misclass)
plot(Ctree.CV_Tree$size,Ctree.CV_Tree$dev,type="b",xlab="nsplit",ylab="relative error")

# Pruning
Ctree.Pruned <- prune.misclass(Ctree.Model,best=9)
plot(Ctree.Pruned);text(Ctree.Pruned,pretty=0)
Ctree.Pred_1 <- predict(Ctree.Pruned,Ctree.Test,type="class")
mean(Ctree.Pred_1!=Ctree.Testing_High) # 23%

# Conclusion : As misclassification error reduced from 28.5% to 23%, consider or retain the pruned tree.

# --------------------------------------------------------------------------------------------------------------
#                                     Classification Tree : rpart : Carseats
# --------------------------------------------------------------------------------------------------------------

rm(list=ls())
library(rpart)
Carseats$High <- as.factor(ifelse(Carseats$Sales>=8,"Yes","No"))
Carseats <- Carseats[,-1]

# Split data into TRAIN & TEST
set.seed(2)
Train <- sample(1:nrow(Carseats),nrow(Carseats)/2)
Test <- -Train

Rpart.Train <- Carseats[Train,]
Rpart.Test <- Carseats[Test,]
Rpart.Testing_High <- Carseats$High[Test]

Rpart <- rpart(High ~ ., data=Rpart.Train,method="class") # When method="class" then it is Gini rule. i.e, parms=list(split='gini')
summary(Rpart)
rsq.rpart(Rpart)

# Plot tree
dev.off()
plot(Rpart,uniform=TRUE,main="Classification Tree for Sales")
text(Rpart,use.n=TRUE,all=TRUE,cex=0.8,pretty=0)

# check how model is doing using test data
Rpart.Pred <- predict(Rpart,Rpart.Test,type="class")
mean(Rpart.Pred!=Rpart.Testing_High) # 23%

# Prune tree
# cross validation to check where to stop pruning :  Get cps'
printcp(Rpart)
plotcp(Rpart)

# Pruning
Rpart.Prune <- prune(Rpart,cp=0.01,nsplit=8)
plot(Rpart.Prune,uniform=TRUE,main="Classification Tree for Sales")
text(Rpart.Prune,use.n=TRUE,all=TRUE,cex=0.8,pretty=0)

# check how model is doing using test data
Rpart.Prune.Pred <- predict(Rpart.Prune,Rpart.Test,type="class")
Rpart.Prune.Prob <- predict(Rpart.Prune,Rpart.Test,type="prob") # Class probabilities
mean(Rpart.Prune.Pred!=Rpart.Testing_High) # 23%

# Conclusion : Even after pruning, misclassification remain same & hence the current split is the best fit.
# Reference : http://www.statmethods.net/advstats/cart.html
#       CP : http://datamining.togaware.com/survivor/Complexity_cp.html
#           http://www.statsoft.com/Textbook/Classification-and-Regression-Trees
#           https://www.youtube.com/watch?v=GOJN9SKl_OE

# --------------------------------------------------------------------------------------------------------------
#                                     Regression Tree : rpart : Carseats
# --------------------------------------------------------------------------------------------------------------

rm(list=ls())
library(rpart)

# Split data into TRAIN & TEST
set.seed(2)
Train <- sample(1:nrow(Carseats),nrow(Carseats)/2)
Test <- -Train

Rpart.Train <- Carseats[Train,]
Rpart.Test <- Carseats[Test,]
Rpart.Testing_Sales <- Carseats$Sales[Test]

Rpart <- rpart(Sales ~ ., data=Rpart.Train,method="anova")
summary(Rpart)

# Plot tree
dev.off()
plot(Rpart,uniform=TRUE,main="Regression Tree for Sales")
text(Rpart,use.n=TRUE,all=TRUE,cex=0.8,pretty=0)

# check how model is doing using test data
Rpart.Pred <- predict(Rpart,Rpart.Test,type="vector")
mean(Rpart.Pred!=Rpart.Testing_Sales)

# Prune tree
# cross validation to check where to stop pruning :  Get cps'
printcp(Rpart)
plotcp(Rpart)

# Pruning
Rpart.Prune <- prune(Rpart,cp=0.01,nsplit=15)
plot(Rpart.Prune,uniform=TRUE,main="Regression Tree for Sales")
text(Rpart.Prune,use.n=TRUE,all=TRUE,cex=0.8,pretty=0)

# check how model is doing using test data
Rpart.Prune.Pred <- predict(Rpart.Prune,Rpart.Test,type="vector")
mean(Rpart.Prune.Pred!=Rpart.Testing_Sales)

# --------------------------------------------------------------------------------------------------------------
#                                         CHAID : USvote
# --------------------------------------------------------------------------------------------------------------

rm(list=ls())
library(partykit)
install.packages("CHAID", repos="http://R-Forge.R-project.org",dependencies = TRUE)
library(CHAID)
data(USvote);USvote

### fit tree to subsample
set.seed(290875)
USvoteS <- USvote[sample(1:nrow(USvote), 1000),]
summary(USvote)

ctrl <- chaid_control(minsplit = 200, minprob = 0.1)
chaidUS <- chaid(vote3 ~ ., data = USvoteS, control = ctrl)
summary(chaidUS)

print(chaidUS)
plot(chaidUS)

# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
# --- END --- CART(Classification & Regression Tree) & CHAID(Chi-square Automatic Interaction Detector)-- END --
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------

# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
#                                                 Nueral Network(nnet)
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------

rm(list=ls())
library(nnet)
library(help=nnet)




# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
# ----------------- END --------------------------- nnet ----------------- END ---------------------------------
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------

# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
#                                                Paired-t-test
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------





# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
# ----------------- END ------------------------ Paired-t-test -------------- END ------------------------------
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------

# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
#                                             Crime against Women 2013
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------

rm(list=ls())
CAW <- read.table("C:/Yashwanth/Data Analysis/Crime against women 2013/crdCAW_1_2013.csv",
                  header = TRUE, sep = ",", quote = "\"", dec = "." ,fill=TRUE, comment.char="", as.is=TRUE)


# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
# ----------------- END -------------------- Crime against Women 2013 -------------- END -----------------------
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------

# http://cran.r-project.org/web/packages/pls/pls.pdf
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
#                                               PCA & Factor Analysis
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------

rm(list=ls())
data(iris);iris

# Summary
summary(iris)
pairs(iris[1:4],main="Iris Data", pch=19, col=as.numeric(iris$Species)+1)
mtext("Type of iris species: red-> setosa; green-> versicolor; blue-> virginica", 1, line=3.7,cex=.8)


#To examine variability of all numeric variables
sapply(iris[1:4],var)
range(sapply(iris[1:4],var))
# maybe this range of variability is big in this context.
#Thus, we will use the correlation matrix
#For this, we must standardize our variables with scale() function:
#We will standardize our variables when these have different units and have very different variances.
iris.stand <- as.data.frame(scale(iris[,c(1,3,4)]))
sapply(iris.stand,sd) #now, standard deviations are 1


# PCA
PCA <- prcomp(iris.stand,cor=TRUE);PCA
summary(PCA)

#plot of variance of each PCA.
#It will be useful to decide how many principal components should be retained.
screeplot(PCA, type="lines",col=3)

#The loadings for the principal components are stored in:
PCA$rotation # with princomp(): pca$loadings


#biplot of first two principal components
biplot(PCA,cex=0.8)
abline(h = 0, v = 0, lty = 2, col = 8)


# Factor Analysis
FA <- factanal(iris[,1:4],1,data=iris)


# Refeence : http://www.qualtrics.com/university/researchsuite/research-resources/data-analysis-guides/advanced-analysis-methods/factor-analysis/
#            https://freshbiostats.wordpress.com/2013/09/04/an-example-of-principal-components-analysis/
#            http://stats.stackexchange.com/questions/29824/factor-analysis-and-regression
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
# ----------------- END -------------------- PCA & Factor Analysis ------------------ END ----------------------
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------

# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
#                                          Principal Component Regression(PCR)
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------

library(pls)

# Refeence : http://www.qualtrics.com/university/researchsuite/research-resources/data-analysis-guides/advanced-analysis-methods/factor-analysis/
# Reference : http://en.wikipedia.org/wiki/Principal_component_regression

# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
# ----------------- END ----------------- Principal Component Regression(PCR) --------------- END --------------
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------

#---------------------------------------------------------------------------------------------------------------
#                                     Correlation for each subsets of data
#---------------------------------------------------------------------------------------------------------------

Sample <- TAB.ORDER[1:1000,c(9,15:18)]
Sample$Product_Base_Margin[is.na(Sample$Product_Base_Margin)] <- 0.05
Sample$Region <- as.factor(Sample$Region)


Split_Sample <- split(Sample[,c(2:5)],as.factor(Sample$Region))
lapply(Split_Sample,cor)

# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------

#---------------------------------------------------------------------------------------------------------------
#                                               Learn Swirl
#---------------------------------------------------------------------------------------------------------------


# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
# ----------------- END ---------------------- Learn Swirl -------------- END ----------------------------------
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------

# Notes : 1. Robust Regression : http://www.ats.ucla.edu/stat/r/dae/rreg.htm

# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
