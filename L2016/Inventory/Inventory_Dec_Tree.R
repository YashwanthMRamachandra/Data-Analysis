# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
#                                  Inventory Decision Tree
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------

Inv_Tra_Week48_MSD_IP <- read.table("C:/Yashwanth/2. Inventory/In transit week 48 MSD.csv",
     header = TRUE, sep = ",", quote = "\"", dec = "." ,fill=TRUE, comment.char="", as.is=TRUE)

str(Inv_Tra_Week48_MSD_IP)

Inv_Tra_Week48_MSD_IP$Business_Area <- as.factor(Inv_Tra_Week48_MSD_IP$Business_Area)
Inv_Tra_Week48_MSD_IP$Merchandise_Division <- as.factor(Inv_Tra_Week48_MSD_IP$Merchandise_Division)
Inv_Tra_Week48_MSD_IP$Merchandise_Sub_Division <- as.factor(Inv_Tra_Week48_MSD_IP$Merchandise_Sub_Division)

# Build the Tree
library(rpart)
Inv_Tra_DT <- rpart(DC_landed ~ Business_Area+Merchandise_Division+Merchandise_Sub_Division, 
                    data = Inv_Tra_Week48_MSD_IP)
summary(Inv_Tra_DT)

library(rpart.plot)

prp(Inv_Tra_DT)

library(rattle)
library(colorspace)
# rattle()
fancyRpartPlot(Inv_Tra_DT,cex = 0.6,sub = "DC Landed Distribution",main = "DC Landed Decision for products over time")

# Prune the tree
fancyRpartPlot(prune(Inv_Tra_DT,cp=0.01),cex = 0.6,sub = "Prune Correlation Distribution")

# Prune using "rpart.control" : rpart.control(minsplit = 30)
Inv_Tra_DT_RC <- rpart(Decision_Criterion ~ Conversion+Price_Comp, data = Staples_Aggr_Monthly_Dec,
                       control = rpart.control(minsplit=30))
fancyRpartPlot(Inv_Tra_DT_RC,cex = 0.3,sub = "Prune Correlation Distribution")


# Validate the Model or Classsification Accuracy
library(xtable)
library(reshape)
Staples_Aggr_Monthly_Dec_Test <- data.frame(cbind(Staples_Aggr_Monthly_Dec_Test,Actual = Staples_Aggr_Monthly_Dec_Test$Decision_Criterion,
                                                  Pred = predict(Inv_Tra_DT,Staples_Aggr_Monthly_Dec_Test,type = "class"),
                                                  Prob = predict(Inv_Tra_DT,Staples_Aggr_Monthly_Dec_Test,type = "prob")))

table(Actual=Staples_Aggr_Monthly_Dec_Test$Ac,Predict=Staples_Aggr_Monthly_Dec_Test$Pred)
DT <- cbind(melt(table(Actual=Staples_Aggr_Monthly_Dec_Test$Decision_Criterion,Predict=Staples_Aggr_Monthly_Dec_Test$Pred)),
            Decision=c("False Negative","True Negative","False Positive","True Positive"))
DT <- data.table(DT)
setorderv(DT,"Decision",order = -1);DT

Class_Table <- xtable(table(Actual=Staples_Aggr_Monthly_Dec_Test$Decision_Criterion,Predict=Staples_Aggr_Monthly_Dec_Test$Pred))
cbind(Class_Table <- xtable(table(Actual=Staples_Aggr_Monthly_Dec_Test$Decision_Criterion,Predict=Staples_Aggr_Monthly_Dec_Test$Pred)),
      Per_Correct = c((Class_Table[1,1]/nrow(Staples_Aggr_Monthly_Dec_Test)*100),(Class_Table[2,2]/nrow(Staples_Aggr_Monthly_Dec_Test)*100)))
sum(cbind(Class_Table <- xtable(table(Actual=Staples_Aggr_Monthly_Dec_Test$Decision_Criterion,Predict=Staples_Aggr_Monthly_Dec_Test$Pred)),
          Per_Correct = c((Class_Table[1,1]/nrow(Staples_Aggr_Monthly_Dec_Test)*100),(Class_Table[2,2]/nrow(Staples_Aggr_Monthly_Dec_Test)*100)))[3])
rm(Class_Table,DT,Staples_Aggr_Monthly_Dec_IND)

# ROC
library(ROCR)
Prediction <- prediction(as.numeric(Staples_Aggr_Monthly_Dec_Test$Pred),as.numeric(Staples_Aggr_Monthly_Dec_Test$Decision_Criterion))
par(mfrow=c(1,2),mar=c(3,2,2,1))
Perf1 <- performance(Prediction,measure="tpr",x.measure="fpr")    # True versus False
plot(Perf1,main="ROC Curve")

Perf2 <- performance(Prediction,measure="sens",x.measure="spec") # True versus True
plot(Perf2,main="Sensitivity & Specificity")
rm(Perf1,Perf2,Prediction)

library(pROC)
ROC <- roc(Staples_Aggr_Monthly_Dec_Test$Pred,as.numeric(Staples_Aggr_Monthly_Dec_Test$Decision_Criterion),plot=TRUE)                     # True versus True
