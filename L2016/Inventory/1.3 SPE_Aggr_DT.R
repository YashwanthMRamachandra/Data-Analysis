	# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
#                                    Staples Price Elasticity : Decision Tree
# --------------------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------------
#                                            Decision Tree : Over time
#---------------------------------------------------------------------------------------------------------------

rm(list=ls())
load("C:/Yashwanth/Staples PE/2. Scripts/0. R_data/3. Staples_PE_Aggr.RData")

Staples_Aggr_Monthly_Dec <- merge(Staples_Aggr_Monthly,Staples_Corr_By_Prod,by="Product_ID")
Staples_Aggr_Monthly_Dec <- Staples_Aggr_Monthly_Dec[Decision_Criterion==1 | Decision_Criterion==2]
Staples_Aggr_Monthly_Dec$Decision_Criterion <- as.factor(Staples_Aggr_Monthly_Dec$Decision_Criterion)

# Sampling
set.seed(400)
Staples_Aggr_Monthly_Dec_IND <- sample(nrow(Staples_Aggr_Monthly_Dec),size=round(((nrow(Staples_Aggr_Monthly_Dec)/100)*70)+1,0))
Staples_Aggr_Monthly_Dec_Train <- Staples_Aggr_Monthly_Dec[Staples_Aggr_Monthly_Dec_IND,]
Staples_Aggr_Monthly_Dec_Test <- Staples_Aggr_Monthly_Dec[-Staples_Aggr_Monthly_Dec_IND,]

# Build the Tree
library(rpart)
Staples_DT <- rpart(Decision_Criterion ~ Conversion+Price_Comp+OA_Bounce_rate+OA_Cart_Views+OA_Visit_Freq, 
                    data = Staples_Aggr_Monthly_Dec_Train)
summary(Staples_DT)

library(rpart.plot)
prp(Staples_DT)

library(rattle)
library(colorspace)
# rattle()
fancyRpartPlot(Staples_DT,cex = 0.6,sub = "Correlation Distribution",main = "Correlation Decision for products over time")

# Prune the tree
fancyRpartPlot(prune(Staples_DT,cp=0.01),cex = 0.6,sub = "Prune Correlation Distribution")

# Prune using "rpart.control" : rpart.control(minsplit = 30)
Staples_DT_RC <- rpart(Decision_Criterion ~ Conversion+Price_Comp, data = Staples_Aggr_Monthly_Dec,
                       control = rpart.control(minsplit=30))
fancyRpartPlot(Staples_DT_RC,cex = 0.3,sub = "Prune Correlation Distribution")


# Validate the Model or Classsification Accuracy
library(xtable)
library(reshape)
Staples_Aggr_Monthly_Dec_Test <- data.frame(cbind(Staples_Aggr_Monthly_Dec_Test,Actual = Staples_Aggr_Monthly_Dec_Test$Decision_Criterion,
                                                  Pred = predict(Staples_DT,Staples_Aggr_Monthly_Dec_Test,type = "class"),
                                                  Prob = predict(Staples_DT,Staples_Aggr_Monthly_Dec_Test,type = "prob")))

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

#---------------------------------------------------------------------------------------------------------------
#                                            Decision Tree : By Product : Annually
#---------------------------------------------------------------------------------------------------------------

Staples_Aggr_Monthly_by_Prod <- data.table(Staples_Aggr_Monthly_by_Prod)
Staples_Aggr_Monthly_Dec <- merge(Staples_Aggr_Monthly_by_Prod,Staples_Corr_By_Prod,by="Product_ID")
Staples_Aggr_Monthly_Dec <- Staples_Aggr_Monthly_Dec[Decision_Criterion==1 | Decision_Criterion==2]
Staples_Aggr_Monthly_Dec$Decision_Criterion <- as.factor(Staples_Aggr_Monthly_Dec$Decision_Criterion)

# Sampling
set.seed(400)
Staples_Aggr_Monthly_Dec_IND <- sample(nrow(Staples_Aggr_Monthly_Dec),size=round(((nrow(Staples_Aggr_Monthly_Dec)/100)*70)+1,0))
Staples_Aggr_Monthly_Dec_Train <- Staples_Aggr_Monthly_Dec[Staples_Aggr_Monthly_Dec_IND,]
Staples_Aggr_Monthly_Dec_Test <- Staples_Aggr_Monthly_Dec[-Staples_Aggr_Monthly_Dec_IND,]

# Build the Tree
library(rpart)
Staples_DT <- rpart(Decision_Criterion ~ Conversion+Price_Comp, data = Staples_Aggr_Monthly_Dec_Train)
summary(Staples_DT)
plot(Staples_DT)
text(Staples_DT,use.n = TRUE,all = TRUE)

library(rpart.plot)
prp(Staples_DT)

library(rattle)
library(colorspace)
rattle() # https://www.youtube.com/watch?v=OBilaZZpvGs
par(mfrow=c(1,2),mar=c(3,2,2,1))
fancyRpartPlot(Staples_DT,cex = 0.6,sub = "Correlation Distribution",main = "Correlation Decision for unique products")

# Prune the tree
fancyRpartPlot(prune(Staples_DT,cp=0.01),cex = 0.6,sub = "Prune Correlation Distribution")

# Prune using "rpart.control" : rpart.control(minsplit = 30)
Staples_DT_RC <- rpart(Decision_Criterion ~ Conversion+Price_Comp, data = Staples_Aggr_Monthly_Dec,
                       control = rpart.control(minsplit=30))
fancyRpartPlot(Staples_DT_RC,cex = 0.3,sub = "Prune Correlation Distribution")

# Inference : 1. 
# Reference : https://cran.r-project.org/web/packages/rpart.plot/rpart.plot.pdf
#             http://www.milbo.org/rpart-plot/prp.pdf

# Validate the Model or Classsification Accuracy
library(xtable)
library(reshape)
Staples_Aggr_Monthly_Dec_Test <- data.frame(cbind(Staples_Aggr_Monthly_Dec_Test,Actual = Staples_Aggr_Monthly_Dec_Test$Decision_Criterion,
                                                  Pred = predict(Staples_DT,Staples_Aggr_Monthly_Dec_Test,type = "class"),
                                                  Prob = predict(Staples_DT,Staples_Aggr_Monthly_Dec_Test,type = "prob")))

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
rm(toMatch,Class_Table,DT,Staples_Aggr_Monthly_Dec_IND)

#---------------------------------------------------------------------------------------------------------------
#                                        Receiver Operating Characteristics : ROC
#---------------------------------------------------------------------------------------------------------------

library(ROCR)
Prediction <- prediction(as.numeric(Staples_Aggr_Monthly_Dec_Test$Pred),as.numeric(Staples_Aggr_Monthly_Dec_Test$Decision_Criterion))
par(mfrow=c(1,2),mar=c(3,2,2,1))
Perf1 <- performance(Prediction,measure="tpr",x.measure="fpr")    # True versus False
plot(Perf1,main="ROC Curve")

Perf2 <- performance(Prediction,measure="sens",x.measure="spec") # True versus True
plot(Perf2,main="Sensitivity & Specificity")
rm(Perf1,Perf2,Prediction)

# ROC : ggplot
library(ggROC)
ggroc(data = rocData,bin = 0.01, roccol = "green", sp = 19)
rocData <- data.frame(cbind(Pred = as.factor(Staples_Aggr_Monthly_Dec_Test$Pred),
                            Actual = as.factor(Staples_Aggr_Monthly_Dec_Test$Decision_Criterion)))

#rocdata <- function(grp, pred){
# Produces x and y co-ordinates for ROC curve plot
# Arguments: grp - labels classifying subject status
#            pred - values of each observation
# Output: List with 2 components:
#         roc = data.frame with x and y co-ordinates of plot
#         stats = data.frame containing: area under ROC curve, p value, upper and lower 95% confidence interval

grp <- as.factor(grp)
if (length(pred) != length(grp)) {
  stop("The number of classifiers must match the number of data points")
} 

if (length(levels(grp)) != 2) {
  stop("There must only be 2 values for the classifier")
}

cut <- unique(pred)
tp <- sapply(cut, function(x) length(which(pred > x & grp == levels(grp)[2])))
fn <- sapply(cut, function(x) length(which(pred < x & grp == levels(grp)[2])))
fp <- sapply(cut, function(x) length(which(pred > x & grp == levels(grp)[1])))
tn <- sapply(cut, function(x) length(which(pred < x & grp == levels(grp)[1])))
tpr <- tp / (tp + fn)
fpr <- fp / (fp + tn)
roc = data.frame(x = fpr, y = tpr)
roc <- roc[order(roc$x, roc$y),]

i <- 2:nrow(roc)
auc <- (roc$x[i] - roc$x[i - 1]) %*% (roc$y[i] + roc$y[i - 1])/2

pos <- pred[grp == levels(grp)[2]]
neg <- pred[grp == levels(grp)[1]]
q1 <- auc/(2-auc)
q2 <- (2*auc^2)/(1+auc)
se.auc <- sqrt(((auc * (1 - auc)) + ((length(pos) -1)*(q1 - auc^2)) + ((length(neg) -1)*(q2 - auc^2)))/(length(pos)*length(neg)))
ci.upper <- auc + (se.auc * 0.96)
ci.lower <- auc - (se.auc * 0.96)

se.auc.null <- sqrt((1 + length(pos) + length(neg))/(12*length(pos)*length(neg)))
z <- (auc - 0.5)/se.auc.null
p <- 2*pnorm(-abs(z))

stats <- data.frame (auc = auc,
                     p.value = p,
                     ci.upper = ci.upper,
                     ci.lower = ci.lower
)

return (list(roc = roc, stats = stats))
}
#rocplot.single <- function(grp, pred, title = "ROC Plot", p.value = FALSE){
require(ggplot2)
plotdata <- rocdata(grp, pred)

if (p.value == TRUE){
  annotation <- with(plotdata$stats, paste("AUC=",signif(auc, 2), " (P=", signif(p.value, 2), ")", sep=""))
} else {
  annotation <- with(plotdata$stats, paste("AUC=",signif(auc, 2), " (95%CI ", signif(ci.upper, 2), " - ", signif(ci.lower, 2), ")", sep=""))
}

p <- ggplot(plotdata$roc, aes(x = x, y = y)) +
  geom_line(aes(colour = "")) +
  geom_abline (intercept = 0, slope = 1) +
  theme_bw() +
  scale_x_continuous("False Positive Rate (1-Specificity)") +
  scale_y_continuous("True Positive Rate (Sensitivity)") +
  scale_colour_manual(labels = annotation, values = "#000000") +
  opts(title = title,
       plot.title = theme_text(face="bold", size=14), 
       axis.title.x = theme_text(face="bold", size=12),
       axis.title.y = theme_text(face="bold", size=12, angle=90),
       panel.grid.major = theme_blank(),
       panel.grid.minor = theme_blank(),
       legend.justification=c(1,0), 
       legend.position=c(1,0),
       legend.title=theme_blank(),
       legend.key = theme_blank()
  )
return(p)
}
#rocplot.multiple <- function(test.data.list, groupName = "Actual", predName = "res", title = "ROC Plot", p.value = TRUE){
require(plyr)
require(ggplot2)
plotdata <- llply(test.data.list, function(x) with(x, rocdata(Actual = eval(parse(text = groupName)), pred = eval(parse(text = predName)))))
plotdata <- list(roc = ldply(plotdata, function(x) x$roc),
                 stats = ldply(plotdata, function(x) x$stats)
)

if (p.value == TRUE){
  annotation <- with(plotdata$stats, paste("AUC=",signif(auc, 2), " (P=", signif(p.value, 2), ")", sep=""))
} else {
  annotation <- with(plotdata$stats, paste("AUC=",signif(auc, 2), " (95%CI ", signif(ci.upper, 2), " - ", signif(ci.lower, 2), ")", sep=""))
}

p <- ggplot(plotdata$roc, aes(x = x, y = y)) +
  geom_line(aes(colour = .id)) +
  geom_abline (intercept = 0, slope = 1) +
  theme_bw() +
  scale_x_continuous("False Positive Rate (1-Specificity)") +
  scale_y_continuous("True Positive Rate (Sensitivity)") +
  scale_colour_brewer(palette="Set1", breaks = names(test.data.list), labels = paste(names(test.data.list), ": ", annotation, sep = "")) +
  opts(title = title,
       plot.title = theme_text(face="bold", size=14), 
       axis.title.x = theme_text(face="bold", size=12),
       axis.title.y = theme_text(face="bold", size=12, angle=90),
       panel.grid.major = theme_blank(),
       panel.grid.minor = theme_blank(),
       legend.justification=c(1,0), 
       legend.position=c(1,0),
       legend.title=theme_blank(),
       legend.key = theme_blank()
  )
return(p)
}#
#rocplot.single(rocData$Pred,rocData$Actual)

# Reference : https://cran.r-project.org/web/packages/ggROC/ggROC.pdf
#             http://www.r-bloggers.com/simple-roc-plots-with-ggplot2-part-1/
#             http://www.r-bloggers.com/simple-roc-plots-with-ggplot2-part-2/

# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
# ---------------- END ------------------ Staples Price Elasticity : Decision Tree ---------- END --------------
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------