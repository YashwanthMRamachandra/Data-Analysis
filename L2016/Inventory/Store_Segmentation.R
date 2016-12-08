# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
#                                   USHI : Inventory : Store Segmentation : Method-1
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------

Inventory_SS_M1 <- read.table("C:/Yashwanth/2. Inventory/2. Store Segmentation/Store_Segmentation_method_1.csv",
                        header = TRUE, sep = ",", quote = "\"", dec = "." ,fill=TRUE, comment.char="", as.is=TRUE)
Inventory_SS_M1$Location <- as.factor(Inventory_SS_M1$Location)
Inventory_SS_M1$Quarter <- as.factor(Inventory_SS_M1$Quarter)
Inventory_SS_M1$Sales_Dollar <- as.numeric(Inventory_SS_M1$Sales_Dollar)
Inventory_SS_M1$Sales_Dollar_Comp <- as.numeric(Inventory_SS_M1$Sales_Dollar_Comp)
str(Inventory_SS_M1)

# Filter to non-zero Sales$
library(data.table)
Inventory_SS_M1 <- data.table(Inventory_SS_M1)
Inventory_SS_M1 <- Inventory_SS_M1[Sales_Dollar_Comp>0]

#---------------------------------------------------------------------------------------------------------------
#                                       Descriptive Statistics
#---------------------------------------------------------------------------------------------------------------

library(psych)
Inventory_SS_M1 <- as.data.frame(Inventory_SS_M1)
View(describe(Inventory_SS_M1[,sapply(Inventory_SS_M1,is.numeric)]))
#summary(Inventory_SS_M1)
#summary(Inventory_SS_M1[,sapply(Inventory_SS_M1,is.numeric)])

# Correlation Matrix : All
View(cor(Inventory_SS_M1[,sapply(Inventory_SS_M1,is.numeric)]))
# There is 90%(very strong) correlation lies b\n Sales$ & Turn rate

# --------------------------------------------------------------------------------------------------------------
#                                    Distribution of Variables : Histogram
# --------------------------------------------------------------------------------------------------------------

library(ggplot2) # Reference : http://docs.ggplot2.org/0.9.2.1/

dev.off()
par(mfrow=c(2,2))
attach(Inventory_SS_M1)
ggplot(Inventory_SS_M1,aes(x=Sales_Dollar)) +
  geom_histogram(aes(y=..density..),col = "blue2") +
  stat_function(fun = dnorm,args = list(mean=mean(Sales_Dollar),sd=sd(Sales_Dollar)),colour = "red") +
  labs(title = "Sales Dollar Distribution",x = "Sales$")

p1 <- ggplot(Inventory_SS_M1,aes(x=Sales_Dollar_Comp)) +
  geom_histogram(aes(y=..density..),col = "blue2") +
  stat_function(fun = dnorm,args = list(mean=mean(Sales_Dollar_Comp),sd=sd(Sales_Dollar_Comp)),colour = "red") +
  labs(title = "Sales Dollar Comp Distribution", x = "Sales$ Comp")

p2 <- ggplot(Inventory_SS_M1,aes(x=Weekly_Turn_Rate)) +
  geom_histogram(aes(y=..density..),col = "blue2") +
  stat_function(fun = dnorm,args = list(mean=mean(Weekly_Turn_Rate),sd=sd(Weekly_Turn_Rate)),colour = "red") +
  labs(title = "Weekly Turn Rate Distribution", x = "Weekly Turnrate")
detach(Inventory_SS_M1)

multiplot(p1,p2,cols=2)

# Inference: Both Sales$ & Weekly Turnrate follows similar distribution & hence resulting 
#		in high correlation.

# --------------------------------------------------------------------------------------------------------------
#                                           Boxplot
# --------------------------------------------------------------------------------------------------------------

attach(Inventory_SS_M1)
ggplot(Inventory_SS_M1,aes(x=Quarter,y=Sales_Dollar)) +
  geom_boxplot(aes(fill=Quarter),outlier.colour = "green",outlier.size = 3)+
  labs(title = "Sales Dollar Distribution")

p3 <- ggplot(Inventory_SS_M1,aes(x=Quarter,y=Sales_Dollar_Comp)) +
  geom_boxplot(aes(fill=Quarter),outlier.colour = "green",outlier.size = 3)+
  labs(title = "Sales$ Comp Distribution", x = "Quarters", y = "Sales$ Comp")

p4 <- ggplot(Inventory_SS_M1,aes(x=Quarter,y=Weekly_Turn_Rate)) +
  geom_boxplot(aes(fill=Quarter),outlier.colour = "green",outlier.size = 3)+
  labs(title = "Weekly Turnrate Distribution", x = "Quarters", y = "Weekly Turnrate")
detach(Inventory_SS_M1)

multiplot(p3,p4,cols=2)

# --------------------------------------------------------------------------------------------------------------
#                                           Quartiles
# --------------------------------------------------------------------------------------------------------------

Quartiles_Sales_Dollar <- quantile(Inventory_SS_M1$Sales_Dollar)
Quartiles_Sales_Dollar_Comp <- quantile(Inventory_SS_M1$Sales_Dollar_Comp)
Quartiles_Turnrate <- quantile(Inventory_SS_M1$Weekly_Turn_Rate)

Inventory_SS_M1$Quarter_Number_Sales_Dollar <- 
		ifelse(Inventory_SS_M1$Sales_Dollar>Quartiles_Sales_Dollar[1]&Inventory_SS_M1$Sales_Dollar<Quartiles_Sales_Dollar[2],"Q1",
		ifelse(Inventory_SS_M1$Sales_Dollar>Quartiles_Sales_Dollar[2]&Inventory_SS_M1$Sales_Dollar<Quartiles_Sales_Dollar[3],"Q2",
		ifelse(Inventory_SS_M1$Sales_Dollar>Quartiles_Sales_Dollar[3]&Inventory_SS_M1$Sales_Dollar<Quartiles_Sales_Dollar[4],"Q3",
		"Q4")))
Inventory_SS_M1$Quarter_Number_Sales_Dollar_Comp <- 
		ifelse(Inventory_SS_M1$Sales_Dollar_Comp>Quartiles_Sales_Dollar_Comp[1]&Inventory_SS_M1$Sales_Dollar_Comp<Quartiles_Sales_Dollar_Comp[2],"Q1",
		ifelse(Inventory_SS_M1$Sales_Dollar_Comp>Quartiles_Sales_Dollar_Comp[2]&Inventory_SS_M1$Sales_Dollar_Comp<Quartiles_Sales_Dollar_Comp[3],"Q2",
		ifelse(Inventory_SS_M1$Sales_Dollar_Comp>Quartiles_Sales_Dollar_Comp[3]&Inventory_SS_M1$Sales_Dollar_Comp<Quartiles_Sales_Dollar_Comp[4],"Q3",
		"Q4")))
Inventory_SS_M1$Quarter_Number_Weekly_Turn_Rate <- 
		ifelse(Inventory_SS_M1$Weekly_Turn_Rate>Quartiles_Turnrate[1]&Inventory_SS_M1$Weekly_Turn_Rate<Quartiles_Turnrate[2],"Q1",
		ifelse(Inventory_SS_M1$Weekly_Turn_Rate>Quartiles_Turnrate[2]&Inventory_SS_M1$Weekly_Turn_Rate<Quartiles_Turnrate[3],"Q2",
		ifelse(Inventory_SS_M1$Weekly_Turn_Rate>Quartiles_Turnrate[3]&Inventory_SS_M1$Weekly_Turn_Rate<Quartiles_Turnrate[4],"Q3",
		"Q4")))
View(Inventory_SS_M1)

# --------------------------------------------------------------------------------------------------------------
#                                           Scatterplot
# --------------------------------------------------------------------------------------------------------------

ggplot(Inventory_SS_M1,aes(Weekly_Turn_Rate,Sales_Dollar_Comp))+
	geom_point(colour="red",size=2)+
	stat_smooth(method = "lm", se=FALSE)
	labs(title = "Correlation between Sales$ Comp and Turnrate", x = "Turn rate", y = "Sales$ Comp")

# --------------------------------------------------------------------------------------------------------------
#                                           Hypothesis Testing
# --------------------------------------------------------------------------------------------------------------

# H_0 : Sales$ & Turnrate are not Highly Correlated(<0.7)
# H_1 : Sales & Turnrate are Highly Correlated(>0.7)

cor(Inventory_SS_M1[,sapply(Inventory_SS_M1,is.numeric)])

# Conclusion : We reject Null Hypothesis & conclude that, Sales$ and Turnrate are
			Higly correlated. Hence we can consider either of the metric for 
			Quartering.

# --------------------------------------------------------------------------------------------------------------
#                                           Clustering
# --------------------------------------------------------------------------------------------------------------

table(Inventory_SS_M1$Quarter_Number_Sales_Dollar_Comp,Inventory_SS_M1$Quarter_Number_Weekly_Turn_Rate)

# --------------------------------------------------------------------------------------------------------------
# -------------- END ---------- USHI : Inventory : Store Segmentation : Method-1 ---------- END -----------
# --------------------------------------------------------------------------------------------------------------



# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
#                                   USHI : Inventory : Store Segmentation : Method-2
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------

Inventory_SS_M2 <- read.table("C:/Yashwanth/2. Inventory/2. Store Segmentation/Store_Segmentation_method_2.csv",
                        header = TRUE, sep = ",", quote = "\"", dec = "." ,fill=TRUE, comment.char="", as.is=TRUE)
Inventory_SS_M2$Location <- as.factor(Inventory_SS_M2$Location)
Inventory_SS_M2$Weekly_Inventory_Dollar_Variance_to_LY <- as.numeric(Inventory_SS_M2$Weekly_Inventory_Dollar_Variance_to_LY)
Inventory_SS_M2$Weekly_Sales_Dollar_Variance_to_LY <- as.numeric(Inventory_SS_M2$Weekly_Sales_Dollar_Variance_to_LY)
dim(Inventory_SS_M2)
Inventory_SS_M2 <- Inventory_SS_M2[complete.cases(Inventory_SS_M2),];dim(Inventory_SS_M2)
str(Inventory_SS_M2)

#---------------------------------------------------------------------------------------------------------------
#                                       Descriptive Statistics
#---------------------------------------------------------------------------------------------------------------

# Remove Outliers
Inventory_SS_M2 <- data.table(Inventory_SS_M2)
Inventory_SS_M2 <- Inventory_SS_M2[Weekly_Inventory_Dollar_Variance_to_LY>-1]
dim(Inventory_SS_M2)

library(psych)
Inventory_SS_M2 <- as.data.frame(Inventory_SS_M2)
View(describe(Inventory_SS_M2[,sapply(Inventory_SS_M2,is.numeric)]))
#summary(Inventory_SS_M2)
#summary(Inventory_SS_M2[,sapply(Inventory_SS_M2,is.numeric)])

# Correlation Matrix : All
View(cor(Inventory_SS_M2[,sapply(Inventory_SS_M2,is.numeric)]))
# There is 8%(Weekly) correlation lies b\n Inventory$ Var & Sales$ Var

# --------------------------------------------------------------------------------------------------------------
#                                    Distribution of Variables : Histogram
# --------------------------------------------------------------------------------------------------------------

library(ggplot2) # Reference : http://docs.ggplot2.org/0.9.2.1/

dev.off()
par(mfrow=c(2,2))
attach(Inventory_SS_M2)
p2_1 <- ggplot(Inventory_SS_M2,aes(x=Weekly_Sales_Dollar_Variance_to_LY)) +
  geom_histogram(aes(y=..density..),col = "blue2") +
  stat_function(fun = dnorm,args = list(mean=mean(Weekly_Sales_Dollar_Variance_to_LY),sd=sd(Weekly_Sales_Dollar_Variance_to_LY)),colour = "red") +
  labs(title = "Sales$ Variance to LY Distribution",x = "Sales$ Variance")

p2_2 <- ggplot(Inventory_SS_M2,aes(x=Weekly_Inventory_Dollar_Variance_to_LY)) +
  geom_histogram(aes(y=..density..),col = "blue2") +
  stat_function(fun = dnorm,args = list(mean=mean(Weekly_Inventory_Dollar_Variance_to_LY),sd=sd(Weekly_Inventory_Dollar_Variance_to_LY)),colour = "red") +
  labs(title = "Inventory$ Variance to LY Distribution", x = "Inventory$ Variance")
detach(Inventory_SS_M2)

multiplot(p2_1,p2_2,cols=2)

# Inference: Both Sales$ & Weekly Turnrate follows similar distribution & hence resulting 
#		in high correlation.

# --------------------------------------------------------------------------------------------------------------
#                                           Scatterplot
# --------------------------------------------------------------------------------------------------------------

ggplot(Inventory_SS_M2,aes(Weekly_Inventory_Dollar_Variance_to_LY,Weekly_Sales_Dollar_Variance_to_LY))+
	geom_point(colour="red",size=2)+
	stat_smooth(method = "lm", se=FALSE)
	labs(title = "Correlation between Sales$ Variance and Inventory$ Variance", x = "Inventory$ Variance", y = "Sales$ Variance")

# --------------------------------------------------------------------------------------------------------------
#                                           Hypothesis Testing
# --------------------------------------------------------------------------------------------------------------

# H_0 : Sales$ Variance & Inventory$ Variance are not Weakly Correlated(<0.05)
# H_1 : Sales$ Variance & Inventory$ Variance are Weakly Correlated(>0.05)

cor(Inventory_SS_M2[,sapply(Inventory_SS_M2,is.numeric)])

# --------------------------------------------------------------------------------------------------------------
#                                           Quartiles
# --------------------------------------------------------------------------------------------------------------

Quartile_Weekly_Sales_Dollar_Variance_to_LY <- quantile(Inventory_SS_M2$Weekly_Sales_Dollar_Variance_to_LY)
Quartile_Weekly_Inventory_Dollar_Variance_to_LY <- quantile(Inventory_SS_M2$Weekly_Inventory_Dollar_Variance_to_LY)

attach(Inventory_SS_M2)
Inventory_SS_M2$Quartile_Weekly_Sales_Dollar_Variance_to_LY <- 
		ifelse(Weekly_Sales_Dollar_Variance_to_LY>Quartile_Weekly_Sales_Dollar_Variance_to_LY[1] & Weekly_Sales_Dollar_Variance_to_LY<Quartile_Weekly_Sales_Dollar_Variance_to_LY[2],"Q1",
		ifelse(Weekly_Sales_Dollar_Variance_to_LY>Quartile_Weekly_Sales_Dollar_Variance_to_LY[2] & Weekly_Sales_Dollar_Variance_to_LY<Quartile_Weekly_Sales_Dollar_Variance_to_LY[3],"Q2",
		ifelse(Weekly_Sales_Dollar_Variance_to_LY>Quartile_Weekly_Sales_Dollar_Variance_to_LY[3] & Weekly_Sales_Dollar_Variance_to_LY<Quartile_Weekly_Sales_Dollar_Variance_to_LY[4],"Q3",
		"Q4")))
Inventory_SS_M2$Quartile_Weekly_Inventory_Dollar_Variance_to_LY <- 
		ifelse(Weekly_Inventory_Dollar_Variance_to_LY>Quartile_Weekly_Inventory_Dollar_Variance_to_LY[1] & Weekly_Inventory_Dollar_Variance_to_LY<Quartile_Weekly_Inventory_Dollar_Variance_to_LY[2],"Q1",
		ifelse(Weekly_Inventory_Dollar_Variance_to_LY>Quartile_Weekly_Inventory_Dollar_Variance_to_LY[2] & Weekly_Inventory_Dollar_Variance_to_LY<Quartile_Weekly_Inventory_Dollar_Variance_to_LY[3],"Q2",
		ifelse(Weekly_Inventory_Dollar_Variance_to_LY>Quartile_Weekly_Inventory_Dollar_Variance_to_LY[3] & Weekly_Inventory_Dollar_Variance_to_LY<Quartile_Weekly_Inventory_Dollar_Variance_to_LY[4],"Q3",
		"Q4")))
detach(Inventory_SS_M2)

Inventory_SS_M2$Quartile_Weekly_Sales_Dollar_Variance_to_LY <- as.factor(Inventory_SS_M2$Quartile_Weekly_Sales_Dollar_Variance_to_LY)
Inventory_SS_M2$Quartile_Weekly_Inventory_Dollar_Variance_to_LY <- as.factor(Inventory_SS_M2$Quartile_Weekly_Inventory_Dollar_Variance_to_LY)

View(Inventory_SS_M2)

# Percentiles
quantile(Inventory_SS_M2$Weekly_Sales_Dollar_Variance_to_LY,prob=seq(0,1,by=0.01))

# Percentile Rank
percentilerank<-function(x){
  rx<-rle(sort(x))
  smaller<-cumsum(c(0, rx$lengths))[seq(length(rx$lengths))]
  larger<-rev(cumsum(c(0, rev(rx$lengths))))[-1]
  rxpr<-smaller/(smaller+larger)
  rxpr[match(x, rx$values)]
}

Inventory_SS_M2$Per_Rank <- percentilerank(Inventory_SS_M2$Weekly_Sales_Dollar_Variance_to_LY)
head(Inventory_SS_M2)

# --------------------------------------------------------------------------------------------------------------
#                                          K-means Clustering : Elbow method
# --------------------------------------------------------------------------------------------------------------

table(Inventory_SS_M2$Quartile_Weekly_Inventory_Dollar_Variance_to_LY,Inventory_SS_M2$Quartile_Weekly_Sales_Dollar_Variance_to_LY)


# http://www.inside-r.org/packages/cran/GMD/docs/elbow
library(clusterSim)
Inventory_SS_M2 <- as.data.frame(Inventory_SS_M2)
#Inventory_SS_Std <- data.Normalization(Inventory_SS_M2[,c(3,4)],type="n1")
#View(Inventory_SS_Std)

## determine a "good" k using elbow
require("GMD")
dist.obj <- dist(Inventory_SS_M2[,c(3,4)])
hclust.obj <- hclust(dist.obj)
css.obj <- css.hclust(dist.obj,hclust.obj) # Clustering Sum-of-Squares for Clustering Evaluation
elbow.obj <- elbow.batch(css.obj)
print(elbow.obj)

## make partition given the "good" k
k <- elbow.obj$k; cutree.obj <- cutree(hclust.obj,k=k)
Inventory_SS_M2$cluster <- cutree.obj
 
## draw a elbow plot and label the data
dev.new(width=12, height=6)
par(mfcol=c(1,2),mar=c(4,5,3,3),omi=c(0.75,0,0,0))
plot(Inventory_SS_M2$Sales_Dollar,Inventory_SS_M2$Weekly_Turn_Rate,pch=as.character(Inventory_SS_M2$cluster),
col=Inventory_SS_Std$cluster,cex=0.75,main="Quarters of simulated data")
plot(css.obj,elbow.obj,if.plot.new=FALSE)


# K-means
set.seed(100)
Inventory_SS_M2_KM <- kmeans(Inventory_SS_M2[,c(3,4)],centers = 5)
Inventory_SS_M2$Clusters <- Inventory_SS_M2_KM$cluster

write.csv(Inventory_SS_M2, "C:/Yashwanth/2. Inventory/2. Store Segmentation/Clusters.csv")
write.csv(Inventory_SS_M2_KM$centers, "C:/Yashwanth/2. Inventory/2. Store Segmentation/Centers.csv")

plot(Inventory_SS_M2[,c(3,4)], col=Inventory_SS_M2_KM$cluster,xlab = "Inventory$ Variance", ylab = "Sales$ Comp Variance")
points(Inventory_SS_M2_KM$centers[,c(1,2)], col=1:3, pch=8, cex=2)

# --------------------------------------------------------------------------------------------------------------
#                                           Naive Bayes Classifier
# --------------------------------------------------------------------------------------------------------------

# http://stackoverflow.com/questions/10059594/a-simple-explanation-of-naive-bayes-classification
library(openxlsx)
Inventory_SS_M2_NBC <- read.xlsx("C:/Yashwanth/2. Inventory/2. Store Segmentation/Store_Segmentation NaiveBayesClassifier.xlsx",
                        sheet=3)
Inventory_SS_M2_NBC$Location <- as.factor(Inventory_SS_M2_NBC$Location)
Inventory_SS_M2_NBC$Weekly_Inventory_Dollar_Variance_to_LY <- as.numeric(Inventory_SS_M2_NBC$Weekly_Inventory_Dollar_Variance_to_LY)
Inventory_SS_M2_NBC$Weekly_Sales_Dollar_Variance_to_LY <- as.numeric(Inventory_SS_M2_NBC$Weekly_Sales_Dollar_Variance_to_LY)
Inventory_SS_M2_NBC$Inventory_Dollar_Var_Flag <- as.factor(Inventory_SS_M2_NBC$Inventory_Dollar_Var_Flag)
Inventory_SS_M2_NBC$Sales_Dollar_Var_Flag <- as.factor(Inventory_SS_M2_NBC$Sales_Dollar_Var_Flag)
Inventory_SS_M2_NBC$Profiles <- as.factor(Inventory_SS_M2_NBC$Profiles)
Inventory_SS_M2_NBC$Cluster_5 <- as.factor(Inventory_SS_M2_NBC$Cluster_5)
str(Inventory_SS_M2_NBC);dim(Inventory_SS_M2_NBC)

# Remove Outliers
Inventory_SS_M2_NBC <- data.table(Inventory_SS_M2_NBC)
Inventory_SS_M2_NBC <- Inventory_SS_M2_NBC[Weekly_Inventory_Dollar_Variance_to_LY>-1]
dim(Inventory_SS_M2_NBC)
Inventory_SS_M2_NBC <- data.frame(Inventory_SS_M2_NBC)

# Naive Bayes Classifier
library(e1071)
table(Inventory_SS_M2_NBC$Sales_Dollar_Var_Flag,Inventory_SS_M2_NBC$Inventory_Dollar_Var_Flag)
Inventory_SS_M2_NBC_Mod <- naiveBayes(Profiles ~ Sales_Dollar_Var_Flag+Inventory_Dollar_Var_Flag,
			data = Inventory_SS_M2_NBC)
Inventory_SS_M2_NBC_Pred <- predict(Inventory_SS_M2_NBC_Mod,Inventory_SS_M2_NBC,type="class")
Inventory_SS_M2_NBC_Prob <- predict(Inventory_SS_M2_NBC_Mod,Inventory_SS_M2_NBC,type="raw")
table(Inventory_SS_M2_NBC_Pred,Inventory_SS_M2_NBC$Cluster_5)
table(Inventory_SS_M2_NBC$Cluster_5,Inventory_SS_M2_NBC$Sales_Dollar_Var_Flag)


Inventory_SS_M2_NBC_PP <- data.frame(Inventory_SS_M2_NBC,Inventory_SS_M2_NBC_Pred,
					Inventory_SS_M2_NBC_Prob)
write.csv(Inventory_SS_M2_NBC_PP,"C:/Yashwanth/2. Inventory/2. Store Segmentation/Pred_Prob_Profile.csv",
			row.names=TRUE)

#coef(lm(Sales_Dollar_Var_Flag ~ Inventory_Dollar_Var_Flag+Weekly_Inventory_Dollar_Variance_to_LY+Turn_Rate,
#			data = Inventory_SS_M2_NBC))

# --------------------------------------------------------------------------------------------------------------
# -------------- END ---------- USHI : Inventory : Store Segmentation : Method-2 ---------- END ----------------
# --------------------------------------------------------------------------------------------------------------





# --------------------------------------------------------------------------------------------------------------
#                                   USHI : Inventory : Store Segmentation : Method-3
# --------------------------------------------------------------------------------------------------------------

Inventory_SS_M3 <- read.table("C:/Yashwanth/2. Inventory/2. Store Segmentation/Store_Segmentation_method_2.csv",
                        header = TRUE, sep = ",", quote = "\"", dec = "." ,fill=TRUE, comment.char="", as.is=TRUE)
Inventory_SS_M3$Location <- as.factor(Inventory_SS_M3$Location)
Inventory_SS_M3$Average_Weekly_GMROI <- as.numeric(Inventory_SS_M3$Average_Weekly_GMROI)
Inventory_SS_M3$Weekly_GMROI_Variance_to_LY <- as.numeric(Inventory_SS_M3$Weekly_GMROI_Variance_to_LY)
Inventory_SS_M3$Sales_Dollar <- as.numeric(Inventory_SS_M3$Sales_Dollar)
Inventory_SS_M3$Sales_Dollar_Comp <- as.numeric(Inventory_SS_M3$Sales_Dollar_Comp)
Inventory_SS_M3$Weekly_Average_NPI_Dollar <- as.numeric(Inventory_SS_M3$Weekly_Average_NPI_Dollar)
str(Inventory_SS_M3);dim(Inventory_SS_M3)

# Filter to non-zero Sales$ Comp
library(data.table)
Inventory_SS_M3 <- data.table(Inventory_SS_M3)
Inventory_SS_M3 <- Inventory_SS_M3[Sales_Dollar_Comp>0]
dim(Inventory_SS_M3)

#---------------------------------------------------------------------------------------------------------------
#                                       Descriptive Statistics
#---------------------------------------------------------------------------------------------------------------

library(psych)
Inventory_SS_M3 <- as.data.frame(Inventory_SS_M3)
View(describe(Inventory_SS_M3[,sapply(Inventory_SS_M3,is.numeric)]))
#summary(Inventory_SS_M3)
#summary(Inventory_SS_M3[,sapply(Inventory_SS_M3,is.numeric)])

# Correlation Matrix : All
View(cor(Inventory_SS_M3[,sapply(Inventory_SS_M3,is.numeric)]))

# --------------------------------------------------------------------------------------------------------------
#                                    Distribution of Variables : Histogram
# --------------------------------------------------------------------------------------------------------------

library(ggplot2) # Reference : http://docs.ggplot2.org/0.9.2.1/

dev.off()
par(mfrow=c(2,2))
attach(Inventory_SS_M3)
ggplot(Inventory_SS_M3,aes(x=Sales_Dollar)) +
  geom_histogram(aes(y=..density..),col = "blue2") +
  stat_function(fun = dnorm,args = list(mean=mean(Sales_Dollar),sd=sd(Sales_Dollar)),colour = "red") +
  labs(title = "Sales Dollar Distribution",x = "Sales$")

p1 <- ggplot(Inventory_SS_M3,aes(x=Sales_Dollar_Comp)) +
  geom_histogram(aes(y=..density..),col = "blue2") +
  stat_function(fun = dnorm,args = list(mean=mean(Sales_Dollar_Comp),sd=sd(Sales_Dollar_Comp)),colour = "red") +
  labs(title = "Sales Dollar Comp Distribution", x = "Sales$ Comp")

p2 <- ggplot(Inventory_SS_M3,aes(x=Weekly_Turn_Rate)) +
  geom_histogram(aes(y=..density..),col = "blue2") +
  stat_function(fun = dnorm,args = list(mean=mean(Weekly_Turn_Rate),sd=sd(Weekly_Turn_Rate)),colour = "red") +
  labs(title = "Weekly Turn Rate Distribution", x = "Weekly Turnrate")

p3 <- ggplot(Inventory_SS_M3,aes(x=Average_Weekly_GMROI)) +
  geom_histogram(aes(y=..density..),col = "blue2") +
  stat_function(fun = dnorm,args = list(mean=mean(Average_Weekly_GMROI),sd=sd(Average_Weekly_GMROI)),colour = "red") +
  labs(title = " Average Weekly GMROI Distribution", x = "Average Weekly GMROI")

p4 <- ggplot(Inventory_SS_M3,aes(x=Weekly_GMROI_Variance_to_LY)) +
  geom_histogram(aes(y=..density..),col = "blue2") +
  stat_function(fun = dnorm,args = list(mean=mean(Weekly_GMROI_Variance_to_LY),sd=sd(Weekly_GMROI_Variance_to_LY)),colour = "red") +
  labs(title = "Weekly GMROI Variance to LY Distribution", x = "Weekly GMROI Variance to LY")

p5 <- ggplot(Inventory_SS_M3,aes(x=Weekly_Average_NPI_Dollar)) +
  geom_histogram(aes(y=..density..),col = "blue2") +
  stat_function(fun = dnorm,args = list(mean=mean(Weekly_Average_NPI_Dollar),sd=sd(Weekly_Average_NPI_Dollar)),colour = "red") +
  labs(title = "Weekly Average NPI Dollar Distribution", x = "Weekly Average NPI Dollar")
detach(Inventory_SS_M3)

multiplot(p1,p2,cols=2)
multiplot(p3,p4,p5,rows=3)

# Inference: Both Sales$ & Weekly Turnrate follows similar distribution & hence resulting 
#		in high correlation.

# --------------------------------------------------------------------------------------------------------------
#                                           Boxplot
# --------------------------------------------------------------------------------------------------------------

attach(Inventory_SS_M3)
ggplot(Inventory_SS_M3,aes(x=Quarter,y=Sales_Dollar)) +
  geom_boxplot(aes(fill=Quarter),outlier.colour = "green",outlier.size = 3)+
  labs(title = "Sales Dollar Distribution")

p3 <- ggplot(Inventory_SS_M3,aes(x=Quarter,y=Sales_Dollar_Comp)) +
  geom_boxplot(aes(fill=Quarter),outlier.colour = "green",outlier.size = 3)+
  labs(title = "Sales$ Comp Distribution", x = "Quarters", y = "Sales$ Comp")

p4 <- ggplot(Inventory_SS_M3,aes(x=Quarter,y=Weekly_Turn_Rate)) +
  geom_boxplot(aes(fill=Quarter),outlier.colour = "green",outlier.size = 3)+
  labs(title = "Weekly Turnrate Distribution", x = "Quarters", y = "Weekly Turnrate")
detach(Inventory_SS_M3)

multiplot(p3,p4,cols=2)

# --------------------------------------------------------------------------------------------------------------
#                                           Scatterplot
# --------------------------------------------------------------------------------------------------------------

attach(Inventory_SS_M3)
# coef(lm(Sales_Dollar_Comp ~ Weekly_GMROI_Variance_to_LY,data = Inventory_SS_M3))
p5 <- ggplot(Inventory_SS_M3,aes(Weekly_GMROI_Variance_to_LY,Sales_Dollar_Comp))+
	geom_point(colour="red",size=2)+
	geom_abline(intercept = 33518585.27, slope = -33027.65)
	labs(title = "Correlation between Sales$ Comp and Weekly GMROI Variance to LY",
		 x = "Weekly GMROI Variance to LY", y = "Sales$ Comp")

# coef(lm(Weekly_Average_NPI_Dollar ~ Weekly_GMROI_Variance_to_LY,data = Inventory_SS_M3))
p6 <- ggplot(Inventory_SS_M3,aes(Weekly_GMROI_Variance_to_LY,Weekly_Average_NPI_Dollar))+
	geom_point(colour="red",size=2)+
	geom_abline(intercept = 226865.3353, slope = -490.0831)
	labs(title = "Correlation between Weekly Average NPI Dollar and Weekly GMROI Variance to LY",
		 x = "Weekly GMROI Variance to LY", y = "Weekly Average NPI Dollar")
detach(Inventory_SS_M3)

multiplot(p5,p6,cols=2)

# --------------------------------------------------------------------------------------------------------------
#                                           Quartiles
# --------------------------------------------------------------------------------------------------------------

Quartiles_Sales_Dollar <- quantile(Inventory_SS_M3$Sales_Dollar)
Quartiles_Sales_Dollar_Comp <- quantile(Inventory_SS_M3$Sales_Dollar_Comp)
Quartiles_Turnrate <- quantile(Inventory_SS_M3$Weekly_Turn_Rate)

Inventory_SS_M3$Quarter_Number_Sales_Dollar <- 
		ifelse(Inventory_SS_M3$Sales_Dollar>Quartiles_Sales_Dollar[1]&Inventory_SS_M3$Sales_Dollar<Quartiles_Sales_Dollar[2],"Q1",
		ifelse(Inventory_SS_M3$Sales_Dollar>Quartiles_Sales_Dollar[2]&Inventory_SS_M3$Sales_Dollar<Quartiles_Sales_Dollar[3],"Q2",
		ifelse(Inventory_SS_M3$Sales_Dollar>Quartiles_Sales_Dollar[3]&Inventory_SS_M3$Sales_Dollar<Quartiles_Sales_Dollar[4],"Q3",
		"Q4")))
Inventory_SS_M3$Quarter_Number_Sales_Dollar_Comp <- 
		ifelse(Inventory_SS_M3$Sales_Dollar_Comp>Quartiles_Sales_Dollar_Comp[1]&Inventory_SS_M3$Sales_Dollar_Comp<Quartiles_Sales_Dollar_Comp[2],"Q1",
		ifelse(Inventory_SS_M3$Sales_Dollar_Comp>Quartiles_Sales_Dollar_Comp[2]&Inventory_SS_M3$Sales_Dollar_Comp<Quartiles_Sales_Dollar_Comp[3],"Q2",
		ifelse(Inventory_SS_M3$Sales_Dollar_Comp>Quartiles_Sales_Dollar_Comp[3]&Inventory_SS_M3$Sales_Dollar_Comp<Quartiles_Sales_Dollar_Comp[4],"Q3",
		"Q4")))
Inventory_SS_M3$Quarter_Number_Weekly_Turn_Rate <- 
		ifelse(Inventory_SS_M3$Weekly_Turn_Rate>Quartiles_Turnrate[1]&Inventory_SS_M3$Weekly_Turn_Rate<Quartiles_Turnrate[2],"Q1",
		ifelse(Inventory_SS_M3$Weekly_Turn_Rate>Quartiles_Turnrate[2]&Inventory_SS_M3$Weekly_Turn_Rate<Quartiles_Turnrate[3],"Q2",
		ifelse(Inventory_SS_M3$Weekly_Turn_Rate>Quartiles_Turnrate[3]&Inventory_SS_M3$Weekly_Turn_Rate<Quartiles_Turnrate[4],"Q3",
		"Q4")))
View(Inventory_SS_M3)

# --------------------------------------------------------------------------------------------------------------
#                                           Hypothesis Testing
# --------------------------------------------------------------------------------------------------------------

# H_0 : Sales$ & Turnrate are not Highly Correlated(<0.7)
# H_1 : Sales & Turnrate are Highly Correlated(>0.7)

cor(Inventory_SS_M3[,sapply(Inventory_SS_M3,is.numeric)])

# Conclusion : We reject Null Hypothesis & conclude that, Sales$ and Turnrate are
			Higly correlated. Hence we can consider either of the metric for 
			Quartering.

# --------------------------------------------------------------------------------------------------------------
#                                           Clustering
# --------------------------------------------------------------------------------------------------------------

table(Inventory_SS_M3$Quarter_Number_Sales_Dollar_Comp,Inventory_SS_M3$Quarter_Number_Weekly_Turn_Rate)

# --------------------------------------------------------------------------------------------------------------
#                                           Elbow Method
# --------------------------------------------------------------------------------------------------------------

# http://www.inside-r.org/packages/cran/GMD/docs/elbow
library(clusterSim)
Inventory_SS_Std <- data.Normalization(Inventory_SS[,c(3,5)],type="n1")
View(Inventory_SS_Std)

## determine a "good" k using elbow
require("GMD")
dist.obj <- dist(Inventory_SS_Std)
hclust.obj <- hclust(dist.obj)
css.obj <- css.hclust(dist.obj,hclust.obj)
elbow.obj <- elbow.batch(css.obj)
print(elbow.obj)

## make partition given the "good" k
k <- elbow.obj$k; cutree.obj <- cutree(hclust.obj,k=k)
Inventory_SS_Std$cluster <- cutree.obj
 
## draw a elbow plot and label the data
dev.new(width=12, height=6)
par(mfcol=c(1,2),mar=c(4,5,3,3),omi=c(0.75,0,0,0))
plot(Inventory_SS_Std$Sales_Dollar,Inventory_SS_Std$Weekly_Turn_Rate,pch=as.character(Inventory_SS_Std$cluster),
col=Inventory_SS_Std$cluster,cex=0.75,main="Quarters of simulated data")
plot(css.obj,elbow.obj,if.plot.new=FALSE)

# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
# ---------- END ------------- USHI : Inventory : Store Segmentation : Method-3 ------------- END --------------
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------


# Multiplot in R
# http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                    ncol = cols, nrow = ceiling(numPlots/cols))
  }

 if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
