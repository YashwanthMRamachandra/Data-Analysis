# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
#                                         || Price Elasticity ||
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------

# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
#                                        Linear Regression
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------

rm(list=ls());cat("\014")
OD.DP_Comm <- read.table("C:/Yashwanth/Pricing/1.OfficeDepot/7.Data to Import/Commercial.csv", 
                        header = TRUE, sep = ",", quote = "\"", dec = "." ,fill=TRUE, comment.char="", as.is=TRUE)

toMatch <- c("SKU","sales","final_price","pr_diff","min","week")
OD.DP_Comm <- OD.DP_Comm[,grep(paste(toMatch,collapse="|"),names(OD.DP_Comm),value=T)]

# --------------------------------------------------------------------------------------------------------------
#                                       || Remove -ve Sales ||
# --------------------------------------------------------------------------------------------------------------

OD.DP_Comm <- data.frame(subset(OD.DP_Comm,sales_units>0,names(OD.DP_Comm)),row.names=NULL)

# --------------------------------------------------------------------------------------------------------------
#                                       || Sales Trend ||
# --------------------------------------------------------------------------------------------------------------

plot(OD.DP_Comm$weekno,OD.DP_Comm$sales_units,type="l")

#---------------------------------------------------------------------------------------------------------------
#                                       Descriptive Statistics
#---------------------------------------------------------------------------------------------------------------

summary(OD.DP_Comm[sapply(OD.DP_Comm,is.numeric)]) # Select only numeric columns

#---------------------------------------------------------------------------------------------------------------
#                                       Correlataion Matrix
#---------------------------------------------------------------------------------------------------------------

toMatch_Com <- c("SKU","weekno","ln_")
names(OD.DP_Comm[,grep(paste(toMatch_Com,collapse="|"),names(OD.DP_Comm),value=T,invert=T)])
OD.DP_Comm_Cor <- OD.DP_Comm[,grep(paste(toMatch_Com,collapse="|"),names(OD.DP_Comm),value=T,invert=T)]

View(cor(OD.DP_Comm_Cor,use="na.or.complete"))
write.csv(cor(OD.DP_Comm_Cor,use="na.or.complete"),
          "C:/Yashwanth/Pricing/1.OfficeDepot/8.Data Output/1.Commercial/Cor_Commercial.csv")

#---------------------------------------------------------------------------------------------------------------
#                                       Transform Variables
#---------------------------------------------------------------------------------------------------------------

OD.DP_Comm_Trans <- OD.DP_Comm
OD.DP_Comm_Trans$sales_units <- sqrt(OD.DP_Comm_Trans$sales_units)
OD.DP_Comm_Trans$Amz_final_price <- (1/OD.DP_Comm_Trans$Amz_final_price)
OD.DP_Comm_Trans$Amz_mkt_final_price <- (1/OD.DP_Comm_Trans$Amz_mkt_final_price)
OD.DP_Comm_Trans$staples_final_price <- (1/OD.DP_Comm_Trans$staples_final_price)
OD.DP_Comm_Trans$min_comp_price <- (1/OD.DP_Comm_Trans$min_comp_price)

toMatch_Com <- c("SKU","weekno","ln_")
names(OD.DP_Comm_Trans[,grep(paste(toMatch_Com,collapse="|"),names(OD.DP_Comm_Trans),value=T,invert=T)])
OD.DP_Comm_Trans <- OD.DP_Comm_Trans[,grep(paste(toMatch_Com,collapse="|"),names(OD.DP_Comm_Trans),value=T,invert=T)]

View(cor(OD.DP_Comm_Trans,use="na.or.complete"))

#---------------------------------------------------------------------------------------------------------------
#                                       Distribution of Variables : Histogram
#---------------------------------------------------------------------------------------------------------------

# Office Depot
dev.off()
par(mfrow=c(1,2))
hist(OD.DP_Comm_Trans$sales_units,prob=T,xlab="Sales Units",main="Sales Units distribution")
curve(dnorm(x,mean=mean(OD.DP_Comm_Trans$sales_units),sd=sd(OD.DP_Comm_Trans$sales_units)),add=TRUE)
hist(log(OD.DP_Comm_Trans$sales_units),prob=T,xlab="log(Sales Units)",main="Sales Units distribution")
curve(dnorm(x,mean=mean(log(OD.DP_Comm_Trans$sales_units)),sd=sd(log(OD.DP_Comm_Trans$sales_units))),add=TRUE)

par(mfrow=c(1,2))
hist(OD.DP_Comm_Trans$od_final_price,prob=T,xlab="Final Price",main="OD Final price distribution")
curve(dnorm(x,mean=mean(OD.DP_Comm_Trans$od_final_price),sd=sd(OD.DP_Comm_Trans$od_final_price)),add=TRUE)
hist(log(OD.DP_Comm_Trans$od_final_price),prob=T,xlab="log(Final Price)",main="OD Final price distribution")
curve(dnorm(x,mean=mean(log(OD.DP_Comm_Trans$od_final_price)),sd=sd(log(OD.DP_Comm_Trans$od_final_price))),add=TRUE)


# Amazon
par(mfrow=c(1,2))
hist(OD.DP_Comm_Trans$Amz_final_price,prob=T,xlab="Final Price",main="Amazon Final price distribution")
curve(dnorm(x,mean=mean(OD.DP_Comm_Trans$Amz_final_price,na.rm=T),sd=sd(OD.DP_Comm_Trans$Amz_final_price,na.rm=T)),add=TRUE)
hist(log(OD.DP_Comm_Trans$Amz_final_price),prob=T,xlab="log(Final Price)",main="Amazon Final price distribution")
curve(dnorm(x,mean=mean(log(OD.DP_Comm_Trans$Amz_final_price),na.rm=T),sd=sd(log(OD.DP_Comm_Trans$Amz_final_price),na.rm=T)),add=TRUE)

# Amazon Marketplace
par(mfrow=c(1,2))
hist(OD.DP_Comm_Trans$Amz_mkt_final_price,prob=T,xlab="Final Price",main="Amazon Final price distribution")
curve(dnorm(x,mean=mean(OD.DP_Comm_Trans$Amz_mkt_final_price,na.rm=T),sd=sd(OD.DP_Comm_Trans$Amz_mkt_final_price,na.rm=T)),add=TRUE)
hist(log(OD.DP_Comm_Trans$Amz_mkt_final_price),prob=T,xlab="log(Final Price)",main="Amazon Final price distribution")
curve(dnorm(x,mean=mean(log(OD.DP_Comm_Trans$Amz_mkt_final_price),na.rm=T),sd=sd(log(OD.DP_Comm_Trans$Amz_mkt_final_price),na.rm=T)),add=TRUE)

# Staples
par(mfrow=c(1,2))
hist(OD.DP_Comm_Trans$staples_final_price,prob=T,xlab="Final Price",main="Amazon Final price distribution")
curve(dnorm(x,mean=mean(OD.DP_Comm_Trans$staples_final_price,na.rm=T),sd=sd(OD.DP_Comm_Trans$staples_final_price,na.rm=T)),add=TRUE)
hist(log(OD.DP_Comm_Trans$staples_final_price),prob=T,xlab="log(Final Price)",main="Amazon Final price distribution")
curve(dnorm(x,mean=mean(log(OD.DP_Comm_Trans$staples_final_price),na.rm=T),sd=sd(log(OD.DP_Comm_Trans$staples_final_price),na.rm=T)),add=TRUE)

# Min Competitor Price
par(mfrow=c(1,2))
hist(OD.DP_Comm_Trans$min_comp_price,prob=T,xlab="Min Competitor Price",main="Min Competitor price distribution")
curve(dnorm(x,mean=mean(OD.DP_Comm_Trans$min_comp_price,na.rm=T),sd=sd(OD.DP_Comm_Trans$min_comp_price,na.rm=T)),add=TRUE)
hist(log(OD.DP_Comm_Trans$min_comp_price),prob=T,xlab="log(Min Competitor Price)",main="Min Competitor price distribution")
curve(dnorm(x,mean=mean(log(OD.DP_Comm_Trans$min_comp_price),na.rm=T),sd=sd(log(OD.DP_Comm_Trans$min_comp_price),na.rm=T)),add=TRUE)

# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
#                                        Normality Check : Shapiro-Wilk test
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------

shapiro.test(OD.DP_Comm_Trans$sales_units)
shapiro.test(OD.DP_Comm_Trans$od_final_price)
shapiro.test(OD.DP_Comm_Trans$Amz_final_price)
shapiro.test(OD.DP_Comm_Trans$Amz_mkt_final_price)
shapiro.test(OD.DP_Comm_Trans$staples_final_price)

# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
#                                                   T-test
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------

t.test(OD.DP_Comm_Trans$od_final_price,OD.DP_Comm_Trans$Amz_final_price)
t.test(OD.DP_Comm_Trans$od_final_price,OD.DP_Comm_Trans$Amz_mkt_final_price)
t.test(OD.DP_Comm_Trans$od_final_price,OD.DP_Comm_Trans$staples_final_price)
t.test(OD.DP_Comm_Trans$od_final_price,OD.DP_Comm_Trans$min_comp_price)

# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
#                                       Sales Driver Analysis and Price Elasticity Analysis
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------

pairs(OD.DP_Comm_Trans,col="blue",pch=20)
View(cor(OD.DP_Comm_Trans,use="na.or.complete"))

# Inference : Since Correlation is very high, variables might have high influence over the sales & hence include it in the model

#---------------------------------------------------------------------------------------------------------------
#                                       Transform Variables
#---------------------------------------------------------------------------------------------------------------

OD.DP_Comm_Trans <- OD.DP_Comm
OD.DP_Comm_Trans$sales_units <- (1/OD.DP_Comm_Trans$sales_units)
OD.DP_Comm_Trans$Amz_final_price <- (1/OD.DP_Comm_Trans$Amz_final_price)
OD.DP_Comm_Trans$Amz_mkt_final_price <- (1/OD.DP_Comm_Trans$Amz_mkt_final_price)
OD.DP_Comm_Trans$staples_final_price <- (1/OD.DP_Comm_Trans$staples_final_price)
OD.DP_Comm_Trans$min_comp_price <- (1/OD.DP_Comm_Trans$min_comp_price)

toMatch_Com <- c("SKU","weekno","ln_")
names(OD.DP_Comm_Trans[,grep(paste(toMatch_Com,collapse="|"),names(OD.DP_Comm_Trans),value=T,invert=T)])
OD.DP_Comm_Trans <- OD.DP_Comm_Trans[,grep(paste(toMatch_Com,collapse="|"),names(OD.DP_Comm_Trans),value=T,invert=T)]

View(cor(OD.DP_Comm_Trans,use="na.or.complete"))

#---------------------------------------------------------------------------------------------------------------
#                                                Linear Model
#---------------------------------------------------------------------------------------------------------------

# Linear Model
Model_1 <- lm(log(sales_units) ~ log(od_final_price)+log(Amz_final_price)+log(Amz_mkt_final_price)+log(staples_final_price),
                          data=OD.DP_Comm_Trans)
summary(Model_1)

# Inference : 1. Since p < 0.05, we can include price varaible into the model. In other words, Price has strong evidence in explaining
#                the Sales.
#             2. 86% of the variations in sales can be explained by Price alone & the remaining 14% can be attributed to other factors 
#                or inherent variability.

# The assumptions for the regression to be true. are that data are random and independent; 
# residuals are normally distributed and have constant variance. Let's check the residuals assumptions visually.
# plotting the residuals vs. other key model metrics
par(mfrow=c(1,2))
plot(Model_1)

# Inference : The Residuals vs Fitted graph shows that, the residuals scatter around the fitted line with no obvious pattern, 
#             and the Normal Q-Q graph shows that basically the residuals are normally distributed. The assumptions are met.

# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
