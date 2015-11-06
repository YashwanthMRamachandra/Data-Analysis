# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
#                                       || Exploratory Data Analysis ||
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------

OD.Commercial <- read.table("C:/Yashwanth/Pricing/1.OfficeDepot/4.EDA/Commercial.csv", 
                  header = TRUE, sep = ",", quote = "\"", dec = "." ,fill=TRUE, comment.char="", as.is=TRUE)
str(OD.Commercial)
OD.Commercial$SKU <- as.factor(OD.Commercial$SKU);OD.Commercial$weekno <- as.factor(OD.Commercial$weekno)

# --------------------------------------------------------------------------------------------------------------
#                                       || Remove -ve Sales ||
# --------------------------------------------------------------------------------------------------------------

OD.Commercial <- data.frame(subset(OD.Commercial,sales_units>0,names(OD.Commercial)),row.names=NULL)

#---------------------------------------------------------------------------------------------------------------
#                                       Descriptive Statistics
#---------------------------------------------------------------------------------------------------------------

summary(OD.Commercial[sapply(OD.Commercial,is.numeric)]) # Select only numeric columns

#---------------------------------------------------------------------------------------------------------------
#                                       Distribution of Variables : Histogram
#---------------------------------------------------------------------------------------------------------------
par(mfrow=c(1,2))
hist(OD.Commercial$sales_units,prob=T,xlab="Sales Units",main="Sales Units distribution")
curve(dnorm(x,mean=mean(OD.Commercial$sales_units),sd=sd(OD.Commercial$sales_units)),add=TRUE)
hist(log(OD.Commercial$sales_units),prob=T,xlab="log(Sales Units)",main="Sales Units distribution")
curve(dnorm(x,mean=mean(log(OD.Commercial$sales_units)),sd=sd(log(OD.Commercial$sales_units))),add=TRUE)

par(mfrow=c(1,2))
hist(OD.Commercial$od_final_price,prob=T,xlab="Final Price",main="OD Final price distribution")
curve(dnorm(x,mean=mean(OD.Commercial$od_final_price),sd=sd(OD.Commercial$od_final_price)),add=TRUE)
hist(log(OD.Commercial$od_final_price),prob=T,xlab="log(Final Price)",main="OD Final price distribution")
curve(dnorm(x,mean=mean(log(OD.Commercial$od_final_price)),sd=sd(log(OD.Commercial$od_final_price))),add=TRUE)

par(mfrow=c(1,2))
hist(OD.Commercial$Amz_final_price,prob=T,xlab="Final Price",main="Amazon Final price distribution")
curve(dnorm(x,mean=mean(OD.Commercial$Amz_final_price,na.rm=T),sd=sd(OD.Commercial$Amz_final_price,na.rm=T)),add=TRUE)
hist(log(OD.Commercial$Amz_final_price),prob=T,xlab="log(Final Price)",main="Amazon Final price distribution")
curve(dnorm(x,mean=mean(log(OD.Commercial$Amz_final_price),na.rm=T),sd=sd(log(OD.Commercial$Amz_final_price),na.rm=T)),add=TRUE)

par(mfrow=c(1,2))
hist(OD.Commercial$Amz_mkt_final_price,prob=T,xlab="Final Price",main="Amz MP Final price distribution")
curve(dnorm(x,mean=mean(OD.Commercial$Amz_mkt_final_price,na.rm=T),sd=sd(OD.Commercial$Amz_mkt_final_price,na.rm=T)),add=TRUE)
hist(log(OD.Commercial$Amz_mkt_final_price),prob=T,xlab="log(Final Price)",main="Amz MP Final price distribution")
curve(dnorm(x,mean=mean(log(OD.Commercial$Amz_mkt_final_price),na.rm=T),sd=sd(log(OD.Commercial$Amz_mkt_final_price),na.rm=T)),add=TRUE)

par(mfrow=c(1,2))
hist(OD.Commercial$staples_final_price,prob=T,xlab="Final Price",main="Staples Final price distribution")
curve(dnorm(x,mean=mean(OD.Commercial$staples_final_price,na.rm=T),sd=sd(OD.Commercial$staples_final_price,na.rm=T)),add=TRUE)
hist(log(OD.Commercial$staples_final_price),prob=T,xlab="log(Final Price)",main="Staples Final price distribution")
curve(dnorm(x,mean=mean(log(OD.Commercial$staples_final_price),na.rm=T),sd=sd(log(OD.Commercial$staples_final_price),na.rm=T)),add=TRUE)

par(mfrow=c(1,2))
hist(OD.Commercial$min_comp_price,prob=T,xlab="Min Competitor Price",main="Min Competitor price distribution")
curve(dnorm(x,mean=mean(OD.Commercial$min_comp_price,na.rm=T),sd=sd(OD.Commercial$min_comp_price,na.rm=T)),add=TRUE)
hist(log(OD.Commercial$min_comp_price),prob=T,xlab="log(Min Competitor Price)",main="Min Competitor price distribution")
curve(dnorm(x,mean=mean(log(OD.Commercial$min_comp_price),na.rm=T),sd=sd(log(OD.Commercial$min_comp_price),na.rm=T)),add=TRUE)

#---------------------------------------------------------------------------------------------------------------
#                                       Correlataion Matrix
#---------------------------------------------------------------------------------------------------------------

cor(OD.Commercial[,c(3,4,6,8,10,12:17,23)],use="na.or.complete")

#---------------------------------------------------------------------------------------------------------------
#                                       Frequency Distribution
#---------------------------------------------------------------------------------------------------------------

attach(OD.Commercial)
range(sales_units)
OD_Sales_Breaks <- seq(1,120,10)
OD_Sales <- data.frame(table(cut(sales_units,OD_Sales_Breaks,right=TRUE)))
colnames(OD_Sales) <- c("OD_Sales","Freq")
OD_Sales$Cum_Freq <- cumsum(OD_Sales$Freq)
OD_Sales$Percentage <- (OD_Sales$Freq/sum(OD_Sales$Freq))*100
OD_Sales$Cum_Percentage <- cumsum((OD_Sales$Freq/sum(OD_Sales$Freq))*100)

range(od_final_price)
OD_FP_Breaks <- seq(90,4900,300)
OD_FP <- data.frame(table(cut(od_final_price,OD_FP_Breaks,right=TRUE)))
colnames(OD_FP) <- c("OD_FP","Freq")
OD_FP$Cum_Freq <- cumsum(OD_FP$Freq)
OD_FP$Percentage <- (OD_FP$Freq/sum(OD_FP$Freq))*100
OD_FP$Cum_Percentage <- cumsum((OD_FP$Freq/sum(OD_FP$Freq))*100)


range(Amz_final_price,na.rm=T)
AMZ_FP_Breaks <- seq(340,2500,300)
AMZ_FP <- data.frame(table(cut(Amz_final_price,AMZ_FP_Breaks,right=TRUE)))
colnames(AMZ_FP) <- c("AMZ_FP","Freq")
AMZ_FP$Cum_Freq <- cumsum(AMZ_FP$Freq)
AMZ_FP$Percentage <- (AMZ_FP$Freq/sum(AMZ_FP$Freq))*100
AMZ_FP$Cum_Percentage <- cumsum((AMZ_FP$Freq/sum(AMZ_FP$Freq))*100)


range(Amz_mkt_final_price,na.rm=T)
AMZ_MP_FP_Breaks <- seq(100,4500,300)
AMZ_MP_FP <- data.frame(table(cut(Amz_mkt_final_price,AMZ_MP_FP_Breaks,right=TRUE)))
colnames(AMZ_MP_FP) <- c("AMZ_MP_FP","Freq")
AMZ_MP_FP$Cum_Freq <- cumsum(AMZ_MP_FP$Freq)
AMZ_MP_FP$Percentage <- (AMZ_MP_FP$Freq/sum(AMZ_MP_FP$Freq))*100
AMZ_MP_FP$Cum_Percentage <- cumsum((AMZ_MP_FP$Freq/sum(AMZ_MP_FP$Freq))*100)


range(staples_final_price,na.rm=T)
Staples_FP_Breaks <- seq(100,4600,400)
Staples_FP <- data.frame(table(cut(staples_final_price,Staples_FP_Breaks,right=TRUE)))
colnames(Staples_FP) <- c("Staples_FP","Freq")
Staples_FP$Cum_Freq <- cumsum(Staples_FP$Freq)
Staples_FP$Percentage <- (Staples_FP$Freq/sum(Staples_FP$Freq))*100
Staples_FP$Cum_Percentage <- cumsum((Staples_FP$Freq/sum(Staples_FP$Freq))*100)
detach(OD.Commercial)

#---------------------------------------------------------------------------------------------------------------
#                                      Scatter Plot
#---------------------------------------------------------------------------------------------------------------

for(i in 1:nrow(OD.Commercial)){
  OD.Commercial$pr_diff_OD_final_pr[i] <- OD.Commercial$od_final_price[i]-OD.Commercial$od_final_price[i+1]
}
OD.Commercial$pr_diff_OD_final_pr[is.na(OD.Commercial$pr_diff_OD_final_pr)] <- OD.Commercial$pr_diff_OD_final_pr[i-1]

attach(OD.Commercial)
par(mfrow=c(1,2),cex.main=1.2)
plot(od_final_price,sales_units,xlab="OD Final Price",ylab="OD Sales",main="OD Sales v/s Price")
abline(lm(sales_units ~ od_final_price))
plot(pr_diff_OD_final_pr,sales_units,xlab="OD Final Price Diff",ylab="OD Sales ",main="OD Sales v/s Price Diff")
abline(lm(sales_units ~ pr_diff_OD_final_pr))

par(mfrow=c(1,2),cex.main=1.2)
plot(Amz_final_price,sales_units,xlab="OD Final Price",ylab="OD Sales",main="OD Sales v/s Price")
abline(lm(sales_units ~ Amz_final_price))
plot(pr_diff_amz_final_pr,sales_units,xlab="Amzazon Final Price Diff",ylab="OD Sales ",main="OD Sales v/s Amazon Price Diff")
abline(lm(sales_units ~ pr_diff_amz_final_pr))

par(mfrow=c(1,2),cex.main=1.2)
plot(Amz_mkt_final_price,sales_units,xlab="Amazon Marketplace Final Price",ylab="OD Sales",main="OD Sales v/s Amazon Marketplace Price")
abline(lm(sales_units ~ Amz_mkt_final_price))
plot(pr_diff_amz_mkt_final_pr,sales_units,xlab="Amazon Marketplace Final Price Diff",ylab="OD Sales ",main="OD Sales v/s Amazon Marketplace Price Diff")
abline(lm(sales_units ~ pr_diff_amz_mkt_final_pr))

par(mfrow=c(1,2),cex.main=1.2)
plot(staples_final_price,sales_units,xlab="Staples Final Price",ylab="OD Sales",main="OD Sales v/s Staples Price")
abline(lm(sales_units ~ staples_final_price))
plot(pr_diff_stpl_final_pr,sales_units,xlab="Staples Final Price Diff",ylab="OD Sales ",main="OD Sales v/s Staples Price Diff")
abline(lm(sales_units ~ pr_diff_stpl_final_pr))
detach(OD.Commercial)

#---------------------------------------------------------------------------------------------------------------
#                                      Box Plot
#---------------------------------------------------------------------------------------------------------------

attach(OD.Commercial)
boxplot(od_final_price ~ as.factor(weekno),xlab="Weeks",ylab="OD Final Price",main="Office Depot")
boxplot(Amz_final_price ~ as.factor(weekno),xlab="Weeks",ylab="Amazon Final Price",main="Amazon")
boxplot(Amz_mkt_final_price ~ as.factor(weekno),xlab="Weeks",ylab="Amazon Marketplace Final Price",main="Amazon Marketplace")
boxplot(staples_final_price ~ as.factor(weekno),xlab="Weeks",ylab="Staples Final Price",main="Staples")

detach(OD.Commercial)

#---------------------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------------

