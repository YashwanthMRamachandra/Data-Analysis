# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
#                                       || Exploratory Data Analysis ||
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------

OD.ShreddersAcc <- read.table("C:/Yashwanth/Pricing/1.OfficeDepot/8.Data Output/Shredder_Bags_Output.csv", 
                  header = TRUE, sep = ",", quote = "\"", dec = "." ,fill=TRUE, comment.char="", as.is=TRUE)
str(OD.ShreddersAcc)
OD.ShreddersAcc$SKU <- as.factor(OD.ShreddersAcc$SKU);OD.ShreddersAcc$weekno <- as.factor(OD.ShreddersAcc$weekno)

# --------------------------------------------------------------------------------------------------------------
#                                       || Remove -ve Sales ||
# --------------------------------------------------------------------------------------------------------------

OD.ShreddersAcc <- data.frame(subset(OD.ShreddersAcc,sales_units>0,names(OD.ShreddersAcc)),row.names=NULL)

#---------------------------------------------------------------------------------------------------------------
#                                       Descriptive Statistics
#---------------------------------------------------------------------------------------------------------------

summary(OD.ShreddersAcc[sapply(OD.ShreddersAcc,is.numeric)]) # Select only numeric columns

#---------------------------------------------------------------------------------------------------------------
#                                       Distribution of Variables : Histogram
#---------------------------------------------------------------------------------------------------------------
# Office Depot
par(mfrow=c(1,2))
hist(OD.ShreddersAcc$sales_units,prob=T,xlab="Sales Units",main="Sales Units distribution")
curve(dnorm(x,mean=mean(OD.ShreddersAcc$sales_units),sd=sd(OD.ShreddersAcc$sales_units)),add=TRUE)
hist(log(OD.ShreddersAcc$sales_units),prob=T,xlab="log(Sales Units)",main="Sales Units distribution")
curve(dnorm(x,mean=mean(log(OD.ShreddersAcc$sales_units)),sd=sd(log(OD.ShreddersAcc$sales_units))),add=TRUE)

par(mfrow=c(1,2))
hist(OD.ShreddersAcc$od_reg_price,prob=T,xlab="Regular Price",main="Regular Price distribution")
curve(dnorm(x,mean=mean(OD.ShreddersAcc$od_reg_price),sd=sd(OD.ShreddersAcc$od_reg_price)),add=TRUE)
hist(log(OD.ShreddersAcc$od_reg_price),prob=T,xlab="log(Regular Price)",main="Regular Price distribution")
curve(dnorm(x,mean=mean(log(OD.ShreddersAcc$od_reg_price)),sd=sd(log(OD.ShreddersAcc$od_reg_price))),add=TRUE)

par(mfrow=c(1,2))
hist(OD.ShreddersAcc$od_ship_price,prob=T,xlab="Shipping Price",main="Shipping Price distribution")
curve(dnorm(x,mean=mean(OD.ShreddersAcc$od_ship_price),sd=sd(OD.ShreddersAcc$od_ship_price)),add=TRUE)
hist(log(OD.ShreddersAcc$od_ship_price),prob=T,xlab="log(Shipping Price)",main="Shipping Price distribution")
curve(dnorm(x,mean=mean(log(OD.ShreddersAcc$od_ship_price)),sd=sd(log(OD.ShreddersAcc$od_ship_price))),add=TRUE)

par(mfrow=c(1,2))
hist(OD.ShreddersAcc$od_discount,prob=T,xlab="Discount",main="Discount distribution")
curve(dnorm(x,mean=mean(OD.ShreddersAcc$od_discount),sd=sd(OD.ShreddersAcc$od_discount)),add=TRUE)
hist(log(subset(OD.ShreddersAcc,od_discount>0,od_discount))[,1],prob=T,
     xlab="log(Discount)",main="Discount distribution")
curve(dnorm(x,mean=mean(log(subset(OD.ShreddersAcc,od_discount>0,od_discount))[,1]),
            sd=sd(log(subset(OD.ShreddersAcc,od_discount>0,od_discount))[,1])),add=TRUE)

par(mfrow=c(1,2))
hist(OD.ShreddersAcc$od_final_price,prob=T,xlab="Final Price",main="OD Final price distribution")
curve(dnorm(x,mean=mean(OD.ShreddersAcc$od_final_price),sd=sd(OD.ShreddersAcc$od_final_price)),add=TRUE)
hist(log(OD.ShreddersAcc$od_final_price),prob=T,xlab="log(Final Price)",main="OD Final price distribution")
curve(dnorm(x,mean=mean(log(OD.ShreddersAcc$od_final_price)),sd=sd(log(OD.ShreddersAcc$od_final_price))),add=TRUE)


# Amazon
par(mfrow=c(1,2))
hist(OD.ShreddersAcc$Amz_reg_price,prob=T,xlab="Regular Price",main="Regular Price distribution")
curve(dnorm(x,mean=mean(OD.ShreddersAcc$Amz_reg_price,na.rm=T),sd=sd(OD.ShreddersAcc$Amz_reg_price,na.rm=T)),add=TRUE)
hist(log(OD.ShreddersAcc$Amz_reg_price),prob=T,xlab="log(Regular Price)",main="Regular Price distribution")
curve(dnorm(x,mean=mean(log(OD.ShreddersAcc$Amz_reg_price),na.rm=T),sd=sd(log(OD.ShreddersAcc$Amz_reg_price),na.rm=T)),add=TRUE)

par(mfrow=c(1,2))
hist(OD.ShreddersAcc$Amz_ship_price,prob=T,xlab="Shipping Price",main="Shipping Price distribution")
curve(dnorm(x,mean=mean(OD.ShreddersAcc$Amz_ship_price,na.rm=T),sd=sd(OD.ShreddersAcc$Amz_ship_price,na.rm=T)),add=TRUE)
hist(log(subset(OD.ShreddersAcc,Amz_ship_price>0,Amz_ship_price))[,1],prob=T,xlab="log(Shipping Price)",
     main="Shipping Price distribution")
curve(dnorm(x,mean=mean(log(subset(OD.ShreddersAcc,Amz_ship_price>0,Amz_ship_price))[,1],na.rm=T),
            sd=sd(log(subset(OD.ShreddersAcc,Amz_ship_price>0,Amz_ship_price))[,1],na.rm=T)),add=TRUE)

par(mfrow=c(1,2))
hist(OD.ShreddersAcc$Amz_discount,prob=T,xlab="Discount",main="Discount distribution")
curve(dnorm(x,mean=mean(OD.ShreddersAcc$Amz_discount,na.rm=T),sd=sd(OD.ShreddersAcc$Amz_discount,na.rm=T)),add=TRUE)
hist(log(subset(OD.ShreddersAcc,Amz_discount>0,Amz_discount))[,1],prob=T,
     xlab="log(Discount)",main="Discount distribution")
curve(dnorm(x,mean=mean(log(subset(OD.ShreddersAcc,Amz_discount>0,Amz_discount))[,1],na.rm=T),
            sd=sd(log(subset(OD.ShreddersAcc,Amz_discount>0,Amz_discount))[,1],na.rm=T)),add=TRUE)

par(mfrow=c(1,2))
hist(OD.ShreddersAcc$Amz_final_price,prob=T,xlab="Final Price",main="Amazon Final price distribution")
curve(dnorm(x,mean=mean(OD.ShreddersAcc$Amz_final_price,na.rm=T),sd=sd(OD.ShreddersAcc$Amz_final_price,na.rm=T)),add=TRUE)
hist(log(OD.ShreddersAcc$Amz_final_price),prob=T,xlab="log(Final Price)",main="Amazon Final price distribution")
curve(dnorm(x,mean=mean(log(OD.ShreddersAcc$Amz_final_price),na.rm=T),sd=sd(log(OD.ShreddersAcc$Amz_final_price),na.rm=T)),add=TRUE)


# Amazon Marketplace
par(mfrow=c(1,2))
hist(OD.ShreddersAcc$Amz_mkt_reg_price,prob=T,xlab="Regular Price",main="Regular Price distribution")
curve(dnorm(x,mean=mean(OD.ShreddersAcc$Amz_mkt_reg_price,na.rm=T),sd=sd(OD.ShreddersAcc$Amz_mkt_reg_price,na.rm=T)),add=TRUE)
hist(log(OD.ShreddersAcc$Amz_mkt_reg_price),prob=T,xlab="log(Regular Price)",main="Regular Price distribution")
curve(dnorm(x,mean=mean(log(OD.ShreddersAcc$Amz_mkt_reg_price),na.rm=T),sd=sd(log(OD.ShreddersAcc$Amz_mkt_reg_price),na.rm=T)),add=TRUE)

par(mfrow=c(1,2))
hist(OD.ShreddersAcc$Amz_mkt_ship_price,prob=T,xlab="Shipping Price",main="Shipping Price distribution")
curve(dnorm(x,mean=mean(OD.ShreddersAcc$Amz_mkt_ship_price,na.rm=T),sd=sd(OD.ShreddersAcc$Amz_mkt_ship_price,na.rm=T)),add=TRUE)
hist(log(subset(OD.ShreddersAcc,Amz_mkt_ship_price>0,Amz_mkt_ship_price))[,1],prob=T,xlab="log(Shipping Price)",
     main="Shipping Price distribution")
curve(dnorm(x,mean=mean(log(subset(OD.ShreddersAcc,Amz_mkt_ship_price>0,Amz_mkt_ship_price))[,1],na.rm=T),
            sd=sd(log(subset(OD.ShreddersAcc,Amz_mkt_ship_price>0,Amz_mkt_ship_price))[,1],na.rm=T)),add=TRUE)

par(mfrow=c(1,2))
hist(OD.ShreddersAcc$Amz_mkt_discount,prob=T,xlab="Discount",main="Discount distribution")
curve(dnorm(x,mean=mean(OD.ShreddersAcc$Amz_mkt_discount,na.rm=T),sd=sd(OD.ShreddersAcc$Amz_mkt_discount,na.rm=T)),add=TRUE)
hist(log(subset(OD.ShreddersAcc,Amz_mkt_discount>0,Amz_mkt_discount))[,1],prob=T,
     xlab="log(Discount)",main="Discount distribution")
curve(dnorm(x,mean=mean(log(subset(OD.ShreddersAcc,Amz_mkt_discount>0,Amz_mkt_discount))[,1],na.rm=T),
            sd=sd(log(subset(OD.ShreddersAcc,Amz_mkt_discount>0,Amz_mkt_discount))[,1],na.rm=T)),add=TRUE)

par(mfrow=c(1,2))
hist(OD.ShreddersAcc$Amz_mkt_final_price,prob=T,xlab="Final Price",main="Amazon Final price distribution")
curve(dnorm(x,mean=mean(OD.ShreddersAcc$Amz_mkt_final_price,na.rm=T),sd=sd(OD.ShreddersAcc$Amz_mkt_final_price,na.rm=T)),add=TRUE)
hist(log(OD.ShreddersAcc$Amz_mkt_final_price),prob=T,xlab="log(Final Price)",main="Amazon Final price distribution")
curve(dnorm(x,mean=mean(log(OD.ShreddersAcc$Amz_mkt_final_price),na.rm=T),sd=sd(log(OD.ShreddersAcc$Amz_mkt_final_price),na.rm=T)),add=TRUE)

# Staples
par(mfrow=c(1,2))
hist(OD.ShreddersAcc$staples_reg_price,prob=T,xlab="Regular Price",main="Regular Price distribution")
curve(dnorm(x,mean=mean(OD.ShreddersAcc$staples_reg_price,na.rm=T),sd=sd(OD.ShreddersAcc$staples_reg_price,na.rm=T)),add=TRUE)
hist(log(OD.ShreddersAcc$staples_reg_price),prob=T,xlab="log(Regular Price)",main="Regular Price distribution")
curve(dnorm(x,mean=mean(log(OD.ShreddersAcc$staples_reg_price),na.rm=T),sd=sd(log(OD.ShreddersAcc$staples_reg_price),na.rm=T)),add=TRUE)

par(mfrow=c(1,2))
hist(OD.ShreddersAcc$staples_ship_price,prob=T,xlab="Shipping Price",main="Shipping Price distribution")
curve(dnorm(x,mean=mean(OD.ShreddersAcc$staples_ship_price,na.rm=T),sd=sd(OD.ShreddersAcc$staples_ship_price,na.rm=T)),add=TRUE)
hist(log(subset(OD.ShreddersAcc,staples_ship_price>0,staples_ship_price))[,1],prob=T,xlab="log(Shipping Price)",
     main="Shipping Price distribution")
curve(dnorm(x,mean=mean(log(subset(OD.ShreddersAcc,staples_ship_price>0,staples_ship_price))[,1],na.rm=T),
            sd=sd(log(subset(OD.ShreddersAcc,staples_ship_price>0,staples_ship_price))[,1],na.rm=T)),add=TRUE)

par(mfrow=c(1,2))
hist(OD.ShreddersAcc$staples_discount,prob=T,xlab="Discount",main="Discount distribution")
curve(dnorm(x,mean=mean(OD.ShreddersAcc$staples_discount,na.rm=T),sd=sd(OD.ShreddersAcc$staples_discount,na.rm=T)),add=TRUE)
hist(log(subset(OD.ShreddersAcc,staples_discount>0,staples_discount))[,1],prob=T,
     xlab="log(Discount)",main="Discount distribution")
curve(dnorm(x,mean=mean(log(subset(OD.ShreddersAcc,staples_discount>0,staples_discount))[,1],na.rm=T),
            sd=sd(log(subset(OD.ShreddersAcc,staples_discount>0,staples_discount))[,1],na.rm=T)),add=TRUE)

par(mfrow=c(1,2))
hist(OD.ShreddersAcc$staples_final_price,prob=T,xlab="Final Price",main="Amazon Final price distribution")
curve(dnorm(x,mean=mean(OD.ShreddersAcc$staples_final_price,na.rm=T),sd=sd(OD.ShreddersAcc$staples_final_price,na.rm=T)),add=TRUE)
hist(log(OD.ShreddersAcc$staples_final_price),prob=T,xlab="log(Final Price)",main="Amazon Final price distribution")
curve(dnorm(x,mean=mean(log(OD.ShreddersAcc$staples_final_price),na.rm=T),sd=sd(log(OD.ShreddersAcc$staples_final_price),na.rm=T)),add=TRUE)

# Min Competitor Price
par(mfrow=c(1,2))
hist(OD.ShreddersAcc$min_comp_price,prob=T,xlab="Min Competitor Price",main="Min Competitor price distribution")
curve(dnorm(x,mean=mean(OD.ShreddersAcc$min_comp_price,na.rm=T),sd=sd(OD.ShreddersAcc$min_comp_price,na.rm=T)),add=TRUE)
hist(log(OD.ShreddersAcc$min_comp_price),prob=T,xlab="log(Min Competitor Price)",main="Min Competitor price distribution")
curve(dnorm(x,mean=mean(log(OD.ShreddersAcc$min_comp_price),na.rm=T),sd=sd(log(OD.ShreddersAcc$min_comp_price),na.rm=T)),add=TRUE)

#---------------------------------------------------------------------------------------------------------------
#                                       Correlataion Matrix
#---------------------------------------------------------------------------------------------------------------
toMatch_Shred <- c("units","final_price","pr_diff","pr_rat","min_comp_price","ln")
OD.ShreddersAcc[,grep(paste(toMatch_Shred,collapse="|"),names(OD.ShreddersAcc),value=T)]
OD.ShreddersAcc_new <- OD.ShreddersAcc[,grep(paste(toMatch_Shred,collapse="|"),names(OD.ShreddersAcc),value=T)]
OD.ShreddersAcc_new <- OD.ShreddersAcc_new[,-c(6,10,14,20)]

cor(OD.ShreddersAcc_new,use="na.or.complete")
write.csv(cor(OD.ShreddersAcc_new,use="na.or.complete"),
          "C:/Yashwanth/Pricing/1.OfficeDepot/4.EDA/1.Correlation/Cor_Shredders_Acc.csv")

#---------------------------------------------------------------------------------------------------------------
#                                       Frequency Distribution
#---------------------------------------------------------------------------------------------------------------

attach(OD.ShreddersAcc)
range(sales_units)
OD_Sales_Breaks <- seq(1,17,4)
OD_Sales <- data.frame(table(cut(sales_units,OD_Sales_Breaks,right=TRUE)))
colnames(OD_Sales) <- c("OD_Sales","Freq")
OD_Sales$Cum_Freq <- cumsum(OD_Sales$Freq)
OD_Sales$Percentage <- (OD_Sales$Freq/sum(OD_Sales$Freq))*100
OD_Sales$Cum_Percentage <- cumsum((OD_Sales$Freq/sum(OD_Sales$Freq))*100)

range(od_final_price)
OD_FP_Breaks <- seq(20,120,20)
OD_FP <- data.frame(table(cut(od_final_price,OD_FP_Breaks,right=TRUE)))
colnames(OD_FP) <- c("OD_FP","Freq")
OD_FP$Cum_Freq <- cumsum(OD_FP$Freq)
OD_FP$Percentage <- (OD_FP$Freq/sum(OD_FP$Freq))*100
OD_FP$Cum_Percentage <- cumsum((OD_FP$Freq/sum(OD_FP$Freq))*100)


range(Amz_final_price,na.rm=T)
AMZ_FP_Breaks <- seq(10,30,5)
AMZ_FP <- data.frame(table(cut(Amz_final_price,AMZ_FP_Breaks,right=TRUE)))
colnames(AMZ_FP) <- c("AMZ_FP","Freq")
AMZ_FP$Cum_Freq <- cumsum(AMZ_FP$Freq)
AMZ_FP$Percentage <- (AMZ_FP$Freq/sum(AMZ_FP$Freq))*100
AMZ_FP$Cum_Percentage <- cumsum((AMZ_FP$Freq/sum(AMZ_FP$Freq))*100)


range(Amz_mkt_final_price,na.rm=T)
AMZ_MP_FP_Breaks <- seq(10,60,5)
AMZ_MP_FP <- data.frame(table(cut(Amz_mkt_final_price,AMZ_MP_FP_Breaks,right=TRUE)))
colnames(AMZ_MP_FP) <- c("AMZ_MP_FP","Freq")
AMZ_MP_FP$Cum_Freq <- cumsum(AMZ_MP_FP$Freq)
AMZ_MP_FP$Percentage <- (AMZ_MP_FP$Freq/sum(AMZ_MP_FP$Freq))*100
AMZ_MP_FP$Cum_Percentage <- cumsum((AMZ_MP_FP$Freq/sum(AMZ_MP_FP$Freq))*100)

range(staples_final_price,na.rm=T)
Staples_FP_Breaks <- seq(20,70,5)
Staples_FP <- data.frame(table(cut(staples_final_price,Staples_FP_Breaks,right=TRUE)))
colnames(Staples_FP) <- c("Staples_FP","Freq")
Staples_FP$Cum_Freq <- cumsum(Staples_FP$Freq)
Staples_FP$Percentage <- (Staples_FP$Freq/sum(Staples_FP$Freq))*100
Staples_FP$Cum_Percentage <- cumsum((Staples_FP$Freq/sum(Staples_FP$Freq))*100)
detach(OD.ShreddersAcc)

write.csv(OD_Sales,"OD_SA_Sales_Tab.csv",row.names=F)
write.csv(OD_FP,"OD_SA_FP_Tab.csv",row.names=F);write.csv(AMZ_FP,"AMZ_SA_FP_Tab.csv",row.names=F)
write.csv(AMZ_MP_FP,"AMZ_SA_MP_FP_Tab.csv",row.names=F);write.csv(Staples_FP,"Staples_SA_FP_Tab.csv",row.names=F)


#---------------------------------------------------------------------------------------------------------------
#                                      Scatter Plot
#---------------------------------------------------------------------------------------------------------------

for(i in 1:nrow(OD.ShreddersAcc)){
  OD.ShreddersAcc$pr_diff_OD_final_pr[i] <- OD.ShreddersAcc$od_final_price[i]-OD.ShreddersAcc$od_final_price[i+1]
}
OD.ShreddersAcc$pr_diff_OD_final_pr[is.na(OD.ShreddersAcc$pr_diff_OD_final_pr)] <- OD.ShreddersAcc$pr_diff_OD_final_pr[i-1]

attach(OD.ShreddersAcc)
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

dev.off()
par(mfrow=c(1,2),cex.main=1.2)
plot(Amz_final_price,min_comp_price,xlab="Amazon Final Price",ylab="Min Competitor Price",main="Final Price v/s Min Competitor price")
abline(lm(Amz_final_price ~ min_comp_price))
plot(pr_rat_amz_final_pr,min_comp_price,xlab="Amazon Price Ratio",ylab="Min Competitor Price",main="Price Ratio v/s Min Competitor price")
abline(lm(pr_rat_amz_final_pr ~ min_comp_price))

par(mfrow=c(1,2),cex.main=1.2)
plot(Amz_mkt_final_price,min_comp_price,xlab="AmazonMP Final Price",ylab="Min Competitor Price",main="Final Price v/s Min Competitor price")
abline(lm(Amz_mkt_final_price ~ min_comp_price))
plot(pr_rat_amz_mkt_final_pr,min_comp_price,xlab="AmazonMP Price Ratio",ylab="Min Competitor Price",main="Price Ratio v/s Min Competitor price")
abline(lm(pr_rat_amz_mkt_final_pr ~ min_comp_price))

par(mfrow=c(1,2),cex.main=1.2)
plot(staples_final_price,min_comp_price,xlab="Staples Final Price",ylab="Min Competitor Price",main="Final Price v/s Min Competitor price")
abline(lm(staples_final_price ~ min_comp_price))
plot(pr_rat_stpl_final_pr,min_comp_price,xlab="AmazonMP Price Ratio",ylab="Min Competitor Price",main="Price Ratio v/s Min Competitor price")
abline(lm(pr_rat_stpl_final_pr ~ min_comp_price))


dev.off()
plot(min_comp_price,sales_units,xlab="Min Competitor Price",ylab="OD Sales",main="OD Sales v/s Min Competitor Price")
abline(lm(sales_units ~ min_comp_price))
detach(OD.ShreddersAcc)

#---------------------------------------------------------------------------------------------------------------
#                                      Box Plot
#---------------------------------------------------------------------------------------------------------------

dev.off()
attach(OD.ShreddersAcc)
boxplot(sales_units ~ as.factor(weekno),xlab="Weeks",ylab="OD Sales Units",main="Office Depot")
boxplot(od_final_price ~ as.factor(weekno),xlab="Weeks",ylab="OD Final Price",main="Office Depot")
boxplot(Amz_final_price ~ as.factor(weekno),xlab="Weeks",ylab="Amazon Final Price",main="Amazon")
boxplot(Amz_mkt_final_price ~ as.factor(weekno),xlab="Weeks",ylab="Amazon Marketplace Final Price",main="Amazon Marketplace")
boxplot(staples_final_price ~ as.factor(weekno),xlab="Weeks",ylab="Staples Final Price",main="Staples")
detach(OD.ShreddersAcc)

#---------------------------------------------------------------------------------------------------------------
#                                      Aggregate Data : Price
#---------------------------------------------------------------------------------------------------------------

OD.SKU_Summary_OD <- as.data.frame(as.list(aggregate(OD.Shredders_Bags$od_final_price ~ OD.Shredders_Bags$SKU,
                                                     FUN=function(x)c(min=min(x),max=max(x),len=length(x)))))
names(OD.SKU_Summary_OD) <- c("SKU","OD Min Finalprice","OD Max Finalprice","SKU frequency")

OD.SKU_Summary_Amz <- as.data.frame(as.list(aggregate(OD.Shredders_Bags$Amz_final_price ~ OD.Shredders_Bags$SKU,
                                                      FUN=function(x)c(min=min(x),max=max(x),len=length(x)))))
names(OD.SKU_Summary_Amz) <- c("SKU","Amazon Min Finalprice","Amazon Max Finalprice","SKU frequency")

OD.SKU_Summary_Amz_MP <- as.data.frame(as.list(aggregate(OD.Shredders_Bags$Amz_mkt_final_price ~ OD.Shredders_Bags$SKU,
                                                         FUN=function(x)c(min=min(x),max=max(x),len=length(x)))))
names(OD.SKU_Summary_Amz_MP) <- c("SKU","Amz MP Min Finalprice","Amz MP Max Finalprice","SKU frequency")

OD.SKU_Summary_Stap <- as.data.frame(as.list(aggregate(OD.Shredders_Bags$staples_final_price ~ OD.Shredders_Bags$SKU,
                                                       FUN=function(x)c(min=min(x),max=max(x),len=length(x)))))
names(OD.SKU_Summary_Stap) <- c("SKU","Staples Min Finalprice","Staples Max Finalprice","SKU frequency")


#---------------------------------------------------------------------------------------------------------------
#                                      Aggregate Data : Sales
#---------------------------------------------------------------------------------------------------------------

OD.SKU_Summary_OD <- as.data.frame(as.list(aggregate(OD.Shredders_Bags$ ~ OD.Shredders_Bags$SKU,
                                                     FUN=function(x)c(min=min(x),max=max(x),len=length(x)))))
names(OD.SKU_Summary_OD) <- c("SKU","OD Min Sales Units","OD Max Sales Units","SKU frequency")

OD.SKU_Summary_Amz <- as.data.frame(as.list(aggregate(OD.Shredders_Bags$ ~ OD.Shredders_Bags$SKU,
                                                      FUN=function(x)c(min=min(x),max=max(x),len=length(x)))))
names(OD.SKU_Summary_Amz) <- c("SKU","Amazon Min Sales Units","Amazon Max Sales Units","SKU frequency")

OD.SKU_Summary_Amz_MP <- as.data.frame(as.list(aggregate(OD.Shredders_Bags$ ~ OD.Shredders_Bags$SKU,
                                                         FUN=function(x)c(min=min(x),max=max(x),len=length(x)))))
names(OD.SKU_Summary_Amz_MP) <- c("SKU","Amz MP Min Sales Units","Amz MP Max Sales Units","SKU frequency")

OD.SKU_Summary_Stap <- as.data.frame(as.list(aggregate(OD.Shredders_Bags$ ~ OD.Shredders_Bags$SKU,
                                                       FUN=function(x)c(min=min(x),max=max(x),len=length(x)))))
names(OD.SKU_Summary_Stap) <- c("SKU","Staples Min Sales Units","Staples Max Sales Units","SKU frequency")

#---------------------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------------

