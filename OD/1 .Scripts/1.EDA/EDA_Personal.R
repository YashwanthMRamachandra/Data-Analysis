# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
#                                       || Exploratory Data Analysis ||
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------

setwd("C:/Yashwanth/Pricing/1.OfficeDepot/4.EDA/")

OD.Personal <- read.table(paste0(getwd()),"/Personal.csv", 
                            header = TRUE, sep = ",", quote = "\"", dec = "." ,fill=TRUE, comment.char="", as.is=TRUE)

OD.Personal <- read.table("C:/Yashwanth/Pricing/1.OfficeDepot/8.Data Output/Personal_Output.csv", 
                          header = TRUE, sep = ",", quote = "\"", dec = "." ,fill=TRUE, comment.char="", as.is=TRUE)
str(OD.Personal)
OD.Personal$SKU <- as.factor(OD.Personal$SKU);OD.Personal$weekno <- as.factor(OD.Personal$weekno)

# --------------------------------------------------------------------------------------------------------------
#                                       || Remove -ve Sales ||
# --------------------------------------------------------------------------------------------------------------

OD.Personal <- data.frame(subset(OD.Personal,sales_units>0,names(OD.Personal)),row.names=NULL)

#---------------------------------------------------------------------------------------------------------------
#                                       Descriptive Statistics
#---------------------------------------------------------------------------------------------------------------

summary(OD.Personal[sapply(OD.Personal,is.numeric)]) # Select only numeric columns

#---------------------------------------------------------------------------------------------------------------
#                                       Distribution of Variables : Histogram
#---------------------------------------------------------------------------------------------------------------
# Office Depot
par(mfrow=c(1,2))
hist(OD.Personal$sales_units,prob=T,xlab="Sales Units",main="Sales Units distribution")
curve(dnorm(x,mean=mean(OD.Personal$sales_units),sd=sd(OD.Personal$sales_units)),add=TRUE)
hist(log(OD.Personal$sales_units),prob=T,xlab="log(Sales Units)",main="Sales Units distribution")
curve(dnorm(x,mean=mean(log(OD.Personal$sales_units)),sd=sd(log(OD.Personal$sales_units))),add=TRUE)

par(mfrow=c(1,2))
hist(OD.Personal$od_reg_price,prob=T,xlab="Regular Price",main="Regular Price distribution")
curve(dnorm(x,mean=mean(OD.Personal$od_reg_price,na.rm=T),sd=sd(OD.Personal$od_reg_price,na.rm=T)),add=TRUE)
hist(log(OD.Personal$od_reg_price),prob=T,xlab="log(Regular Price)",main="Regular Price distribution")
curve(dnorm(x,mean=mean(log(OD.Personal$od_reg_price),na.rm=T),sd=sd(log(OD.Personal$od_reg_price),na.rm=T)),add=TRUE)

par(mfrow=c(1,2))
hist(OD.Personal$od_ship_price,prob=T,xlab="Shipping Price",main="Shipping Price distribution")
curve(dnorm(x,mean=mean(OD.Personal$od_ship_price),sd=sd(OD.Personal$od_ship_price)),add=TRUE)
hist(log(OD.Personal$od_ship_price),prob=T,xlab="log(Shipping Price)",main="Shipping Price distribution")
curve(dnorm(x,mean=mean(log(OD.Personal$od_ship_price)),sd=sd(log(OD.Personal$od_ship_price))),add=TRUE)

par(mfrow=c(1,2))
hist(OD.Personal$od_discount,prob=T,xlab="Discount",main="Discount distribution")
curve(dnorm(x,mean=mean(OD.Personal$od_discount,na.rm=T),sd=sd(OD.Personal$od_discount,na.rm=T)),add=TRUE)
hist(log(subset(OD.Personal,od_discount>0,od_discount))[,1],prob=T,
     xlab="log(Discount)",main="Discount distribution")
curve(dnorm(x,mean=mean(log(subset(OD.Personal,od_discount>0,od_discount))[,1]),
            sd=sd(log(subset(OD.Personal,od_discount>0,od_discount))[,1])),add=TRUE)

par(mfrow=c(1,2))
hist(OD.Personal$od_final_price,prob=T,xlab="Final Price",main="OD Final price distribution")
curve(dnorm(x,mean=mean(OD.Personal$od_final_price),sd=sd(OD.Personal$od_final_price)),add=TRUE)
hist(log(OD.Personal$od_final_price),prob=T,xlab="log(Final Price)",main="OD Final price distribution")
curve(dnorm(x,mean=mean(log(OD.Personal$od_final_price)),sd=sd(log(OD.Personal$od_final_price))),add=TRUE)


# Amazon
par(mfrow=c(1,2))
hist(OD.Personal$Amz_reg_price,prob=T,xlab="Regular Price",main="Regular Price distribution")
curve(dnorm(x,mean=mean(OD.Personal$Amz_reg_price,na.rm=T),sd=sd(OD.Personal$Amz_reg_price,na.rm=T)),add=TRUE)
hist(log(OD.Personal$Amz_reg_price),prob=T,xlab="log(Regular Price)",main="Regular Price distribution")
curve(dnorm(x,mean=mean(log(OD.Personal$Amz_reg_price),na.rm=T),sd=sd(log(OD.Personal$Amz_reg_price),na.rm=T)),add=TRUE)

par(mfrow=c(1,2))
hist(OD.Personal$Amz_ship_price,prob=T,xlab="Shipping Price",main="Shipping Price distribution")
curve(dnorm(x,mean=mean(OD.Personal$Amz_ship_price,na.rm=T),sd=sd(OD.Personal$Amz_ship_price,na.rm=T)),add=TRUE)
hist(log(subset(OD.Personal,Amz_ship_price>0,Amz_ship_price))[,1],prob=T,xlab="log(Shipping Price)",
     main="Shipping Price distribution")
curve(dnorm(x,mean=mean(log(subset(OD.Personal,Amz_ship_price>0,Amz_ship_price))[,1],na.rm=T),
            sd=sd(log(subset(OD.Personal,Amz_ship_price>0,Amz_ship_price))[,1],na.rm=T)),add=TRUE)

par(mfrow=c(1,2))
hist(OD.Personal$Amz_discount,prob=T,xlab="Discount",main="Discount distribution")
curve(dnorm(x,mean=mean(OD.Personal$Amz_discount,na.rm=T),sd=sd(OD.Personal$Amz_discount,na.rm=T)),add=TRUE)
hist(log(subset(OD.Personal,Amz_discount>0,Amz_discount))[,1],prob=T,
     xlab="log(Discount)",main="Discount distribution")
curve(dnorm(x,mean=mean(log(subset(OD.Personal,Amz_discount>0,Amz_discount))[,1],na.rm=T),
            sd=sd(log(subset(OD.Personal,Amz_discount>0,Amz_discount))[,1],na.rm=T)),add=TRUE)

par(mfrow=c(1,2))
hist(OD.Personal$Amz_final_price,prob=T,xlab="Final Price",main="Amazon Final price distribution")
curve(dnorm(x,mean=mean(OD.Personal$Amz_final_price,na.rm=T),sd=sd(OD.Personal$Amz_final_price,na.rm=T)),add=TRUE)
hist(log(OD.Personal$Amz_final_price),prob=T,xlab="log(Final Price)",main="Amazon Final price distribution")
curve(dnorm(x,mean=mean(log(OD.Personal$Amz_final_price),na.rm=T),sd=sd(log(OD.Personal$Amz_final_price),na.rm=T)),add=TRUE)


# Amazon Marketplace
par(mfrow=c(1,2))
hist(OD.Personal$Amz_mkt_reg_price,prob=T,xlab="Regular Price",main="Regular Price distribution")
curve(dnorm(x,mean=mean(OD.Personal$Amz_mkt_reg_price,na.rm=T),sd=sd(OD.Personal$Amz_mkt_reg_price,na.rm=T)),add=TRUE)
hist(log(OD.Personal$Amz_mkt_reg_price),prob=T,xlab="log(Regular Price)",main="Regular Price distribution")
curve(dnorm(x,mean=mean(log(OD.Personal$Amz_mkt_reg_price),na.rm=T),sd=sd(log(OD.Personal$Amz_mkt_reg_price),na.rm=T)),add=TRUE)

par(mfrow=c(1,2))
hist(OD.Personal$Amz_mkt_ship_price,prob=T,xlab="Shipping Price",main="Shipping Price distribution")
curve(dnorm(x,mean=mean(OD.Personal$Amz_mkt_ship_price,na.rm=T),sd=sd(OD.Personal$Amz_mkt_ship_price,na.rm=T)),add=TRUE)
hist(log(subset(OD.Personal,Amz_mkt_ship_price>0,Amz_mkt_ship_price))[,1],prob=T,xlab="log(Shipping Price)",
     main="Shipping Price distribution")
curve(dnorm(x,mean=mean(log(subset(OD.Personal,Amz_mkt_ship_price>0,Amz_mkt_ship_price))[,1],na.rm=T),
            sd=sd(log(subset(OD.Personal,Amz_mkt_ship_price>0,Amz_mkt_ship_price))[,1],na.rm=T)),add=TRUE)

par(mfrow=c(1,2))
hist(OD.Personal$Amz_mkt_discount,prob=T,xlab="Discount",main="Discount distribution")
curve(dnorm(x,mean=mean(OD.Personal$Amz_mkt_discount,na.rm=T),sd=sd(OD.Personal$Amz_mkt_discount,na.rm=T)),add=TRUE)
hist(log(subset(OD.Personal,Amz_mkt_discount>0,Amz_mkt_discount))[,1],prob=T,
     xlab="log(Discount)",main="Discount distribution")
curve(dnorm(x,mean=mean(log(subset(OD.Personal,Amz_mkt_discount>0,Amz_mkt_discount))[,1],na.rm=T),
            sd=sd(log(subset(OD.Personal,Amz_mkt_discount>0,Amz_mkt_discount))[,1],na.rm=T)),add=TRUE)

par(mfrow=c(1,2))
hist(OD.Personal$Amz_mkt_final_price,prob=T,xlab="Final Price",main="Amazon Final price distribution")
curve(dnorm(x,mean=mean(OD.Personal$Amz_mkt_final_price,na.rm=T),sd=sd(OD.Personal$Amz_mkt_final_price,na.rm=T)),add=TRUE)
hist(log(OD.Personal$Amz_mkt_final_price),prob=T,xlab="log(Final Price)",main="Amazon Final price distribution")
curve(dnorm(x,mean=mean(log(OD.Personal$Amz_mkt_final_price),na.rm=T),sd=sd(log(OD.Personal$Amz_mkt_final_price),na.rm=T)),add=TRUE)

# Bestbuy
par(mfrow=c(1,2))
hist(OD.Personal$bb_reg_price,prob=T,xlab="Regular Price",main="Regular Price distribution")
curve(dnorm(x,mean=mean(OD.Personal$bb_reg_price,na.rm=T),sd=sd(OD.Personal$bb_reg_price,na.rm=T)),add=TRUE)
hist(log(OD.Personal$bb_reg_price),prob=T,xlab="log(Regular Price)",main="Regular Price distribution")
curve(dnorm(x,mean=mean(log(OD.Personal$bb_reg_price),na.rm=T),sd=sd(log(OD.Personal$bb_reg_price),na.rm=T)),add=TRUE)

par(mfrow=c(1,2))
hist(OD.Personal$bb_ship_price,prob=T,xlab="Shipping Price",main="Shipping Price distribution")
curve(dnorm(x,mean=mean(OD.Personal$bb_ship_price,na.rm=T),sd=sd(OD.Personal$bb_ship_price,na.rm=T)),add=TRUE)
hist(log(subset(OD.Personal,bb_ship_price>0,bb_ship_price))[,1],prob=T,xlab="log(Shipping Price)",
     main="Shipping Price distribution")
curve(dnorm(x,mean=mean(log(subset(OD.Personal,bb_ship_price>0,bb_ship_price))[,1],na.rm=T),
            sd=sd(log(subset(OD.Personal,bb_ship_price>0,bb_ship_price))[,1],na.rm=T)),add=TRUE)

par(mfrow=c(1,2))
hist(OD.Personal$bb_discount,prob=T,xlab="Discount",main="Discount distribution")
curve(dnorm(x,mean=mean(OD.Personal$bb_discount,na.rm=T),sd=sd(OD.Personal$bb_discount,na.rm=T)),add=TRUE)
hist(log(subset(OD.Personal,bb_discount>0,bb_discount))[,1],prob=T,
     xlab="log(Discount)",main="Discount distribution")
curve(dnorm(x,mean=mean(log(subset(OD.Personal,bb_discount>0,bb_discount))[,1],na.rm=T),
            sd=sd(log(subset(OD.Personal,bb_discount>0,bb_discount))[,1],na.rm=T)),add=TRUE)

par(mfrow=c(1,2))
hist(OD.Personal$bb_final_price,prob=T,xlab="Final Price",main="Amazon Final price distribution")
curve(dnorm(x,mean=mean(OD.Personal$bb_final_price,na.rm=T),sd=sd(OD.Personal$bb_final_price,na.rm=T)),add=TRUE)
hist(log(OD.Personal$bb_final_price),prob=T,xlab="log(Final Price)",main="Amazon Final price distribution")
curve(dnorm(x,mean=mean(log(OD.Personal$bb_final_price),na.rm=T),sd=sd(log(OD.Personal$bb_final_price),na.rm=T)),add=TRUE)

# Staples
par(mfrow=c(1,2))
hist(OD.Personal$staples_reg_price,prob=T,xlab="Regular Price",main="Regular Price distribution")
curve(dnorm(x,mean=mean(OD.Personal$staples_reg_price,na.rm=T),sd=sd(OD.Personal$staples_reg_price,na.rm=T)),add=TRUE)
hist(log(OD.Personal$staples_reg_price),prob=T,xlab="log(Regular Price)",main="Regular Price distribution")
curve(dnorm(x,mean=mean(log(OD.Personal$staples_reg_price),na.rm=T),sd=sd(log(OD.Personal$staples_reg_price),na.rm=T)),add=TRUE)

par(mfrow=c(1,2))
hist(OD.Personal$staples_ship_price,prob=T,xlab="Shipping Price",main="Shipping Price distribution")
curve(dnorm(x,mean=mean(OD.Personal$staples_ship_price,na.rm=T),sd=sd(OD.Personal$staples_ship_price,na.rm=T)),add=TRUE)
hist(log(subset(OD.Personal,staples_ship_price>0,staples_ship_price))[,1],prob=T,xlab="log(Shipping Price)",
     main="Shipping Price distribution")
curve(dnorm(x,mean=mean(log(subset(OD.Personal,staples_ship_price>0,staples_ship_price))[,1],na.rm=T),
            sd=sd(log(subset(OD.Personal,staples_ship_price>0,staples_ship_price))[,1],na.rm=T)),add=TRUE)


par(mfrow=c(1,2))
hist(OD.Personal$staples_discount,prob=T,xlab="Discount",main="Discount distribution")
curve(dnorm(x,mean=mean(OD.Personal$staples_discount,na.rm=T),sd=sd(OD.Personal$staples_discount,na.rm=T)),add=TRUE)
hist(log(subset(OD.Personal,staples_discount>0,staples_discount))[,1],prob=T,
     xlab="log(Discount)",main="Discount distribution")
curve(dnorm(x,mean=mean(log(subset(OD.Personal,staples_discount>0,staples_discount))[,1],na.rm=T),
            sd=sd(log(subset(OD.Personal,staples_discount>0,staples_discount))[,1],na.rm=T)),add=TRUE)

par(mfrow=c(1,2))
hist(OD.Personal$staples_final_price,prob=T,xlab="Final Price",main="Staples Final price distribution")
curve(dnorm(x,mean=mean(OD.Personal$staples_final_price,na.rm=T),sd=sd(OD.Personal$staples_final_price,na.rm=T)),add=TRUE)
hist(log(OD.Personal$staples_final_price),prob=T,xlab="log(Final Price)",main="Staples Final price distribution")
curve(dnorm(x,mean=mean(log(OD.Personal$staples_final_price),na.rm=T),sd=sd(log(OD.Personal$staples_final_price),na.rm=T)),add=TRUE)


# Walmart
par(mfrow=c(1,2))
hist(OD.Personal$Walmart_reg_price,prob=T,xlab="Regular Price",main="Regular Price distribution")
curve(dnorm(x,mean=mean(OD.Personal$Walmart_reg_price,na.rm=T),sd=sd(OD.Personal$Walmart_reg_price,na.rm=T)),add=TRUE)
hist(log(OD.Personal$Walmart_reg_price),prob=T,xlab="log(Regular Price)",main="Regular Price distribution")
curve(dnorm(x,mean=mean(log(OD.Personal$Walmart_reg_price),na.rm=T),sd=sd(log(OD.Personal$Walmart_reg_price),na.rm=T)),add=TRUE)

par(mfrow=c(1,2))
hist(OD.Personal$Walmart_ship_price,prob=T,xlab="Shipping Price",main="Shipping Price distribution")
curve(dnorm(x,mean=mean(OD.Personal$Walmart_ship_price,na.rm=T),sd=sd(OD.Personal$Walmart_ship_price,na.rm=T)),add=TRUE)
hist(log(subset(OD.Personal,Walmart_ship_price>0,Walmart_ship_price))[,1],prob=T,xlab="log(Shipping Price)",
     main="Shipping Price distribution")
curve(dnorm(x,mean=mean(log(subset(OD.Personal,Walmart_ship_price>0,Walmart_ship_price))[,1],na.rm=T),
            sd=sd(log(subset(OD.Personal,Walmart_ship_price>0,Walmart_ship_price))[,1],na.rm=T)),add=TRUE)


par(mfrow=c(1,2))
hist(OD.Personal$Walmart_discount,prob=T,xlab="Discount",main="Discount distribution")
curve(dnorm(x,mean=mean(OD.Personal$Walmart_discount,na.rm=T),sd=sd(OD.Personal$Walmart_discount,na.rm=T)),add=TRUE)
hist(log(subset(OD.Personal,Walmart_discount>0,Walmart_discount))[,1],prob=T,
     xlab="log(Discount)",main="Discount distribution")
curve(dnorm(x,mean=mean(log(subset(OD.Personal,Walmart_discount>0,Walmart_discount))[,1],na.rm=T),
            sd=sd(log(subset(OD.Personal,Walmart_discount>0,Walmart_discount))[,1],na.rm=T)),add=TRUE)

par(mfrow=c(1,2))
hist(OD.Personal$Walmart_final_price,prob=T,xlab="Final Price",main="Walmart Final price distribution")
curve(dnorm(x,mean=mean(OD.Personal$Walmart_final_price,na.rm=T),sd=sd(OD.Personal$Walmart_final_price,na.rm=T)),add=TRUE)
hist(log(OD.Personal$Walmart_final_price),prob=T,xlab="log(Final Price)",main="Walmart Final price distribution")
curve(dnorm(x,mean=mean(log(OD.Personal$Walmart_final_price),na.rm=T),sd=sd(log(OD.Personal$Walmart_final_price),na.rm=T)),add=TRUE)

# Min Competitor Price
par(mfrow=c(1,2))
hist(OD.Personal$min_comp_price,prob=T,xlab="Min Competitor Price",main="Min Competitor price distribution")
curve(dnorm(x,mean=mean(OD.Personal$min_comp_price,na.rm=T),sd=sd(OD.Personal$min_comp_price,na.rm=T)),add=TRUE)
hist(log(OD.Personal$min_comp_price),prob=T,xlab="log(Min Competitor Price)",main="Min Competitor price distribution")
curve(dnorm(x,mean=mean(log(OD.Personal$min_comp_price),na.rm=T),sd=sd(log(OD.Personal$min_comp_price),na.rm=T)),add=TRUE)

#---------------------------------------------------------------------------------------------------------------
#                                       Correlataion Matrix
#---------------------------------------------------------------------------------------------------------------

toMatch_Per <- c("units","final_price","reg_price","pr_diff","pr_rat","min_comp_price","ln")
OD.Personal[,grep(paste(toMatch_Per,collapse="|"),names(OD.Personal),value=T)]
cor(OD.Personal[,grep(paste(toMatch_Per,collapse="|"),names(OD.Personal),value=T)],use="na.or.complete")
write.csv(cor(OD.Personal[,grep(paste(toMatch_Per,collapse="|"),names(OD.Personal),value=T)],use="na.or.complete"),
          "C:/Yashwanth/Pricing/1.OfficeDepot/4.EDA/1.Correlation/Cor_Personal.csv")

#---------------------------------------------------------------------------------------------------------------
#                                       Frequency Distribution
#---------------------------------------------------------------------------------------------------------------

attach(OD.Personal)
range(sales_units)
OD_Sales_Breaks <- seq(1,130,10)
OD_Sales <- data.frame(table(cut(sales_units,OD_Sales_Breaks,right=TRUE)))
colnames(OD_Sales) <- c("OD_Sales","Freq")
OD_Sales$Cum_Freq <- cumsum(OD_Sales$Freq)
OD_Sales$Percentage <- (OD_Sales$Freq/sum(OD_Sales$Freq))*100
OD_Sales$Cum_Percentage <- cumsum((OD_Sales$Freq/sum(OD_Sales$Freq))*100)

range(od_final_price)
OD_FP_Breaks <- seq(30,750,50)
OD_FP <- data.frame(table(cut(od_final_price,OD_FP_Breaks,right=TRUE)))
colnames(OD_FP) <- c("OD_FP","Freq")
OD_FP$Cum_Freq <- cumsum(OD_FP$Freq)
OD_FP$Percentage <- (OD_FP$Freq/sum(OD_FP$Freq))*100
OD_FP$Cum_Percentage <- cumsum((OD_FP$Freq/sum(OD_FP$Freq))*100)


range(Amz_final_price,na.rm=T)
AMZ_FP_Breaks <- seq(50,300,50)
AMZ_FP <- data.frame(table(cut(Amz_final_price,AMZ_FP_Breaks,right=TRUE)))
colnames(AMZ_FP) <- c("AMZ_FP","Freq")
AMZ_FP$Cum_Freq <- cumsum(AMZ_FP$Freq)
AMZ_FP$Percentage <- (AMZ_FP$Freq/sum(AMZ_FP$Freq))*100
AMZ_FP$Cum_Percentage <- cumsum((AMZ_FP$Freq/sum(AMZ_FP$Freq))*100)


range(Amz_mkt_final_price,na.rm=T)
AMZ_MP_FP_Breaks <- seq(50,300,50)
AMZ_MP_FP <- data.frame(table(cut(Amz_mkt_final_price,AMZ_MP_FP_Breaks,right=TRUE)))
colnames(AMZ_MP_FP) <- c("AMZ_MP_FP","Freq")
AMZ_MP_FP$Cum_Freq <- cumsum(AMZ_MP_FP$Freq)
AMZ_MP_FP$Percentage <- (AMZ_MP_FP$Freq/sum(AMZ_MP_FP$Freq))*100
AMZ_MP_FP$Cum_Percentage <- cumsum((AMZ_MP_FP$Freq/sum(AMZ_MP_FP$Freq))*100)

range(bb_final_price,na.rm=T)
bb_FP_Breaks <- seq(50,250,50)
bb_FP <- data.frame(table(cut(bb_final_price,bb_FP_Breaks,right=TRUE)))
colnames(bb_FP) <- c("bb_FP","Freq")
bb_FP$Cum_Freq <- cumsum(bb_FP$Freq)
bb_FP$Percentage <- (bb_FP$Freq/sum(bb_FP$Freq))*100
bb_FP$Cum_Percentage <- cumsum((bb_FP$Freq/sum(bb_FP$Freq))*100)

range(staples_final_price,na.rm=T)
Staples_FP_Breaks <- seq(50,350,50)
Staples_FP <- data.frame(table(cut(staples_final_price,Staples_FP_Breaks,right=TRUE)))
colnames(Staples_FP) <- c("Staples_FP","Freq")
Staples_FP$Cum_Freq <- cumsum(Staples_FP$Freq)
Staples_FP$Percentage <- (Staples_FP$Freq/sum(Staples_FP$Freq))*100
Staples_FP$Cum_Percentage <- cumsum((Staples_FP$Freq/sum(Staples_FP$Freq))*100)
detach(OD.Personal)

write.csv(OD_Sales,"OD_Per_Sales_Tab.csv",row.names=F)
write.csv(OD_FP,"OD_Per_FP_Tab.csv",row.names=F);write.csv(AMZ_FP,"AMZ_Per_FP_Tab.csv",row.names=F)
write.csv(AMZ_MP_FP,"AMZ_Per_MP_FP_Tab.csv",row.names=F);write.csv(Staples_FP,"Staples_Per_FP_Tab.csv",row.names=F)
write.csv(bb_FP,"bb_Per_FP_Tab.csv",row.names=F)

#---------------------------------------------------------------------------------------------------------------
#                                      Scatter Plot
#---------------------------------------------------------------------------------------------------------------

for(i in 1:nrow(OD.Personal)){
  OD.Personal$pr_diff_OD_final_pr[i] <- OD.Personal$od_final_price[i]-OD.Personal$od_final_price[i+1]
}
OD.Personal$pr_diff_OD_final_pr[is.na(OD.Personal$pr_diff_OD_final_pr)] <- OD.Personal$pr_diff_OD_final_pr[i-1]

attach(OD.Personal)
par(mfrow=c(1,2),cex.main=1.2)
plot(od_final_price,sales_units,xlab="OD Final Price",ylab="OD Sales",main="OD Sales v/s Price")
abline(lm(sales_units ~ od_final_price))
plot(pr_diff_OD_final_pr,sales_units,xlab="OD Final Price Diff",ylab="OD Sales ",main="OD Sales v/s Price Diff")
abline(lm(sales_units ~ pr_diff_OD_final_pr))

par(mfrow=c(1,2),cex.main=1.2)
plot(Amz_final_price,sales_units,xlab="Amazon Final Price",ylab="OD Sales",main="OD Sales v/s Price")
abline(lm(sales_units ~ Amz_final_price))
plot(pr_diff_amz_final_pr,sales_units,xlab="Amzazon Final Price Diff",ylab="OD Sales ",main="OD Sales v/s Amazon Price Diff")
abline(lm(sales_units ~ pr_diff_amz_final_pr))

par(mfrow=c(1,2),cex.main=1.2)
plot(Amz_mkt_final_price,sales_units,xlab="Amazon Marketplace Final Price",ylab="OD Sales",main="OD Sales v/s Amazon Marketplace Price")
abline(lm(sales_units ~ Amz_mkt_final_price))
plot(pr_diff_amz_mkt_final_pr,sales_units,xlab="Amazon Marketplace Final Price Diff",ylab="OD Sales ",main="OD Sales v/s Amazon Marketplace Price Diff")
abline(lm(sales_units ~ pr_diff_amz_mkt_final_pr))

par(mfrow=c(1,2),cex.main=1.2)
plot(bb_final_price,sales_units,xlab="BestBuy Final Price",ylab="OD Sales",main="OD Sales v/s BestBuy Price")
abline(lm(sales_units ~ bb_final_price))
plot(pr_diff_bb_final_pr,sales_units,xlab="BestBuy Final Price Diff",ylab="OD Sales ",main="OD Sales v/s Bestbuy Price Diff")
abline(lm(sales_units ~ pr_diff_bb_final_pr))

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

par(mfrow=c(1,2),cex.main=1.2)
plot(bb_final_price,min_comp_price,xlab="Bestbuy Final Price",ylab="Min Competitor Price",main="Final Price v/s Min Competitor price")
abline(lm(bb_final_price ~ min_comp_price))
plot(pr_rat_bb_final_pr,min_comp_price,xlab="Bestbuy Price Ratio",ylab="Min Competitor Price",main="Price Ratio v/s Min Competitor price")
abline(lm(pr_rat_bb_final_pr ~ min_comp_price))

par(mfrow=c(1,2),cex.main=1.2)
plot(Walmart_final_price,min_comp_price,xlab="Walmart Final Price",ylab="Min Competitor Price",main="Final Price v/s Min Competitor price")
abline(lm(Walmart_final_price ~ min_comp_price))
plot(pr_rat_wal_final_pr,min_comp_price,xlab="Walmart Price Ratio",ylab="Min Competitor Price",main="Price Ratio v/s Min Competitor price")
abline(lm(pr_rat_wal_final_pr ~ min_comp_price))

dev.off()
plot(min_comp_price,sales_units,xlab="Min Competitor Price",ylab="OD Sales",main="OD Sales v/s Min Competitor Price")
abline(lm(sales_units ~ min_comp_price))
detach(OD.Personal)

#---------------------------------------------------------------------------------------------------------------
#                                      Box Plot
#---------------------------------------------------------------------------------------------------------------

dev.off()
attach(OD.Personal)
boxplot(sales_units ~ as.factor(weekno),xlab="Weeks",ylab="Sales Units",main="Office Depot")
boxplot(od_final_price ~ as.factor(weekno),xlab="Weeks",ylab="OD Final Price",main="Office Depot")
boxplot(Amz_final_price ~ as.factor(weekno),xlab="Weeks",ylab="Amazon Final Price",main="Amazon")
boxplot(Amz_mkt_final_price ~ as.factor(weekno),xlab="Weeks",ylab="Amazon Marketplace Final Price",main="Amazon Marketplace")
boxplot(bb_final_price ~ as.factor(weekno),xlab="Weeks",ylab="Bestbuy Final Price",main="Bestbuy")
boxplot(staples_final_price ~ as.factor(weekno),xlab="Weeks",ylab="Staples Final Price",main="Staples")
detach(OD.Personal)

#---------------------------------------------------------------------------------------------------------------
#                                      Aggregate Data : Price
#---------------------------------------------------------------------------------------------------------------

OD.SKU_Summary_OD_FP <- as.data.frame(as.list(aggregate(OD.Personal$od_final_price ~ OD.Personal$SKU,
                                                     FUN=function(x)c(min=min(x),max=max(x),len=length(x)))))
names(OD.SKU_Summary_OD_FP) <- c("SKU","OD Min Finalprice","OD Max Finalprice","SKU frequency")

OD.SKU_Summary_Amz_FP <- as.data.frame(as.list(aggregate(OD.Personal$Amz_final_price ~ OD.Personal$SKU,
                                                      FUN=function(x)c(min=min(x),max=max(x),len=length(x)))))
names(OD.SKU_Summary_Amz_FP) <- c("SKU","Amazon Min Finalprice","Amazon Max Finalprice","SKU frequency")

OD.SKU_Summary_Amz_MP_FP <- as.data.frame(as.list(aggregate(OD.Personal$Amz_mkt_final_price ~ OD.Personal$SKU,
                                                         FUN=function(x)c(min=min(x),max=max(x),len=length(x)))))
names(OD.SKU_Summary_Amz_MP_FP) <- c("SKU","AmzMP Min Finalprice","AmzMP Max Finalprice","SKU frequency")

OD.SKU_Summary_Stap_FP <- as.data.frame(as.list(aggregate(OD.Personal$staples_final_price ~ OD.Personal$SKU,
                                                       FUN=function(x)c(min=min(x),max=max(x),len=length(x)))))
names(OD.SKU_Summary_Stap_FP) <- c("SKU","Staples Min Finalprice","Staples Max Finalprice","SKU frequency")

#---------------------------------------------------------------------------------------------------------------
#                                      Aggregate Data : Sales
#---------------------------------------------------------------------------------------------------------------

OD.SKU_Summary_OD_SU <- as.data.frame(as.list(aggregate(OD.Personal$ ~ OD.Personal$SKU,
                                                     FUN=function(x)c(min=min(x),max=max(x),len=length(x)))))
names(OD.SKU_Summary_OD_SU) <- c("SKU","OD Min Sales Units","OD Max Sales Units","SKU frequency")

OD.SKU_Summary_Amz_SU <- as.data.frame(as.list(aggregate(OD.Personal$ ~ OD.Personal$SKU,
                                                      FUN=function(x)c(min=min(x),max=max(x),len=length(x)))))
names(OD.SKU_Summary_Amz_SU) <- c("SKU","Amazon Min Sales Units","Amazon Max Sales Units","SKU frequency")

OD.SKU_Summary_Amz_MP_SU <- as.data.frame(as.list(aggregate(OD.Personal$ ~ OD.Personal$SKU,
                                                         FUN=function(x)c(min=min(x),max=max(x),len=length(x)))))
names(OD.SKU_Summary_Amz_MP_SU) <- c("SKU","AmzMP Min Sales Units","AmzMP Max Sales Units","SKU frequency")

OD.SKU_Summary_Stap_SU <- as.data.frame(as.list(aggregate(OD.Personal$ ~ OD.Personal$SKU,
                                                       FUN=function(x)c(min=min(x),max=max(x),len=length(x)))))
names(OD.SKU_Summary_Stap_SU) <- c("SKU","Staples Min Sales Units","Staples Max Sales Units","SKU frequency")

#---------------------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------------

