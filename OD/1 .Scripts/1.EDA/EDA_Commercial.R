# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
#                                       || Exploratory Data Analysis ||
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------

setwd("C:/Yashwanth/Pricing/1.OfficeDepot/4.EDA/")

OD.Commercial <- read.table("Commercial.csv", 
                  header = TRUE, sep = ",", quote = "\"", dec = "." ,fill=TRUE, comment.char="", as.is=TRUE)

OD.Commercial <- read.table("C:/Yashwanth/Pricing/1.OfficeDepot/8.Data Output/1.Commercial/Commercial_Output.csv", 
                            header = TRUE, sep = ",", quote = "\"", dec = "." ,fill=TRUE, comment.char="", as.is=TRUE)

str(OD.Commercial)
OD.Commercial$SKU <- as.factor(OD.Commercial$SKU);OD.Commercial$weekno <- as.factor(OD.Commercial$weekno)

# --------------------------------------------------------------------------------------------------------------
#                                       || Remove -ve Sales ||
# --------------------------------------------------------------------------------------------------------------

OD.Commercial <- data.frame(subset(OD.Commercial,sales_units>0,names(OD.Commercial)),row.names=NULL)

# --------------------------------------------------------------------------------------------------------------
#                                       || Sales Trend ||
# --------------------------------------------------------------------------------------------------------------

plot(OD.Commercial$weekno,OD.Commercial$sales_units,type="l")

#---------------------------------------------------------------------------------------------------------------
#                                       Descriptive Statistics
#---------------------------------------------------------------------------------------------------------------

summary(OD.Commercial[sapply(OD.Commercial,is.numeric)]) # Select only numeric columns

#---------------------------------------------------------------------------------------------------------------
#                                       Distribution of Variables : Histogram
#---------------------------------------------------------------------------------------------------------------
# Office Depot
par(mfrow=c(1,2))
hist(OD.Commercial$sales_units,prob=T,xlab="Sales Units",main="Sales Units distribution")
curve(dnorm(x,mean=mean(OD.Commercial$sales_units),sd=sd(OD.Commercial$sales_units)),add=TRUE)
hist(log(OD.Commercial$sales_units),prob=T,xlab="log(Sales Units)",main="Sales Units distribution")
curve(dnorm(x,mean=mean(log(OD.Commercial$sales_units)),sd=sd(log(OD.Commercial$sales_units))),add=TRUE)

par(mfrow=c(1,2))
hist(OD.Commercial$od_reg_price,prob=T,xlab="Regular Price",main="Regular Price distribution")
curve(dnorm(x,mean=mean(OD.Commercial$od_reg_price),sd=sd(OD.Commercial$od_reg_price)),add=TRUE)
hist(log(OD.Commercial$od_reg_price),prob=T,xlab="log(Regular Price)",main="Regular Price distribution")
curve(dnorm(x,mean=mean(log(OD.Commercial$od_reg_price)),sd=sd(log(OD.Commercial$od_reg_price))),add=TRUE)

par(mfrow=c(1,2))
hist(OD.Commercial$od_ship_price,prob=T,xlab="Shipping Price",main="Shipping Price distribution")
curve(dnorm(x,mean=mean(OD.Commercial$od_ship_price),sd=sd(OD.Commercial$od_ship_price)),add=TRUE)
hist(log(OD.Commercial$od_ship_price),prob=T,xlab="log(Shipping Price)",main="Shipping Price distribution")
curve(dnorm(x,mean=mean(log(OD.Commercial$od_ship_price)),sd=sd(log(OD.Commercial$od_ship_price))),add=TRUE)

par(mfrow=c(1,2))
hist(OD.Commercial$od_discount,prob=T,xlab="Discount",main="Discount distribution")
curve(dnorm(x,mean=mean(OD.Commercial$od_discount),sd=sd(OD.Commercial$od_discount)),add=TRUE)
hist(log(subset(OD.Commercial,od_discount>0,od_discount))[,1],prob=T,
     xlab="log(Discount)",main="Discount distribution")
curve(dnorm(x,mean=mean(log(subset(OD.Commercial,od_discount>0,od_discount))[,1]),
            sd=sd(log(subset(OD.Commercial,od_discount>0,od_discount))[,1])),add=TRUE)

par(mfrow=c(1,2))
hist(OD.Commercial$od_final_price,prob=T,xlab="Final Price",main="OD Final price distribution")
curve(dnorm(x,mean=mean(OD.Commercial$od_final_price),sd=sd(OD.Commercial$od_final_price)),add=TRUE)
hist(log(OD.Commercial$od_final_price),prob=T,xlab="log(Final Price)",main="OD Final price distribution")
curve(dnorm(x,mean=mean(log(OD.Commercial$od_final_price)),sd=sd(log(OD.Commercial$od_final_price))),add=TRUE)


# Amazon
par(mfrow=c(1,2))
hist(OD.Commercial$Amz_reg_price,prob=T,xlab="Regular Price",main="Regular Price distribution")
curve(dnorm(x,mean=mean(OD.Commercial$Amz_reg_price,na.rm=T),sd=sd(OD.Commercial$Amz_reg_price,na.rm=T)),add=TRUE)
hist(log(OD.Commercial$Amz_reg_price),prob=T,xlab="log(Regular Price)",main="Regular Price distribution")
curve(dnorm(x,mean=mean(log(OD.Commercial$Amz_reg_price),na.rm=T),sd=sd(log(OD.Commercial$Amz_reg_price),na.rm=T)),add=TRUE)

par(mfrow=c(1,2))
hist(OD.Commercial$Amz_ship_price,prob=T,xlab="Shipping Price",main="Shipping Price distribution")
curve(dnorm(x,mean=mean(OD.Commercial$Amz_ship_price,na.rm=T),sd=sd(OD.Commercial$Amz_ship_price,na.rm=T)),add=TRUE)
hist(log(subset(OD.Commercial,Amz_ship_price>0,Amz_ship_price))[,1],prob=T,xlab="log(Shipping Price)",
     main="Shipping Price distribution")
curve(dnorm(x,mean=mean(log(subset(OD.Commercial,Amz_ship_price>0,Amz_ship_price))[,1],na.rm=T),
            sd=sd(log(subset(OD.Commercial,Amz_ship_price>0,Amz_ship_price))[,1],na.rm=T)),add=TRUE)

par(mfrow=c(1,2))
hist(OD.Commercial$Amz_discount,prob=T,xlab="Discount",main="Discount distribution")
curve(dnorm(x,mean=mean(OD.Commercial$Amz_discount,na.rm=T),sd=sd(OD.Commercial$Amz_discount,na.rm=T)),add=TRUE)
hist(log(subset(OD.Commercial,Amz_discount>0,Amz_discount))[,1],prob=T,
     xlab="log(Discount)",main="Discount distribution")
curve(dnorm(x,mean=mean(log(subset(OD.Commercial,Amz_discount>0,Amz_discount))[,1],na.rm=T),
            sd=sd(log(subset(OD.Commercial,Amz_discount>0,Amz_discount))[,1],na.rm=T)),add=TRUE)

par(mfrow=c(1,2))
hist(OD.Commercial$Amz_final_price,prob=T,xlab="Final Price",main="Amazon Final price distribution")
curve(dnorm(x,mean=mean(OD.Commercial$Amz_final_price,na.rm=T),sd=sd(OD.Commercial$Amz_final_price,na.rm=T)),add=TRUE)
hist(log(OD.Commercial$Amz_final_price),prob=T,xlab="log(Final Price)",main="Amazon Final price distribution")
curve(dnorm(x,mean=mean(log(OD.Commercial$Amz_final_price),na.rm=T),sd=sd(log(OD.Commercial$Amz_final_price),na.rm=T)),add=TRUE)


# Amazon Marketplace
par(mfrow=c(1,2))
hist(OD.Commercial$Amz_mkt_reg_price,prob=T,xlab="Regular Price",main="Regular Price distribution")
curve(dnorm(x,mean=mean(OD.Commercial$Amz_mkt_reg_price,na.rm=T),sd=sd(OD.Commercial$Amz_mkt_reg_price,na.rm=T)),add=TRUE)
hist(log(OD.Commercial$Amz_mkt_reg_price),prob=T,xlab="log(Regular Price)",main="Regular Price distribution")
curve(dnorm(x,mean=mean(log(OD.Commercial$Amz_mkt_reg_price),na.rm=T),sd=sd(log(OD.Commercial$Amz_mkt_reg_price),na.rm=T)),add=TRUE)

par(mfrow=c(1,2))
hist(OD.Commercial$Amz_mkt_ship_price,prob=T,xlab="Shipping Price",main="Shipping Price distribution")
curve(dnorm(x,mean=mean(OD.Commercial$Amz_mkt_ship_price,na.rm=T),sd=sd(OD.Commercial$Amz_mkt_ship_price,na.rm=T)),add=TRUE)
hist(log(subset(OD.Commercial,Amz_mkt_ship_price>0,Amz_mkt_ship_price))[,1],prob=T,xlab="log(Shipping Price)",
     main="Shipping Price distribution")
curve(dnorm(x,mean=mean(log(subset(OD.Commercial,Amz_mkt_ship_price>0,Amz_mkt_ship_price))[,1],na.rm=T),
            sd=sd(log(subset(OD.Commercial,Amz_mkt_ship_price>0,Amz_mkt_ship_price))[,1],na.rm=T)),add=TRUE)

par(mfrow=c(1,2))
hist(OD.Commercial$Amz_mkt_discount,prob=T,xlab="Discount",main="Discount distribution")
curve(dnorm(x,mean=mean(OD.Commercial$Amz_mkt_discount,na.rm=T),sd=sd(OD.Commercial$Amz_mkt_discount,na.rm=T)),add=TRUE)
hist(log(subset(OD.Commercial,Amz_mkt_discount>0,Amz_mkt_discount))[,1],prob=T,
     xlab="log(Discount)",main="Discount distribution")
curve(dnorm(x,mean=mean(log(subset(OD.Commercial,Amz_mkt_discount>0,Amz_mkt_discount))[,1],na.rm=T),
            sd=sd(log(subset(OD.Commercial,Amz_mkt_discount>0,Amz_mkt_discount))[,1],na.rm=T)),add=TRUE)

par(mfrow=c(1,2))
hist(OD.Commercial$Amz_mkt_final_price,prob=T,xlab="Final Price",main="Amazon Final price distribution")
curve(dnorm(x,mean=mean(OD.Commercial$Amz_mkt_final_price,na.rm=T),sd=sd(OD.Commercial$Amz_mkt_final_price,na.rm=T)),add=TRUE)
hist(log(OD.Commercial$Amz_mkt_final_price),prob=T,xlab="log(Final Price)",main="Amazon Final price distribution")
curve(dnorm(x,mean=mean(log(OD.Commercial$Amz_mkt_final_price),na.rm=T),sd=sd(log(OD.Commercial$Amz_mkt_final_price),na.rm=T)),add=TRUE)

# Staples
par(mfrow=c(1,2))
hist(OD.Commercial$staples_reg_price,prob=T,xlab="Regular Price",main="Regular Price distribution")
curve(dnorm(x,mean=mean(OD.Commercial$staples_reg_price,na.rm=T),sd=sd(OD.Commercial$staples_reg_price,na.rm=T)),add=TRUE)
hist(log(OD.Commercial$staples_reg_price),prob=T,xlab="log(Regular Price)",main="Regular Price distribution")
curve(dnorm(x,mean=mean(log(OD.Commercial$staples_reg_price),na.rm=T),sd=sd(log(OD.Commercial$staples_reg_price),na.rm=T)),add=TRUE)

par(mfrow=c(1,2))
hist(OD.Commercial$staples_ship_price,prob=T,xlab="Shipping Price",main="Shipping Price distribution")
curve(dnorm(x,mean=mean(OD.Commercial$staples_ship_price,na.rm=T),sd=sd(OD.Commercial$staples_ship_price,na.rm=T)),add=TRUE)
hist(log(subset(OD.Commercial,staples_ship_price>0,staples_ship_price))[,1],prob=T,xlab="log(Shipping Price)",
     main="Shipping Price distribution")
curve(dnorm(x,mean=mean(log(subset(OD.Commercial,staples_ship_price>0,staples_ship_price))[,1],na.rm=T),
            sd=sd(log(subset(OD.Commercial,staples_ship_price>0,staples_ship_price))[,1],na.rm=T)),add=TRUE)

par(mfrow=c(1,2))
hist(OD.Commercial$staples_discount,prob=T,xlab="Discount",main="Discount distribution")
curve(dnorm(x,mean=mean(OD.Commercial$staples_discount,na.rm=T),sd=sd(OD.Commercial$staples_discount,na.rm=T)),add=TRUE)
hist(log(subset(OD.Commercial,staples_discount>0,staples_discount))[,1],prob=T,
     xlab="log(Discount)",main="Discount distribution")
curve(dnorm(x,mean=mean(log(subset(OD.Commercial,staples_discount>0,staples_discount))[,1],na.rm=T),
            sd=sd(log(subset(OD.Commercial,staples_discount>0,staples_discount))[,1],na.rm=T)),add=TRUE)

par(mfrow=c(1,2))
hist(OD.Commercial$staples_final_price,prob=T,xlab="Final Price",main="Amazon Final price distribution")
curve(dnorm(x,mean=mean(OD.Commercial$staples_final_price,na.rm=T),sd=sd(OD.Commercial$staples_final_price,na.rm=T)),add=TRUE)
hist(log(OD.Commercial$staples_final_price),prob=T,xlab="log(Final Price)",main="Amazon Final price distribution")
curve(dnorm(x,mean=mean(log(OD.Commercial$staples_final_price),na.rm=T),sd=sd(log(OD.Commercial$staples_final_price),na.rm=T)),add=TRUE)

# Min Competitor Price
par(mfrow=c(1,2))
hist(OD.Commercial$min_comp_price,prob=T,xlab="Min Competitor Price",main="Min Competitor price distribution")
curve(dnorm(x,mean=mean(OD.Commercial$min_comp_price,na.rm=T),sd=sd(OD.Commercial$min_comp_price,na.rm=T)),add=TRUE)
hist(log(OD.Commercial$min_comp_price),prob=T,xlab="log(Min Competitor Price)",main="Min Competitor price distribution")
curve(dnorm(x,mean=mean(log(OD.Commercial$min_comp_price),na.rm=T),sd=sd(log(OD.Commercial$min_comp_price),na.rm=T)),add=TRUE)


#---------------------------------------------------------------------------------------------------------------
#                                       Correlataion Matrix
#---------------------------------------------------------------------------------------------------------------

toMatch_Com <- c("units","final_price","reg_price","pr_diff","pr_rat","min_comp_price","ln")
OD.Commercial[,grep(paste(toMatch_Com,collapse="|"),names(OD.Commercial),value=T)]
OD.Commercial_new <- OD.Commercial[,grep(paste(toMatch_Com,collapse="|"),names(OD.Commercial),value=T)]
OD.Commercial_new <- OD.Commercial_new[,-c(10,11,15,19,25)]

cor(OD.Commercial_new,use="na.or.complete")
write.csv(cor(OD.Commercial_new,use="na.or.complete"),
          "C:/Yashwanth/Pricing/1.OfficeDepot/4.EDA/1.Correlation/Cor_Commercial.csv")

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

write.csv(OD_Sales,"OD_Sales_Tab.csv",row.names=F)
write.csv(OD_FP,"OD_FP_Tab.csv",row.names=F);write.csv(AMZ_FP,"AMZ_FP_Tab.csv",row.names=F)
write.csv(AMZ_MP_FP,"AMZ_MP_FP_Tab.csv",row.names=F);write.csv(Staples_FP,"Staples_FP_Tab.csv",row.names=F)

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

plot(min_comp_price,sales_units,xlab="Min Competitor Price",ylab="OD Sales",main="OD Sales v/s Min Competitor Price")
abline(lm(sales_units ~ min_comp_price))

detach(OD.Commercial)

#---------------------------------------------------------------------------------------------------------------
#                                      Box Plot
#---------------------------------------------------------------------------------------------------------------

dev.off()
attach(OD.Commercial)
boxplot(sales_units ~ as.factor(weekno),xlab="Weeks",ylab="OD Sales Units",main="Office Depot")
boxplot(od_final_price ~ as.factor(weekno),xlab="Weeks",ylab="OD Final Price",main="Office Depot")
boxplot(Amz_final_price ~ as.factor(weekno),xlab="Weeks",ylab="Amazon Final Price",main="Amazon")
boxplot(Amz_mkt_final_price ~ as.factor(weekno),xlab="Weeks",ylab="Amazon Marketplace Final Price",main="Amazon Marketplace")
boxplot(staples_final_price ~ as.factor(weekno),xlab="Weeks",ylab="Staples Final Price",main="Staples")

detach(OD.Commercial)

#---------------------------------------------------------------------------------------------------------------
#                                      Aggregate Data : Price
#---------------------------------------------------------------------------------------------------------------

OD.SKU_Summ_OD_FP <- as.data.frame(as.list(aggregate(OD.Commercial$od_final_price ~ OD.Commercial$SKU,
                                                FUN=function(x)c(min=min(x),max=max(x),len=length(x)))))
names(OD.SKU_Summ_OD_FP) <- c("SKU","OD Min Finalprice","OD Max Finalprice","SKU frequency")

OD.SKU_Summ_Amz_FP <- as.data.frame(as.list(aggregate(OD.Commercial$Amz_final_price ~ OD.Commercial$SKU,
                                FUN=function(x)c(min=min(x),max=max(x),len=length(x)))))
names(OD.SKU_Summ_Amz_FP) <- c("SKU","Amazon Min Finalprice","Amazon Max Finalprice","SKU frequency")
                                
OD.SKU_Summ_Amz_MP_FP <- as.data.frame(as.list(aggregate(OD.Commercial$Amz_mkt_final_price ~ OD.Commercial$SKU,
                                   FUN=function(x)c(min=min(x),max=max(x),len=length(x)))))
names(OD.SKU_Summ_Amz_MP_FP) <- c("SKU","AmzMP Min Finalprice","AmzMP Max Finalprice","SKU frequency")

OD.SKU_Summ_Stap_FP <- as.data.frame(as.list(aggregate(OD.Commercial$staples_final_price ~ OD.Commercial$SKU,
                                FUN=function(x)c(min=min(x),max=max(x),len=length(x)))))
names(OD.SKU_Summ_Stap_FP) <- c("SKU","Staples Min Finalprice","Staples Max Finalprice","SKU frequency")

write.csv(OD.SKU_Summ_OD_FP,".csv",row.names=F);write.csv(,".csv",row.names=F)
write.csv(,".csv",row.names=F);write.csv(,".csv",row.names=F)
write.csv(,".csv",row.names=F)
#---------------------------------------------------------------------------------------------------------------
#                                      Aggregate Data : Sales
#---------------------------------------------------------------------------------------------------------------

OD.SKU_Summ_OD_SU <- as.data.frame(as.list(aggregate(OD.Commercial$sales_units ~ OD.Commercial$SKU,
                                                     FUN=function(x)c(min=min(x),max=max(x),len=length(x)))))
names(OD.SKU_Summ_OD_SU) <- c("SKU","OD Min Sales Units","OD Max Sales Units","SKU frequency")

OD.SKU_Summ_Amz_SU <- as.data.frame(as.list(aggregate(OD.Commercial$sales_units ~ OD.Commercial$SKU,
                                                      FUN=function(x)c(min=min(x),max=max(x),len=length(x)))))
names(OD.SKU_Summ_Amz_SU) <- c("SKU","Amazon Min Sales Units","Amazon Max Sales Units","SKU frequency")

OD.SKU_Summ_Amz_MP_SU <- as.data.frame(as.list(aggregate(OD.Commercial$sales_units ~ OD.Commercial$SKU,
                                                         FUN=function(x)c(min=min(x),max=max(x),len=length(x)))))
names(OD.SKU_Summ_Amz_MP_SU) <- c("SKU","AmzMP Min Sales Units","AmzMP Max Sales Units","SKU frequency")

OD.SKU_Summ_Stap_SU <- as.data.frame(as.list(aggregate(OD.Commercial$sales_units ~ OD.Commercial$SKU,
                                                       FUN=function(x)c(min=min(x),max=max(x),len=length(x)))))
names(OD.SKU_Summ_Stap_SU) <- c("SKU","Staples Min Sales Units","Staples Max Sales Units","SKU frequency")

# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
#                                       || Missing Value Imputation ||
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------

write.csv(OD.Commercial[,c(1:3,grep("final_price",names(OD.Commercial)))],
          "C:/Yashwanth/Pricing/1.OfficeDepot/8.Data Output/OD.Commecial_Output2.csv",row.names=FALSE)

OD.Commercial_IP <- read.table("C:/Yashwanth/Pricing/1.OfficeDepot/8.Data Output/1.Commercial/OD.Commecial_Output2.csv", 
                               header = TRUE, sep = ",", quote = "\"", dec = "." ,fill=TRUE, comment.char="", as.is=TRUE)
OD.Commercial_IP <- data.frame(subset(OD.Commercial_IP,sales_units>0,names(OD.Commercial_IP)),row.names=NULL)

OD.Commercial_IP$pr_diff_amz_final_pr <- round(OD.Commercial_IP$od_final_price-OD.Commercial_IP$Amz_final_price,2)
OD.Commercial_IP$pr_diff_amz_mkt_final_pr <- round(OD.Commercial_IP$od_final_price-OD.Commercial_IP$Amz_mkt_final_price,2)   
OD.Commercial_IP$pr_diff_stpl_final_pr  <- round(OD.Commercial_IP$od_final_price-OD.Commercial_IP$staples_final_price,2)

OD.Commercial_IP$pr_rat_amz_final_pr <- round(OD.Commercial_IP$od_final_price/OD.Commercial_IP$Amz_final_price,2)
OD.Commercial_IP$pr_rat_amz_mkt_final_pr <- round(OD.Commercial_IP$od_final_price/OD.Commercial_IP$Amz_final_price,2)
OD.Commercial_IP$pr_rat_stpl_final_pr <- round(OD.Commercial_IP$od_final_price/OD.Commercial_IP$Amz_final_price,2)

OD.Commercial_IP$ln_sales_units <- round(log(OD.Commercial_IP$sales_units),2)
OD.Commercial_IP$ln_od_final_pr <- round(log(OD.Commercial_IP$od_final_price),2)
OD.Commercial_IP$ln_amz_final_pr <- round(log(OD.Commercial_IP$Amz_final_price),2)   
OD.Commercial_IP$ln_amz_mkt_final_pr <- round(log(OD.Commercial_IP$Amz_mkt_final_price),2)  
OD.Commercial_IP$ln_stpl_final_pr <- round(log(OD.Commercial_IP$staples_final_price),2)

OD.Commercial_MVI <- data.frame(subset(OD.Commercial,sales_units>0,
                      names(OD.Commercial[,c(1:4,6:10,12:16,18:22,24:27,48,49)])),row.names=NULL)
write.csv(OD.Commercial_MVI,"C:/Yashwanth/Pricing/1.OfficeDepot/8.Data Output/OD.Commecial_Output3.csv",row.names=FALSE)


#---------------------------------------------------------------------------------------------------------------
#                                       Distribution of Variables : Histogram
#---------------------------------------------------------------------------------------------------------------
# Office Depot
par(mfrow=c(1,2))
hist(OD.Commercial_IP$sales_units,prob=T,xlab="Sales Units",main="Sales Units distribution")
curve(dnorm(x,mean=mean(OD.Commercial_IP$sales_units),sd=sd(OD.Commercial_IP$sales_units)),add=TRUE)
hist(log(OD.Commercial_IP$sales_units),prob=T,xlab="log(Sales Units)",main="Sales Units distribution")
curve(dnorm(x,mean=mean(log(OD.Commercial_IP$sales_units)),sd=sd(log(OD.Commercial_IP$sales_units))),add=TRUE)

par(mfrow=c(1,2))
hist(OD.Commercial_IP$od_reg_price,prob=T,xlab="Regular Price",main="Regular Price distribution")
curve(dnorm(x,mean=mean(OD.Commercial_IP$od_reg_price),sd=sd(OD.Commercial_IP$od_reg_price)),add=TRUE)
hist(log(OD.Commercial_IP$od_reg_price),prob=T,xlab="log(Regular Price)",main="Regular Price distribution")
curve(dnorm(x,mean=mean(log(OD.Commercial_IP$od_reg_price)),sd=sd(log(OD.Commercial_IP$od_reg_price))),add=TRUE)

par(mfrow=c(1,2))
hist(OD.Commercial_IP$od_final_price,prob=T,xlab="Final Price",main="OD Final price distribution")
curve(dnorm(x,mean=mean(OD.Commercial_IP$od_final_price),sd=sd(OD.Commercial_IP$od_final_price)),add=TRUE)
hist(log(OD.Commercial_IP$od_final_price),prob=T,xlab="log(Final Price)",main="OD Final price distribution")
curve(dnorm(x,mean=mean(log(OD.Commercial_IP$od_final_price)),sd=sd(log(OD.Commercial_IP$od_final_price))),add=TRUE)


# Amazon
par(mfrow=c(1,2))
hist(OD.Commercial_IP$Amz_reg_price,prob=T,xlab="Regular Price",main="Regular Price distribution")
curve(dnorm(x,mean=mean(OD.Commercial_IP$Amz_reg_price,na.rm=T),sd=sd(OD.Commercial_IP$Amz_reg_price,na.rm=T)),add=TRUE)
hist(log(OD.Commercial_IP$Amz_reg_price),prob=T,xlab="log(Regular Price)",main="Regular Price distribution")
curve(dnorm(x,mean=mean(log(OD.Commercial_IP$Amz_reg_price),na.rm=T),sd=sd(log(OD.Commercial_IP$Amz_reg_price),na.rm=T)),add=TRUE)

par(mfrow=c(1,2))
hist(OD.Commercial_IP$Amz_final_price,prob=T,xlab="Final Price",main="Amazon Final price distribution")
curve(dnorm(x,mean=mean(OD.Commercial_IP$Amz_final_price,na.rm=T),sd=sd(OD.Commercial_IP$Amz_final_price,na.rm=T)),add=TRUE)
hist(log(OD.Commercial_IP$Amz_final_price),prob=T,xlab="log(Final Price)",main="Amazon Final price distribution")
curve(dnorm(x,mean=mean(log(OD.Commercial_IP$Amz_final_price),na.rm=T),sd=sd(log(OD.Commercial_IP$Amz_final_price),na.rm=T)),add=TRUE)


# Amazon Marketplace
par(mfrow=c(1,2))
hist(OD.Commercial_IP$Amz_mkt_reg_price,prob=T,xlab="Regular Price",main="Regular Price distribution")
curve(dnorm(x,mean=mean(OD.Commercial_IP$Amz_mkt_reg_price,na.rm=T),sd=sd(OD.Commercial_IP$Amz_mkt_reg_price,na.rm=T)),add=TRUE)
hist(log(OD.Commercial_IP$Amz_mkt_reg_price),prob=T,xlab="log(Regular Price)",main="Regular Price distribution")
curve(dnorm(x,mean=mean(log(OD.Commercial_IP$Amz_mkt_reg_price),na.rm=T),sd=sd(log(OD.Commercial_IP$Amz_mkt_reg_price),na.rm=T)),add=TRUE)

par(mfrow=c(1,2))
hist(OD.Commercial_IP$Amz_mkt_final_price,prob=T,xlab="Final Price",main="Amazon Final price distribution")
curve(dnorm(x,mean=mean(OD.Commercial_IP$Amz_mkt_final_price,na.rm=T),sd=sd(OD.Commercial_IP$Amz_mkt_final_price,na.rm=T)),add=TRUE)
hist(log(OD.Commercial_IP$Amz_mkt_final_price),prob=T,xlab="log(Final Price)",main="Amazon Final price distribution")
curve(dnorm(x,mean=mean(log(OD.Commercial_IP$Amz_mkt_final_price),na.rm=T),sd=sd(log(OD.Commercial_IP$Amz_mkt_final_price),na.rm=T)),add=TRUE)

# Staples
par(mfrow=c(1,2))
hist(OD.Commercial_IP$staples_reg_price,prob=T,xlab="Regular Price",main="Regular Price distribution")
curve(dnorm(x,mean=mean(OD.Commercial_IP$staples_reg_price,na.rm=T),sd=sd(OD.Commercial_IP$staples_reg_price,na.rm=T)),add=TRUE)
hist(log(OD.Commercial_IP$staples_reg_price),prob=T,xlab="log(Regular Price)",main="Regular Price distribution")
curve(dnorm(x,mean=mean(log(OD.Commercial_IP$staples_reg_price),na.rm=T),sd=sd(log(OD.Commercial_IP$staples_reg_price),na.rm=T)),add=TRUE)

par(mfrow=c(1,2))
hist(OD.Commercial_IP$staples_final_price,prob=T,xlab="Final Price",main="Amazon Final price distribution")
curve(dnorm(x,mean=mean(OD.Commercial_IP$staples_final_price,na.rm=T),sd=sd(OD.Commercial_IP$staples_final_price,na.rm=T)),add=TRUE)
hist(log(OD.Commercial_IP$staples_final_price),prob=T,xlab="log(Final Price)",main="Amazon Final price distribution")
curve(dnorm(x,mean=mean(log(OD.Commercial_IP$staples_final_price),na.rm=T),sd=sd(log(OD.Commercial_IP$staples_final_price),na.rm=T)),add=TRUE)


#---------------------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------------

# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
#                                       || Rough Work ||
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------

ddply(OD.Commercial[,grep("final_pr",names(OD.Commercial))])
OD.Commercial_aggr <- OD.Commercial[,c(1,grep("final_price",names(OD.Commercial)))]
OD.Commercial_aggr_DT <- data.table(OD.Commercial_aggr)

OD.Commercial_FT <- read.table("C:/Yashwanth/Pricing/1.OfficeDepot/8.Data Output/Commercial_freq_table.csv", 
                               header = TRUE, sep = ",", quote = "\"", dec = "." ,fill=TRUE, comment.char="", as.is=TRUE)
OD.Commercial_FT$id <- seq(1:nrow(OD.Commercial_FT))
OD.Commercial_FT <- OD.Commercial_FT[,c(6,1:5)]

OD.Commercial_FT_KM <- kmeans(OD.Commercial_FT[3:ncol(OD.Commercial_FT)],5)

OD.Commercial_Clusters <- data.frame(id=OD.Commercial_FT$id,clusters=OD.Commercial_FT_KM$cluster,OD.Commercial_FT[2:ncol(OD.Commercial_FT)])
write.csv(OD.Commercial_Clusters,"C:/Yashwanth/Pricing/1.OfficeDepot/8.Data Output/OD.Commecial_Output2.csv",row.names=NULL)

OD.Commercial_Clusters_Interpol <- read.table("C:/Yashwanth/Pricing/1.OfficeDepot/8.Data Output/OD.Commecial_Output2.csv", 
                                              header = TRUE, sep = ",", quote = "\"", dec = "." ,fill=TRUE, comment.char="", as.is=TRUE)

cluster_1 <- subset(OD.Commercial_Clusters_Interpol,clusters==1,names(OD.Commercial_Clusters_Interpol))
dist(t(cluster_1[4:7]),method="manhattan")

dist <- data.frame(as.matrix(dist(OD.Commercial_Clusters_Interpol[,4:7],method="manhattan",upper=FALSE)))

dist <- dist[,1:65]

SD <- NA
for(i in c(1:nrow(dist))){
  SD[i] <- minnz(as.integer(dist[i,]))
}
dist <- cbind(dist,SD)

OD.Comm_Model <- subset(OD.Commercial_Clusters_Interpol,Amz_final_price!=0,names(OD.Commercial_Clusters_Interpol))
