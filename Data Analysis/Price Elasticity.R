# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------

#                                         || Price Elasticity ||
#          Price indices and implicit quantities of farm output and inputs for the United States, 1948-2011
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------

# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
#                                        Linear Regression
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------

rm(list=ls())
Farm_data <- read.table("C:/Yashwanth/Data Analysis/Farm_data.csv", 
                              header = TRUE, sep = ",", quote = "\"", dec = "." ,fill=TRUE, comment.char="", as.is=TRUE)
set.seed(100);
Farm_data$SKU <- ceiling(runif(nrow(Farm_data),54321,76843))
Farm_data <- Farm_data[,c("SKU","Price","Quantity","Sales")]

# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
#                                        Exploratory Data Analysis
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------

 # set the 1 by 2 layout plot window
 par(mfrow = c(1,2))

 # boxplot to check if there are outliers
 boxplot(Farm_data$Quantity,horizontal = TRUE, xlab = "Sales",main="Boxplot")

 # histogram to explore the data distribution shape
  hist(Farm_data$Quantity,main= "Histogram", xlab = "", prob = T)
  lines(density(Farm_data$Quantity),lty="dashed",lwd=2.5,col="red")
  dev.off()

  # set the 1 by 2 layout plot window
  par(mfrow = c(1,2))
  
  # boxplot to check if there are outliers
  boxplot(Farm_data$Price,horizontal = TRUE, xlab = "Price",main="Boxplot")
  
  # histogram to explore the data distribution shape
  hist(Farm_data$Price,main= "Histogram", xlab = "", prob = T)
  lines(density(Farm_data$Price),lty="dashed",lwd=2.5,col="red")

  # histogram to explore the data distribution shape
  hist(Farm_data$Price,prob=T,xlab="Price",main="Price distribution")
  curve(dnorm(x,mean=mean(Farm_data$Price),sd=sd(Farm_data$Price)),add=TRUE)

# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
#                                      Relationship b/n Mean & SD : Chebyshev's Theorem
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------

# library(psych)
# describe(Farm_data1$Quantity)  # mean = 192502.2  &  SD = 58104.71
# mean(Farm_data1$Quantity)+sd(Farm_data1$Quantity);mean(Farm_data1$Quantity)-sd(Farm_data1$Quantity)
# mean(Farm_data1$Quantity)+2*sd(Farm_data1$Quantity);mean(Farm_data1$Quantity)-2*sd(Farm_data1$Quantity)
# 
# # What proportion of data that have sales units between 0 & 40
# K <-  1.5    # (30-mean(Farm_data$Sales_Units))/sd(Farm_data$Sales_Units)
# 
# # Chebyshev's inequality : 1-(1/K^2)
# Cheb.In <- paste0((1-(1/K^2))*100,"%")

# Interpretation : ~56% of the data have sales units between 0 to 40. (OR)
# Approximately 56% of observations will lie within 1.5*standard deviation of the mean. 
# Note : Since sd is very high, it is not relevant to identify the proportion of data.

# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
#                                        Normality Check : Shapiro-Wilk test
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------

Farm_data1 <- data.frame(subset(Farm_data,Price <0.75,names(Farm_data)),row.names=NULL)
Farm_data2 <- data.frame(subset(Farm_data,Price >=0.75,names(Farm_data)),row.names=NULL)

shapiro.test(Farm_data1$Quantity)
shapiro.test(Farm_data2$Quantity)

# Inference : Since p<0.05, there is no strong evidence to reject NH that two groups of sales data are normally distibuted

# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
#                                                   T-test
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------

t.test(Farm_data1$Quantity,Farm_data2$Quantity)

# Inference : 1. Since p < 0.05, we reject NH & conclude that means of two groups of sales are significantly different.
#      2. With 95% CI, we can estimate means of sales with Price < 0.75 is somewhere in 85962 & 114631 units less than that of
#         Price > = 0.75
#      3. We conclude that, Price >= 0.75 had better sales than Price < 0.75

# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
#                                       Sales Driver Analysis and Price Elasticity Analysis
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------

pairs(Farm_data2[,2:3],col="blue",pch=20)
cor(Farm_data[,2:3])

# Inference : Since Correlation is very high(-vely), it might have high influence over the sales & hence include it in the model

#---------------------------------------------------------------------------------------------------------------
#                                                Linear Model
#---------------------------------------------------------------------------------------------------------------

# Linear Model
Model_1 <- lm(log(Quantity) ~ log(Price), data=Farm_data)
summary(Model_1)

# Inference : 1. Since p < 0.05, we can include price varaible into the model. In other words, Price has strong evidence in explaining
#                the Sales.
#             2. 86% of the variations in sales can be explained by Price alone & the remaining 14% can be attributed to other factors 
#                or inherent variability.

# The assumptions for the regression to be true. are that data are random and independent; 
# residuals are normally distributed and have constant variance. Let's check the residuals assumptions visually.
# plotting the residuals vs. other key model metrics
plot(Model_1)

# Inference : The Residuals vs Fitted graph shows that, the residuals scatter around the fitted line with no obvious pattern, 
#             and the Normal Q-Q graph shows that basically the residuals are normally distributed. The assumptions are met.

#---------------------------------------------------------------------------------------------------------------
#                                             BootStrapping/Data Sampling
#---------------------------------------------------------------------------------------------------------------

library(boot)
Farm_data_boot <- Farm_data[,2:3]
lm.bootstrap <- function(data, indices) {
  d <- data[indices, ]
  fit <- lm(log(Quantity) ~ log(Price), data = d)  
  return(coef(fit))
}

set.seed(12345) # seed for the RNG to ensure that you get exactly the same results as here
lm.boot <- boot(data=Farm_data_boot, statistic=lm.bootstrap, R=10000) # 10'000 samples
print(lm.boot)

# Final equation
#             Sales ~ (70829.97-516.0930)+(176137.12+865.7666)*Price              # Actual
#             Sales ~ (-12.410785-7.795004e-06)+(-0.609974-3.820490e-04)*Price    # Log 

#---------------------------------------------------------------------------------------------------------------
#                                                 Simulation
#---------------------------------------------------------------------------------------------------------------

# Decrease price by 10% 
Farm_data_New_Price_10 <- data.frame(cbind(Quantity=Farm_data$Quantity, Price=Farm_data$Price-((Farm_data$Price)/100)*10))
Farm_data_New_Price_10$New_sales_1 <- ceiling(1/exp(predict(Model_1,Farm_data_New_Price_10)))
colnames(Farm_data_New_Price_10)[2] <- "New_Price_1"
Farm_data <- cbind(Farm_data,Farm_data_New_Price_10[,2:3])
Farm_data$Dec_Price <- ifelse(Farm_data$New_sales > Farm_data$Sales, "Increase", " ")

# Increase price by 10% 
attach(Farm_data_New_Price_10)
Farm_data_New_Price_10$New_Price_2 <- ifelse(Farm_data$Dec_Price=="Increase",New_Price_1,(Farm_data$Price+((Farm_data$Price)/100)*36))
detach(Farm_data_New_Price_10)
colnames(Farm_data_New_Price_10)[4] <- "Price"
Farm_data_New_Price_10$New_sales_2 <- ceiling(1/exp(predict(Model_1,Farm_data_New_Price_10)))
colnames(Farm_data_New_Price_10)[4] <- "New_Price_2"
Farm_data <- cbind(Farm_data,Farm_data_New_Price_10[,4:5])
Farm_data$Inc_Price <- ifelse(Farm_data$Dec_Price=="Increase","",ifelse(Farm_data$New_sales_2 > Farm_data$Sales, "Increase", " "))
rm(list=setdiff(ls(),c("Farm_data","Model_1","lm.boot")))

#---------------------------------------------------------------------------------------------------------------
#                                                 Initial Recommended Price 
#---------------------------------------------------------------------------------------------------------------

attach(Farm_data)
Farm_data$Ini_Rec_Price <- round(ifelse(Dec_Price=="Increase",New_Price_1,New_Price_2),2)
Farm_data$Pred_Sales <- New_sales_2
detach(Farm_data)

Farm_data$Final_Action <- ifelse(Farm_data$Pred_Sales > Farm_data$Sales,"Increase"," ")

#---------------------------------------------------------------------------------------------------------------
#                                                   Optimization
#---------------------------------------------------------------------------------------------------------------

# Cost & Profit
Farm_data$Cost <- round(((Farm_data$Price)/100)*80,2)
Farm_data$Profit <- NA
attach(Farm_data)
for (i in 1:nrow(Farm_data)){
  Farm_data$Profit[i] <-  round((Ini_Rec_Price[i]-Cost[i])*(1/(exp(as.numeric(coef(Model_1))[1]-as.numeric(coef(Model_1))[2]*log(Ini_Rec_Price[i])))),3)
}
detach(Farm_data)

toMatch <- c("New","Dec","Inc")
Farm_data_New <- Farm_data[,grep(paste(toMatch,collapse="|"),names(Farm_data),value=T,invert=TRUE)]

# Floor & CAP
Farm_data_New$Floor <- 0
Farm_data_New$CAP <- NA
attach(Farm_data_New)
for(i in 1:nrow(Farm_data_New)){
  Farm_data_New$CAP[i] <- round(min(Farm_data_New$Price[i],Farm_data_New$Ini_Rec_Price[i]),2)
}
detach(Farm_data_New)

# Profit Optimization function
Profit_Opti_fun <- function(Price,Cost)
                    (round((Price-Cost)*(1/(exp(as.numeric(coef(Model_1))[1]-as.numeric(coef(Model_1))[2]*log(Price)))),3))

# Optimal Price & Max Profit
Opti <- list();
attach(Farm_data_New)
for(i in 1:nrow(Farm_data)){
Opti[[i]] <- optimize(Profit_Opti_fun,lower=Floor[i],upper=Ini_Rec_Price[i],Cost=Cost[i],maximum=TRUE)
}
detach(Farm_data_New)

# Unlist Optimal Price & Max profit
Farm_data_New$Opti_Price <- NA;Farm_data_New$Max_Profit <- NA
for(i in 1:nrow(Farm_data_New)){
  Farm_data_New$Opti_Price[i] <- round(unlist(lapply(Opti[[i]]$maximum,unlist)),2)
  Farm_data_New$Max_Profit[i] <- unlist(lapply(Opti[[i]]$objective,unlist))
}

Farm_data_New <- Farm_data_New[,c("SKU","Price","Quantity","Cost","Final_Action","Floor","CAP","Ini_Rec_Price","Profit","Sales",
                                  "Pred_Sales","Opti_Price","Max_Profit")]

#---------------------------------------------------------------------------------------------------------------
#                                                  Export output to Excel
#---------------------------------------------------------------------------------------------------------------

library(xlsx)
write.xlsx(Farm_data_New,"C:/Yashwanth/Data Analysis/Farm_Output.xlsx",row.names=FALSE)


# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------

# Inference : 1. Since p < 0.05, we can include price varaible into the model. In other words, Price has strong evidence in explaining
#                the Sales.
#             2. 84% of the variations in sales can be explained by Price alone & the remaining 16% can be attributed to other factors 
#                or inherent variability.

# The assumptions for the regression to be true. are that data are random and independent; 
# residuals are normally distributed and have constant variance. Let's check the residuals assumptions visually.
# plotting the residuals vs. other key model metrics


# Inference : The Residuals vs Fitted graph shows that, the residuals scatter around the fitted line with no obvious pattern, 
#             and the Normal Q-Q graph shows that basically the residuals are normally distributed. The assumptions are met.


# Profit_Opti_fun <- function(Price,Cost)
#               (round((Price-Cost)*(1/(exp(as.numeric(coef(Model_1))[1]-as.numeric(coef(Model_1))[2]*log(Price)))),3))
# 
# 
# 
# # Optimal Price & Max Profit
# Opti <- list();
# attach(Farm_data_New)
# for(i in 1:nrow(Farm_data)){
#   Opti[[i]] <- optimize(Profit_Opti_fun,lower=Floor[i],upper=CAP[i],Cost=Cost[i],maximum=TRUE)
# }
# detach(Farm_data_New)
