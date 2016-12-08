# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
#                                   Allied EDA
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------

Allied_Can_IP <- read.table("C:/Yashwanth/1. Allied/5. Canada/Canada_data.csv",
                            header = TRUE, sep = ",", quote = "\"", dec = "." ,fill=TRUE, comment.char="", as.is=TRUE)

Allied_Can_IP$Quarter <- as.factor(Allied_Can_IP$Quarter)
Allied_Can_IP$Week <- as.Date(Allied_Can_IP$Week)
Allied_Can_IP$Week_Number <- as.factor(Allied_Can_IP$Week_Number)
Allied_Can_IP$Store_No <- as.factor(Allied_Can_IP$Store_No)
Allied_Can_IP$Store <- as.factor(Allied_Can_IP$Store)
Allied_Can_IP$Comp_Location_Type <- as.factor(Allied_Can_IP$Comp_Location_Type)
Allied_Can_IP$CAD_Sales_Dollar <- as.numeric(Allied_Can_IP$CAD_Sales_Dollar)
Allied_Can_IP$CAD_Margin_Dollar <- as.numeric(Allied_Can_IP$CAD_Margin_Dollar)
Allied_Can_IP$Margin_per <- as.numeric(Allied_Can_IP$Margin_per)
#Allied_Can_IP$CAD_LY_Weekly_Sales_Dollar <- as.numeric(Allied_Can_IP$CAD_LY_Weekly_Sales_Dollar)
#Allied_Can_IP$CAD_LY_Weekly_Margin_Dollar <- as.numeric(Allied_Can_IP$CAD_LY_Weekly_Margin_Dollar)
Allied_Can_IP$CAD_Weekly_Budget_Sales_Dollar  <- as.numeric(Allied_Can_IP$CAD_Weekly_Budget_Sales_Dollar)
Allied_Can_IP$CAD_Weekly_Budget_Margin_Dollar <- as.numeric(Allied_Can_IP$CAD_Weekly_Budget_Margin_Dollar)

#---------------------------------------------------------------------------------------------------------------
#                                       Sampling : Stratification
#---------------------------------------------------------------------------------------------------------------

library(sampling)
Allied_Can_FT <- data.frame(table(Allied_Can_IP$Store))
Allied_Can_FT$Per <- (Allied_Can_FT$Freq/sum(Allied_Can_FT$Freq))*100
names(Allied_Can_FT)[1] <- "Store"

# Consider 70% of the data as sample
Allied_Can_FT$Strata_Size <- ceiling((Allied_Can_FT$Freq*(ceiling((dim(Allied_Can_IP)[1]/100)*70)/sum(Allied_Can_FT$Freq))))

# Stratification
Allied_Can_Strata <- strata(Allied_Can_IP,c("Store"),size = Allied_Can_FT$Strata_Size,
                            method = "srswor")
Allied_Can_IP <- getdata(Allied_Can_IP,Allied_Can_Strata)

#---------------------------------------------------------------------------------------------------------------
#                                       Descriptive Statistics
#---------------------------------------------------------------------------------------------------------------

library(psych)
View(describe(Allied_Can_IP[,sapply(Allied_Can_IP,is.numeric)]))
#summary(Allied_Can_IP)
#summary(Allied_Can_IP[,sapply(Allied_Can_IP,is.numeric)])

# Correlation Matrix : All
View(cor(Allied_Can_IP[,sapply(Allied_Can_IP,is.numeric)]))

library(plyr)
Corr_Units <- ddply(Allied_Can_IP,"Merch_Div",summarise,cor(Sales_Units,Margin_per))
Corr_Dollar <- ddply(Allied_Can_IP,"Merch_Div",summarise,cor(CAD_Sales_Dollar,Margin_per))


# --------------------------------------------------------------------------------------------------------------
#                                    Distribution of Variables : Histogram
# --------------------------------------------------------------------------------------------------------------

library(ggplot2) # Reference : http://docs.ggplot2.org/0.9.2.1/
library(data.table)
# data.table 1.9.6  For help type ?data.table or https://github.com/Rdatatable/data.table/wiki
# The fastest way to learn (by data.table authors): https://www.datacamp.com/courses/data-analysis-the-data-table-way

# Distribution by Store
attach(Allied_Can_IP)
ggplot(Allied_Can_IP,aes(x=CAD_Sales_Dollar)) +
  facet_wrap(~ Store) +
  geom_histogram(aes(y=..density..),col = "blue2") +
  stat_function(fun = dnorm,args = list(mean=mean(CAD_Sales_Dollar),sd=sd(CAD_Sales_Dollar)),colour = "red") +
  labs(title = "CAD_Sales_Dollar Distribution by Store")

ggplot(Allied_Can_IP,aes(x=CAD_Comp_Sls)) +
  facet_wrap(~ Store) +
  geom_histogram(aes(y=..density..),col = "blue2") +
  stat_function(fun = dnorm,args = list(mean=mean(CAD_Comp_Sls),sd=sd(CAD_Comp_Sls)),colour = "red") +
  labs(title = "CAD_Comp_Sls Distribution by Store")

ggplot(Allied_Can_IP,aes(x=CAD_Margin_Dollar)) +
  facet_wrap(~ Store) +
  geom_histogram(aes(y=..density..),col = "blue2") +
  stat_function(fun = dnorm,args = list(mean=mean(CAD_Margin_Dollar),sd=sd(CAD_Margin_Dollar)),colour = "red") +
  labs(title = "CAD_Margin_Dollar Distribution by Store")

ggplot(Allied_Can_IP,aes(x=CAD_LY_Weekly_Sales_Dollar)) +
  facet_wrap(~ Store) +
  geom_histogram(aes(y=..density..),col = "blue2") +
  stat_function(fun = dnorm,args = list(mean=mean(CAD_LY_Weekly_Sales_Dollar),sd=sd(CAD_LY_Weekly_Sales_Dollar)),colour = "red") +
  labs(title = "CAD_LY_Weekly_Sales_Dollar Distribution by Store")

ggplot(Allied_Can_IP,aes(x=CAD_LY_Weekly_Comp_Sales_Dollar)) +
  facet_wrap(~ Store) +
  geom_histogram(aes(y=..density..),col = "blue2") +
  stat_function(fun = dnorm,args = list(mean=mean(CAD_LY_Weekly_Comp_Sales_Dollar),sd=sd(CAD_LY_Weekly_Comp_Sales_Dollar)),colour = "red") +
  labs(title = "CAD_LY_Weekly_Comp_Sales_Dollar Distribution by Store")

ggplot(Allied_Can_IP,aes(x=CAD_Weekly_Budget_Sales_Dollar)) +
  facet_wrap(~ Store) +
  geom_histogram(aes(y=..density..),col = "blue2") +
  stat_function(fun = dnorm,args = list(mean=mean(CAD_Weekly_Budget_Sales_Dollar),sd=sd(CAD_Weekly_Budget_Sales_Dollar)),colour = "red") +
  labs(title = "CAD_Sales_Dollar Distribution by Store")

ggplot(Allied_Can_IP,aes(x=CAD_Weekly_Budget_Margin_Dollar)) +
  facet_wrap(~ Store) +
  geom_histogram(aes(y=..density..),col = "blue2") +
  stat_function(fun = dnorm,args = list(mean=mean(CAD_Weekly_Budget_Margin_Dollar),sd=sd(CAD_Weekly_Budget_Margin_Dollar)),colour = "red") +
  labs(title = "CAD_Weekly_Budget_Margin_Dollar Distribution by Store")
detach(Allied_Can_IP)


# Distribution by Merchant division

attach(Allied_Can_IP)
ggplot(Allied_Can_IP,aes(x=Sales_Units)) +
  facet_wrap(~ Merch_Div) +
  geom_histogram(aes(y=..density..),col = "blue2") +
  stat_function(fun = dnorm,args = list(mean=mean(Sales_Units),sd=sd(Sales_Units)),colour = "red") +
  labs(title = "CAD_Sales_Units Distribution by Merchant Division")

ggplot(Allied_Can_IP,aes(x=CAD_Sales_Dollar)) +
  facet_wrap(~ Merch_Div) +
  geom_histogram(aes(y=..density..),col = "blue2") +
  stat_function(fun = dnorm,args = list(mean=mean(CAD_Sales_Dollar),sd=sd(CAD_Sales_Dollar)),colour = "red") +
  labs(title = "CAD_Sales_Dollar Distribution by Merchant Division")

ggplot(Allied_Can_IP,aes(x=CAD_Comp_Sls)) +
  facet_wrap(~ Merch_Div) +
  geom_histogram(aes(y=..density..),col = "blue2") +
  stat_function(fun = dnorm,args = list(mean=mean(CAD_Comp_Sls),sd=sd(CAD_Comp_Sls)),colour = "red") +
  labs(title = "CAD_Comp_Sls Distribution by Merchant Division")

ggplot(Allied_Can_IP,aes(x=CAD_Margin_Dollar)) +
  facet_wrap(~ Merch_Div) +
  geom_histogram(aes(y=..density..),col = "blue2") +
  stat_function(fun = dnorm,args = list(mean=mean(CAD_Margin_Dollar),sd=sd(CAD_Margin_Dollar)),colour = "red") +
  labs(title = "CAD_Margin_Dollar Distribution by Merchant Division")

ggplot(Allied_Can_IP,aes(x=CAD_LY_Weekly_Sales_Dollar)) +
  facet_wrap(~ Merch_Div) +
  geom_histogram(aes(y=..density..),col = "blue2") +
  stat_function(fun = dnorm,args = list(mean=mean(CAD_LY_Weekly_Sales_Dollar),sd=sd(CAD_LY_Weekly_Sales_Dollar)),colour = "red") +
  labs(title = "CAD_LY_Weekly_Sales_Dollar Distribution by Merchant Division")

ggplot(Allied_Can_IP,aes(x=CAD_LY_Weekly_Comp_Sales_Dollar)) +
  facet_wrap(~ Merch_Div) +
  geom_histogram(aes(y=..density..),col = "blue2") +
  stat_function(fun = dnorm,args = list(mean=mean(CAD_LY_Weekly_Comp_Sales_Dollar),sd=sd(CAD_LY_Weekly_Comp_Sales_Dollar)),colour = "red") +
  labs(title = "CAD_LY_Weekly_Comp_Sales_Dollar Distribution by Merchant Division")

ggplot(Allied_Can_IP,aes(x=CAD_Weekly_Budget_Sales_Dollar)) +
  facet_wrap(~ Merch_Div) +
  geom_histogram(aes(y=..density..),col = "blue2") +
  stat_function(fun = dnorm,args = list(mean=mean(CAD_Weekly_Budget_Sales_Dollar),sd=sd(CAD_Weekly_Budget_Sales_Dollar)),colour = "red") +
  labs(title = "CAD_Sales_Dollar Distribution by Merchant Division")

ggplot(Allied_Can_IP,aes(x=CAD_Weekly_Budget_Margin_Dollar)) +
  facet_wrap(~ Merch_Div) +
  geom_histogram(aes(y=..density..),col = "blue2") +
  stat_function(fun = dnorm,args = list(mean=mean(CAD_Weekly_Budget_Margin_Dollar),sd=sd(CAD_Weekly_Budget_Margin_Dollar)),colour = "red") +
  labs(title = "CAD_Weekly_Budget_Margin_Dollar Distribution by Merchant Division")
detach(Allied_Can_IP)

