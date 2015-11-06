# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
#                                           ||  GAP EDA  ||
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------

rm(list=ls())
GAP_Input <- read.table("C:/Yashwanth/GAP/15. Phase3/1. Input_data/Data_Input_v2.csv", 
                     header = TRUE, sep = ",", quote = "\"", dec = "." ,fill=TRUE, comment.char="", as.is=TRUE)
str(GAP_Input)
GAP_Input$Level_1 <- as.factor(GAP_Input$Level_1)
GAP_Input$Level_2 <- as.factor(GAP_Input$Level_2)
GAP_Input$Category_Id <- as.factor(GAP_Input$Category_Id)
GAP_Input$Average_Bounce_rate <- round(GAP_Input$Average_Bounce_rate,2)
GAP_Input$Avg_Time_spent_on_page <- round(GAP_Input$Avg_Time_spent_on_page,2)

# --------------------------------------------------------------------------------------------------------------
#                                           ||  Descriptive Statistics  ||
# --------------------------------------------------------------------------------------------------------------

summary(GAP_Input[,sapply(GAP_Input, is.numeric)])
library(psych)
describe(GAP_Input[,sapply(GAP_Input, is.numeric)])
cor(GAP_Input[,sapply(GAP_Input, is.numeric)])

# --------------------------------------------------------------------------------------------------------------
#                                           ||  Histogram  ||
# --------------------------------------------------------------------------------------------------------------

library(ggplot2)

# Visits Distribution
attach(GAP_Input)
  ggplot(GAP_Input,aes(x=Total_Visits)) +
  facet_wrap(~ Level_1) +
  geom_histogram(aes(y=..density..),col = "blue2") +
  stat_function(fun = dnorm,args = list(mean=mean(Total_Visits),sd=sd(Total_Visits)),colour = "red") +
  labs(title = "Visits Distribution")

  ggplot(GAP_Input,aes(x=Total_Visits)) +
    geom_histogram(aes(y=..density..),col = "blue2") +
    stat_function(fun = dnorm,args = list(mean=mean(Total_Visits),sd=sd(Total_Visits)),colour = "red") +
    labs(title = "Visits Distribution")
  
# Revenue Distribution
  ggplot(GAP_Input,aes(x=Total_Revenue)) +
    facet_wrap(~ Level_1) +
    geom_histogram(aes(y=..density..),col = "blue2") +
    stat_function(fun = dnorm,args = list(mean=mean(Total_Revenue),sd=sd(Total_Revenue)),colour = "red") +
    labs(title = "Revenue Distribution")
  
  ggplot(GAP_Input,aes(x=Total_Revenue)) +
    geom_histogram(aes(y=..density..),col = "blue2") +
    stat_function(fun = dnorm,args = list(mean=mean(Total_Revenue),sd=sd(Total_Revenue)),colour = "red") +
    labs(title = "Revenue Distribution")
  
# Bounce rate Distribution
  ggplot(GAP_Input,aes(x=Average_Bounce_rate)) +
    facet_wrap(~ Level_1) +
    geom_histogram(aes(y=..density..),col = "blue2") +
    stat_function(fun = dnorm,args = list(mean=mean(Average_Bounce_rate),sd=sd(Average_Bounce_rate)),colour = "red") +
    labs(title = "Bounce rate Distribution")
  
  ggplot(GAP_Input,aes(x=Average_Bounce_rate)) +
    geom_histogram(aes(y=..density..),col = "blue2") +
    stat_function(fun = dnorm,args = list(mean=mean(Average_Bounce_rate),sd=sd(Average_Bounce_rate)),colour = "red") +
    labs(title = "Bounce rate Distribution")

# Time Spent Distribution
  ggplot(GAP_Input,aes(x=Avg_Time_spent_on_page)) +
    facet_wrap(~ Level_1) +
    geom_histogram(aes(y=..density..),col = "blue2") +
    stat_function(fun = dnorm,args = list(mean=mean(Avg_Time_spent_on_page),sd=sd(Avg_Time_spent_on_page)),colour = "red") +
    labs(title = "Time Spent Distribution")
  
  ggplot(GAP_Input,aes(x=Avg_Time_spent_on_page)) +
    geom_histogram(aes(y=..density..),col = "blue2") +
    stat_function(fun = dnorm,args = list(mean=mean(Avg_Time_spent_on_page),sd=sd(Avg_Time_spent_on_page)),colour = "red") +
    labs(title = "Time Spent Distribution")
detach(GAP_Input);detach(GAP_Input)
    
# --------------------------------------------------------------------------------------------------------------
#                                           ||  Box Plots  ||
# --------------------------------------------------------------------------------------------------------------

# Quartiles
quantile(GAP_Input$Total_Visits)
quantile(GAP_Input$Total_Revenue)
quantile(GAP_Input$Average_Bounce_rate)
quantile(GAP_Input$Avg_Time_spent_on_page)
  

attach(GAP_Input)
# Visits Distribution
ggplot(GAP_Input,aes(factor(Level_1),Total_Visits)) +
  geom_boxplot(outlier.colour = "green",outlier.size = 3) +
  labs(title = "Visits Distribution",x="Sub Level", y="Visits") +
  theme_bw(base_size = 12, base_family = "")

ggplot(GAP_Input,aes(x=Total_Visits)) +
  geom_boxplot(outlier.colour = "green",outlier.size = 3) +
  labs(title = "Visits Distribution",x="Sub Level", y="Visits") +
  theme_bw(base_size = 12, base_family = "")

detach(GAP_Input)

# --------------------------------------------------------------------------------------------------------------
#                                            ||  K-means clustering - Elbow method  ||
# --------------------------------------------------------------------------------------------------------------

# http://www.r-bloggers.com/optimal-number-of-clusters/
# setup period and method to compute correlations

GAP_Correlation <- cor(GAP_Input[,c("Total_Visits","Total_Revenue","Average_Bounce_rate","Avg_Time_spent_on_page")])    
GAP_Dissimilarity <- 1 - (GAP_Correlation)
GAP_Distance <- as.dist(GAP_Dissimilarity)

# get first 2 pricipal componenets
GAP_Comp <- cmdscale(GAP_Distance)


# --------------------------------------------------------------------------------------------------------------
#                                           ||  ANOVA & TukeyHSD  ||
# --------------------------------------------------------------------------------------------------------------
# Null Hypo : All the group means are equal.

anova(lm(Total_Visits ~ Level_1,data = GAP_Input))
anova(lm(Total_Revenue ~ Level_1,data = GAP_Input))

TukeyHSD(aov(lm(Total_Visits ~ Level_1,data = GAP_Input)))
TukeyHSD(aov(lm(Total_Revenue ~ Level_1,data = GAP_Input)))
TukeyHSD(aov(lm(Average_Bounce_rate ~ Level_1,data = GAP_Input)))
TukeyHSD(aov(lm(Avg_Time_spent_on_page ~ Level_1,data = GAP_Input)))

# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
# ------------------- END --------------------- ||  GAP EDA  || ------------------ END -------------------------
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
