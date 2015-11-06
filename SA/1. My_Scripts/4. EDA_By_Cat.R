# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
#                                   Best Seller Model ;  Logistic Ression
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------

#---------------------------------------------------------------------------------------------------------------
#                                         Input Data and data types
#---------------------------------------------------------------------------------------------------------------

rm(list=ls())
setwd("C:/Yashwanth/Sports Authority/3. Model/")
SA_Input <- read.table("1. Model_Input_All_Cat.csv",header = TRUE, sep = ",", quote = "\"", dec = "." ,fill=TRUE, comment.char="", 
                       as.is=TRUE)

SA_Input$Revenue <- with(SA_Input,Revenue_Q1+Revenue_Q2+Revenue_Q3+Revenue_Q4)
SA_Input <- SA_Input[with(SA_Input,order(Revenue,decreasing = TRUE)),]

SA_Input$Sales <- c(rep(1,500),rep(0,length(501:nrow(SA_Input))))
SA_Input$Sales <- as.factor(SA_Input$Sales);str(SA_Input)
table(SA_Input$Sales)

#---------------------------------------------------------------------------------------------------------------
#                                           Cleats & Training Aid
#---------------------------------------------------------------------------------------------------------------

library(data.table)
SA_Input <- data.table(SA_Input)
SA_CTA <- SA_Input[SA_Input$Category=="Cleats & Training Aid"]

                                          # Descriptive Statistics  #
library(psych)
describe(as.data.frame(SA_CTA)[,sapply(as.data.frame(SA_CTA),is.numeric)])
cor(data.frame(SA_CTA)[,sapply(data.frame(SA_CTA),is.numeric)])
# 1. Revenue is moderately correlated with Review_Q1,Review_Q2 & Review_Q4 but weakly correlated with Review_Q3


                                          #   Weight of Evidence   #
library(woe)
C_Bin <- 10
WOE_Q1 <- woe(SA_CTA,"Reviews_Q1",FALSE,"Sales",C_Bin,Bad = 0,Good = 1)
WOE_Q1 <- cbind(WOE_Q1,Quarters=rep("Reviews Q1",nrow(WOE_Q1)))

WOE_Q2 <- woe(SA_CTA,"Reviews_Q2",FALSE,"Sales",C_Bin,Bad = 0,Good = 1)
WOE_Q2 <- cbind(WOE_Q2,Quarters=rep("Reviews Q2",nrow(WOE_Q2)))

WOE_Q3 <- woe(SA_CTA,"Reviews_Q3",FALSE,"Sales",C_Bin,Bad = 0,Good = 1)
WOE_Q3 <- cbind(WOE_Q3,Quarters=rep("Reviews Q3",nrow(WOE_Q3)))

WOE_Q4 <- woe(SA_CTA,"Reviews_Q4",FALSE,"Sales",C_Bin,Bad = 0,Good = 1)
WOE_Q4 <- cbind(WOE_Q4,Quarters=rep("Reviews Q4",nrow(WOE_Q4)))

WOE_CTA <- rbind(WOE_Q1,WOE_Q2,WOE_Q3,WOE_Q4);rm(WOE_Q1,WOE_Q2,WOE_Q3,WOE_Q4)

WOE_CTA$IV_Dec <- with(WOE_CTA,ifelse(IV == Inf | IV >= 0.0 & IV <= 0.02, "Unpredictive",
                                      ifelse(IV > 0.02 & IV < 0.1,"Weak Predictive",
                                             ifelse(IV >= 0.1 & IV <= 0.3, "Medium Predictive","Strong Predictive"))))

# Ref : http://ucanalytics.com/blogs/information-value-and-weight-of-evidencebanking-case/

#---------------------------------------------------------------------------------------------------------------
#                                             Activity Monitor
#---------------------------------------------------------------------------------------------------------------

SA_Act_Mon <- SA_Input[SA_Input$Category=="Activity Monitors"]

                                          # Descriptive Statistics  #
describe(as.data.frame(SA_Act_Mon)[,sapply(as.data.frame(SA_Act_Mon),is.numeric)])
cor(data.frame(SA_Act_Mon)[,sapply(data.frame(SA_Act_Mon),is.numeric)])
# 1. Revenue is weekly correlated with Review_Q1,Review_Q2 & Review_Q4 is strongly correlated but Review_Q2 is -vely correlated.

	
                                          #   Weight of Evidence   #
C_Bin <- 10
WOE_Q1 <- woe(SA_Act_Mon,"Reviews_Q1",FALSE,"Sales",C_Bin,Bad = 0,Good = 1)
WOE_Q1 <- cbind(WOE_Q1,Quarters=rep("Reviews Q1",nrow(WOE_Q1)))

WOE_Q2 <- woe(SA_Act_Mon,"Reviews_Q2",FALSE,"Sales",C_Bin,Bad = 0,Good = 1)
WOE_Q2 <- cbind(WOE_Q2,Quarters=rep("Reviews Q2",nrow(WOE_Q2)))

WOE_Q3 <- woe(SA_Act_Mon,"Reviews_Q3",FALSE,"Sales",C_Bin,Bad = 0,Good = 1)
WOE_Q3 <- cbind(WOE_Q3,Quarters=rep("Reviews Q3",nrow(WOE_Q3)))

WOE_Q4 <- woe(SA_Act_Mon,"Reviews_Q4",FALSE,"Sales",C_Bin,Bad = 0,Good = 1)
WOE_Q4 <- cbind(WOE_Q4,Quarters=rep("Reviews Q4",nrow(WOE_Q4)))

WOE_Act_Mon <- rbind(WOE_Q1,WOE_Q2,WOE_Q3,WOE_Q4);rm(WOE_Q1,WOE_Q2,WOE_Q3,WOE_Q4)

WOE_Act_Mon$IV_Dec <- with(WOE_Act_Mon,ifelse(IV == Inf | IV >= 0.0 & IV <= 0.02, "Unpredictive",
                                      ifelse(IV > 0.02 & IV < 0.1,"Weak Predictive",
                                             ifelse(IV >= 0.1 & IV <= 0.3, "Medium Predictive","Strong Predictive"))))

#---------------------------------------------------------------------------------------------------------------
#                                               Baseball Bat
#---------------------------------------------------------------------------------------------------------------

SA_BBB <- SA_Input[SA_Input$Category=="Baseball Bat"]

                                            # Descriptive Statistics  #
describe(as.data.frame(SA_BBB)[,sapply(as.data.frame(SA_BBB),is.numeric)])
cor(data.frame(SA_BBB)[,sapply(data.frame(SA_BBB),is.numeric)])
# 1. Revenue is weekly correlated with Review_Q1,Review_Q2, Review_Q4 but Review_Q3 is moderately correlated.


                                            #   Weight of Evidence   #
C_Bin <- 10
WOE_Q1 <- woe(SA_BBB,"Reviews_Q1",FALSE,"Sales",C_Bin,Bad = 0,Good = 1)
WOE_Q1 <- cbind(WOE_Q1,Quarters=rep("Reviews Q1",nrow(WOE_Q1)))

WOE_Q2 <- woe(SA_BBB,"Reviews_Q2",FALSE,"Sales",C_Bin,Bad = 0,Good = 1)
WOE_Q2 <- cbind(WOE_Q2,Quarters=rep("Reviews Q2",nrow(WOE_Q2)))

WOE_Q3 <- woe(SA_BBB,"Reviews_Q3",FALSE,"Sales",C_Bin,Bad = 0,Good = 1)
WOE_Q3 <- cbind(WOE_Q3,Quarters=rep("Reviews Q3",nrow(WOE_Q3)))

WOE_Q4 <- woe(SA_BBB,"Reviews_Q4",FALSE,"Sales",C_Bin,Bad = 0,Good = 1)
WOE_Q4 <- cbind(WOE_Q4,Quarters=rep("Reviews Q4",nrow(WOE_Q4)))

WOE_BBB <- rbind(WOE_Q1,WOE_Q2,WOE_Q3,WOE_Q4);rm(WOE_Q1,WOE_Q2,WOE_Q3,WOE_Q4)

WOE_BBB$IV_Dec <- with(WOE_BBB,ifelse(IV == Inf | IV >= 0.0 & IV <= 0.02, "Unpredictive",
                                      ifelse(IV > 0.02 & IV < 0.1,"Weak Predictive",
                                             ifelse(IV >= 0.1 & IV <= 0.3, "Medium Predictive","Strong Predictive"))))

#---------------------------------------------------------------------------------------------------------------
#                                             Golf Complete Sets
#---------------------------------------------------------------------------------------------------------------

SA_GCS <- SA_Input[SA_Input$Category=="Golf Complete Sets"]

                                          #  Descriptive Statistics  #
describe(as.data.frame(SA_GCS)[,sapply(as.data.frame(SA_GCS),is.numeric)])
cor(data.frame(SA_GCS)[,sapply(data.frame(SA_GCS),is.numeric)])
# 1. Revenue is strongly correlated with Review_Q1,Review_Q4 & modereately with Review_Q3 but Review_Q2 is weekly correlated.


                                          #   Weight of Evidence   #
C_Bin <- 10
WOE_Q1 <- woe(SA_GCS,"Reviews_Q1",FALSE,"Sales",C_Bin,Bad = 0,Good = 1)
WOE_Q1 <- cbind(WOE_Q1,Quarters=rep("Reviews Q1",nrow(WOE_Q1)))

WOE_Q2 <- woe(SA_GCS,"Reviews_Q2",FALSE,"Sales",C_Bin,Bad = 0,Good = 1)
WOE_Q2 <- cbind(WOE_Q2,Quarters=rep("Reviews Q2",nrow(WOE_Q2)))

WOE_Q3 <- woe(SA_GCS,"Reviews_Q3",FALSE,"Sales",C_Bin,Bad = 0,Good = 1)
WOE_Q3 <- cbind(WOE_Q3,Quarters=rep("Reviews Q3",nrow(WOE_Q3)))

WOE_Q4 <- woe(SA_GCS,"Reviews_Q4",FALSE,"Sales",C_Bin,Bad = 0,Good = 1)
WOE_Q4 <- cbind(WOE_Q4,Quarters=rep("Reviews Q4",nrow(WOE_Q4)))

WOE_GCS <- rbind(WOE_Q1,WOE_Q2,WOE_Q3,WOE_Q4);rm(WOE_Q1,WOE_Q2,WOE_Q3,WOE_Q4)

WOE_GCS$IV_Dec <- with(WOE_GCS,ifelse(IV == Inf | IV >= 0.0 & IV <= 0.02, "Unpredictive",
                                      ifelse(IV > 0.02 & IV < 0.1,"Weak Predictive",
                                             ifelse(IV >= 0.1 & IV <= 0.3, "Medium Predictive","Strong Predictive"))))

#---------------------------------------------------------------------------------------------------------------
#                                                   NFL Apparel
#---------------------------------------------------------------------------------------------------------------

SA_NFL_App <- SA_Input[SA_Input$Category=="NFL Apparel"]

                                              # Descriptive Statistics  #
describe(as.data.frame(SA_NFL_App)[,sapply(as.data.frame(SA_NFL_App),is.numeric)])
cor(data.frame(SA_NFL_App)[,sapply(data.frame(SA_NFL_App),is.numeric)])
# 1. Revenue is weekly correlated with Review_Q2,Review_Q3,Review_Q4 but Review_Q1 is -vely correlated.


                                            #   Weight of Evidence   #
C_Bin <- 10
WOE_Q1 <- woe(SA_NFL_App,"Reviews_Q1",FALSE,"Sales",C_Bin,Bad = 0,Good = 1)
WOE_Q1 <- cbind(WOE_Q1,Quarters=rep("Reviews Q1",nrow(WOE_Q1)))

WOE_Q2 <- woe(SA_NFL_App,"Reviews_Q2",FALSE,"Sales",C_Bin,Bad = 0,Good = 1)
WOE_Q2 <- cbind(WOE_Q2,Quarters=rep("Reviews Q2",nrow(WOE_Q2)))

WOE_Q3 <- woe(SA_NFL_App,"Reviews_Q3",FALSE,"Sales",C_Bin,Bad = 0,Good = 1)
WOE_Q3 <- cbind(WOE_Q3,Quarters=rep("Reviews Q3",nrow(WOE_Q3)))

WOE_Q4 <- woe(SA_NFL_App,"Reviews_Q4",FALSE,"Sales",C_Bin,Bad = 0,Good = 1)
WOE_Q4 <- cbind(WOE_Q4,Quarters=rep("Reviews Q4",nrow(WOE_Q4)))

WOE_NFL_App <- rbind(WOE_Q1,WOE_Q2,WOE_Q3,WOE_Q4);rm(WOE_Q1,WOE_Q2,WOE_Q3,WOE_Q4)

WOE_NFL_App$IV_Dec <- with(WOE_NFL_App,ifelse(IV == Inf | IV >= 0.0 & IV <= 0.02, "Unpredictive",
                                              ifelse(IV > 0.02 & IV < 0.1,"Weak Predictive",
                                                     ifelse(IV >= 0.1 & IV <= 0.3, "Medium Predictive","Strong Predictive"))))

#---------------------------------------------------------------------------------------------------------------
#                                                   Treadmill
#---------------------------------------------------------------------------------------------------------------

SA_Treadmill <- SA_Input[SA_Input$Category=="Treadmill"]

                                          # Descriptive Statistics  #
describe(as.data.frame(SA_Treadmill)[,sapply(as.data.frame(SA_Treadmill),is.numeric)])
cor(data.frame(SA_Treadmill)[,sapply(data.frame(SA_Treadmill),is.numeric)])
# 1. Revenue is moderately correlated across all Quarter Reviews.


                                          #   Weight of Evidence   #
C_Bin <- 10
WOE_Q1 <- woe(SA_Treadmill,"Reviews_Q1",FALSE,"Sales",C_Bin,Bad = 0,Good = 1)
WOE_Q1 <- cbind(WOE_Q1,Quarters=rep("Reviews Q1",nrow(WOE_Q1)))

WOE_Q2 <- woe(SA_Treadmill,"Reviews_Q2",FALSE,"Sales",C_Bin,Bad = 0,Good = 1)
WOE_Q2 <- cbind(WOE_Q2,Quarters=rep("Reviews Q2",nrow(WOE_Q2)))

WOE_Q3 <- woe(SA_Treadmill,"Reviews_Q3",FALSE,"Sales",C_Bin,Bad = 0,Good = 1)
WOE_Q3 <- cbind(WOE_Q3,Quarters=rep("Reviews Q3",nrow(WOE_Q3)))

WOE_Q4 <- woe(SA_Treadmill,"Reviews_Q4",FALSE,"Sales",C_Bin,Bad = 0,Good = 1)
WOE_Q4 <- cbind(WOE_Q4,Quarters=rep("Reviews Q4",nrow(WOE_Q4)))

WOE_Treadmill <- rbind(WOE_Q1,WOE_Q2,WOE_Q3,WOE_Q4);rm(WOE_Q1,WOE_Q2,WOE_Q3,WOE_Q4)

WOE_Treadmill$IV_Dec <- with(WOE_Treadmill,ifelse(IV == Inf | IV >= 0.0 & IV <= 0.02, "Unpredictive",
                                                  ifelse(IV > 0.02 & IV < 0.1,"Weak Predictive",
                                                         ifelse(IV >= 0.1 & IV <= 0.3, "Medium Predictive","Strong Predictive"))))

#---------------------------------------------------------------------------------------------------------------
#                                              Women's Running Shoes
#---------------------------------------------------------------------------------------------------------------

SA_WRS <- SA_Input[SA_Input$Category=="Women's Running Shoes"]

                                            #  Descriptive Statistics  #
describe(as.data.frame(SA_WRS)[,sapply(as.data.frame(SA_WRS),is.numeric)])
cor(data.frame(SA_WRS)[,sapply(data.frame(SA_WRS),is.numeric)])
# 1. Revenue is weekly correlated with Review_Q1 & fairly correlated with Review_Q2,Review_Q4 but Strongly with Review_Q3.


                                            #   Weight of Evidence   #
C_Bin <- 10
WOE_Q1 <- woe(SA_WRS,"Reviews_Q1",FALSE,"Sales",C_Bin,Bad = 0,Good = 1)
WOE_Q1 <- cbind(WOE_Q1,Quarters=rep("Reviews Q1",nrow(WOE_Q1)))

WOE_Q2 <- woe(SA_WRS,"Reviews_Q2",FALSE,"Sales",C_Bin,Bad = 0,Good = 1)
WOE_Q2 <- cbind(WOE_Q2,Quarters=rep("Reviews Q2",nrow(WOE_Q2)))

WOE_Q3 <- woe(SA_WRS,"Reviews_Q3",FALSE,"Sales",C_Bin,Bad = 0,Good = 1)
WOE_Q3 <- cbind(WOE_Q3,Quarters=rep("Reviews Q3",nrow(WOE_Q3)))

WOE_Q4 <- woe(SA_WRS,"Reviews_Q4",FALSE,"Sales",C_Bin,Bad = 0,Good = 1)
WOE_Q4 <- cbind(WOE_Q4,Quarters=rep("Reviews Q4",nrow(WOE_Q4)))

WOE_WRS <- rbind(WOE_Q1,WOE_Q2,WOE_Q3,WOE_Q4);rm(WOE_Q1,WOE_Q2,WOE_Q3,WOE_Q4)

WOE_WRS$IV_Dec <- with(WOE_WRS,ifelse(IV == Inf | IV >= 0.0 & IV <= 0.02, "Unpredictive",
                                      ifelse(IV > 0.02 & IV < 0.1,"Weak Predictive",
                                             ifelse(IV >= 0.1 & IV <= 0.3, "Medium Predictive","Strong Predictive"))))

rm(list = setdiff(ls(),c(SA_Input,"SA_CTA","SA_Act_Mon","SA_BBB","SA_GCS","SA_NFL_App","SA_Treadmill","SA_WRS")))







# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
# -------------- END --------------------- Best Seller Model ;  Logistic Ression ----------- END ---------------
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
