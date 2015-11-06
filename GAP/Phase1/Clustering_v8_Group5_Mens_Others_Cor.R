# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
#                                       ||     Bounce Rate    ||
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------

Bounce_Rate <- read.table("C:/Yashwanth/Clustering/1.Input_data/Bounce_rate.csv", 
                          header = TRUE, sep = ",", quote = "\"", dec = "." ,fill=TRUE, comment.char="", as.is=TRUE)

Avg_Time_On_Page <- read.table("C:/Yashwanth/Clustering/1.Input_data/AvgTimeOnPage.csv", 
                               header = TRUE, sep = ",", quote = "\"", dec = "." ,fill=TRUE, comment.char="", as.is=TRUE)

Visits <- read.table("C:/Yashwanth/Clustering/1.Input_data/Visits.csv", 
                     header = TRUE, sep = ",", quote = "\"", dec = "." ,fill=TRUE, comment.char="", as.is=TRUE)

Prod_Page_Views <- read.table("C:/Yashwanth/Clustering/1.Input_data/ProductPageViews.csv", 
                              header = TRUE, sep = ",", quote = "\"", dec = "." ,fill=TRUE, comment.char="", as.is=TRUE)

Bounce_Rate_Mens_Others <- data.frame(subset(Bounce_Rate,
                                             Pages=="big_and_tall_mens_shirts" | Pages=="shirts_men" |
                                               Pages=="mens_fitted_dress_shirts" | Pages=="mens_cotton_pants" |
                                               Pages=="navy_pants_for_men" | Pages=="mens_dress_pants" |
                                               Pages=="tall_dress_pants_for_men" | Pages=="mens_madras_shorts" |
                                               Pages=="mens_plaid_shorts" | Pages=="mens_running_shorts" |
                                               Pages=="mens_swim_shorts" | Pages=="mens_tall_shorts",
                                             select=names(Bounce_Rate)),row.names=NULL) 

Avg_Time_On_Page_Mens_Others <- data.frame(subset(Avg_Time_On_Page,
                                                  Pages=="big_and_tall_mens_shirts" | Pages=="shirts_men" |
                                                    Pages=="mens_fitted_dress_shirts" | Pages=="mens_cotton_pants" |
                                                    Pages=="navy_pants_for_men" | Pages=="mens_dress_pants" |
                                                    Pages=="tall_dress_pants_for_men" | Pages=="mens_madras_shorts" |
                                                    Pages=="mens_plaid_shorts" | Pages=="mens_running_shorts" |
                                                    Pages=="mens_swim_shorts" | Pages=="mens_tall_shorts",
                                                  select=names(Avg_Time_On_Page)),row.names=NULL) 

Visits_Mens_Others <- data.frame(subset(Visits,
                                        Pages=="big_and_tall_mens_shirts" | Pages=="shirts_men" |
                                          Pages=="mens_fitted_dress_shirts" | Pages=="mens_cotton_pants" |
                                          Pages=="navy_pants_for_men" | Pages=="mens_dress_pants" |
                                          Pages=="tall_dress_pants_for_men" | Pages=="mens_madras_shorts" |
                                          Pages=="mens_plaid_shorts" | Pages=="mens_running_shorts" |
                                          Pages=="mens_swim_shorts" | Pages=="mens_tall_shorts",
                                        select=names(Visits)),row.names=NULL) 

Prod_Page_Views_Mens_Others <- data.frame(subset(Prod_Page_Views,
                                                 Pages=="big_and_tall_mens_shirts" | Pages=="shirts_men" |
                                                   Pages=="mens_fitted_dress_shirts" | Pages=="mens_cotton_pants" |
                                                   Pages=="navy_pants_for_men" | Pages=="mens_dress_pants" |
                                                   Pages=="tall_dress_pants_for_men" | Pages=="mens_madras_shorts" |
                                                   Pages=="mens_plaid_shorts" | Pages=="mens_running_shorts" |
                                                   Pages=="mens_swim_shorts" | Pages=="mens_tall_shorts",
                                                 select=names(Prod_Page_Views)),row.names=NULL) 

rm(Bounce_Rate,Avg_Time_On_Page,Visits,Prod_Page_Views)

# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
#                                       || Correlation matrix - One ||
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------

rownames(GAP_Pages_Mens_Pants) <- GAP_Pages_Mens_Pants[,1]
GAP_Pages_Mens_Pants <- GAP_Pages_Mens_Pants[,-1]
GAP_Pages_Mens_Pants_T <- data.frame(t(GAP_Pages_Mens_Pants))
GAP_Pages_Mens_Pants_Cor <- data.frame(cor(GAP_Pages_Mens_Pants))
write.csv(GAP_Pages_Mens_Pants_Cor,"C:/Yashwanth/Clustering/3.Correlation Matrix/5.Mens_Group_Five/Corelation_Pages_Mens_Others.csv")
rm(list=ls(pattern="GAP"))

# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
#                                       || Correlation matrix  - Two ||
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------

#-----------------------------------------------------------------------------------------------------------------
#------------------------------------------------- Bounce Rate ------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------
                           # --------------------- # Pair One #--------------------#

Bounce_Rate_Mens_Others_Pair1 <- data.frame(subset(Bounce_Rate_Mens_Others,
                                                  Pages=="mens_tall_shorts" | Pages=="big_and_tall_mens_shirts",
                                                    names(Bounce_Rate_Mens_Others)),row.names=NULL)
BR_Mens_Others_Pair1 <- data.frame(t(Bounce_Rate_Mens_Others_Pair1[,2:ncol(Bounce_Rate_Mens_Others_Pair1)]),
                                  row.names=NULL)
names(BR_Mens_Others_Pair1) <- Bounce_Rate_Mens_Others_Pair1[,1]                        
BR_Mens_Others_Pair1_Cor <- data.frame(cor(BR_Mens_Others_Pair1))
print(BR_Mens_Others_Pair1_Cor);rm(BR_Mens_Others_Pair1,Bounce_Rate_Mens_Others_Pair1)
write.csv(BR_Mens_Others_Pair1_Cor,"C:/Yashwanth/Clustering/3.Correlation Matrix/5.Mens_Group_Five/BR_Pair1_Others.csv")

                              #--------------------- # Pair Two #--------------------#

Bounce_Rate_Mens_Others_Pair2 <- data.frame(subset(Bounce_Rate_Mens_Others,
                                                   Pages=="mens_fitted_dress_shirts" | Pages=="mens_cotton_pants",
                                                   names(Bounce_Rate_Mens_Others)),row.names=NULL)
BR_Mens_Others_Pair2 <- data.frame(t(Bounce_Rate_Mens_Others_Pair2[,2:ncol(Bounce_Rate_Mens_Others_Pair2)]),
                                   row.names=NULL)
names(BR_Mens_Others_Pair2) <- Bounce_Rate_Mens_Others_Pair2[,1]                        
BR_Mens_Others_Pair2_Cor <- data.frame(cor(BR_Mens_Others_Pair2))
print(BR_Mens_Others_Pair2_Cor);rm(BR_Mens_Others_Pair2,Bounce_Rate_Mens_Others_Pair2)
write.csv(BR_Mens_Others_Pair2_Cor,"C:/Yashwanth/Clustering/3.Correlation Matrix/5.Mens_Group_Five/BR_Pair2_Others.csv")

                                  #--------------------- # Pair Three #--------------------#

Bounce_Rate_Mens_Others_Pair3 <- data.frame(subset(Bounce_Rate_Mens_Others,
                                                   Pages=="tall_dress_pants_for_men" | Pages=="shirts_men",
                                                   names(Bounce_Rate_Mens_Others)),row.names=NULL)
BR_Mens_Others_Pair3 <- data.frame(t(Bounce_Rate_Mens_Others_Pair3[,2:ncol(Bounce_Rate_Mens_Others_Pair3)]),
                                   row.names=NULL)
names(BR_Mens_Others_Pair3) <- Bounce_Rate_Mens_Others_Pair3[,1]                        
BR_Mens_Others_Pair3_Cor <- data.frame(cor(BR_Mens_Others_Pair3))
print(BR_Mens_Others_Pair3_Cor);rm(BR_Mens_Others_Pair3,Bounce_Rate_Mens_Others_Pair3)
write.csv(BR_Mens_Others_Pair3_Cor,"C:/Yashwanth/Clustering/3.Correlation Matrix/5.Mens_Group_Five/BR_Pair3_Others.csv")

                                  #--------------------- # Pair Four #--------------------#


Bounce_Rate_Mens_Others_Pair4 <- data.frame(subset(Bounce_Rate_Mens_Others,
                                                   Pages=="navy_pants_for_men" | Pages=="mens_running_shorts",
                                                   names(Bounce_Rate_Mens_Others)),row.names=NULL)
BR_Mens_Others_Pair4 <- data.frame(t(Bounce_Rate_Mens_Others_Pair4[,2:ncol(Bounce_Rate_Mens_Others_Pair4)]),
                                   row.names=NULL)
names(BR_Mens_Others_Pair4) <- Bounce_Rate_Mens_Others_Pair4[,1]                        
BR_Mens_Others_Pair4_Cor <- data.frame(cor(BR_Mens_Others_Pair4))
print(BR_Mens_Others_Pair4_Cor);rm(BR_Mens_Others_Pair4,Bounce_Rate_Mens_Others_Pair4)
write.csv(BR_Mens_Others_Pair4_Cor,"C:/Yashwanth/Clustering/3.Correlation Matrix/5.Mens_Group_Five/BR_Pair4_Others.csv")


                                #--------------------- # Pair Five #--------------------#

Bounce_Rate_Mens_Others_Pair5 <- data.frame(subset(Bounce_Rate_Mens_Others,
                                                   Pages=="mens_dress_pants" | Pages=="mens_swim_shorts",
                                                   names(Bounce_Rate_Mens_Others)),row.names=NULL)
BR_Mens_Others_Pair5 <- data.frame(t(Bounce_Rate_Mens_Others_Pair5[,2:ncol(Bounce_Rate_Mens_Others_Pair5)]),
                                   row.names=NULL)
names(BR_Mens_Others_Pair5) <- Bounce_Rate_Mens_Others_Pair5[,1]                        
BR_Mens_Others_Pair5_Cor <- data.frame(cor(BR_Mens_Others_Pair5))
print(BR_Mens_Others_Pair5_Cor);rm(BR_Mens_Others_Pair5,Bounce_Rate_Mens_Others_Pair5)
write.csv(BR_Mens_Others_Pair5_Cor,"C:/Yashwanth/Clustering/3.Correlation Matrix/5.Mens_Group_Five/BR_Pair5_Others.csv")
rm(list=ls(pattern="Cor"))

#-----------------------------------------------------------------------------------------------------------------
#------------------------------------------------- Avg TOP ------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------
                                  #--------------------- # Pair One #--------------------#

Avg_Time_On_Page_Mens_Others_Pair1 <- data.frame(subset(Avg_Time_On_Page_Mens_Others,
                                              Pages=="mens_tall_shorts" | Pages=="big_and_tall_mens_shirts",
                                              names(Avg_Time_On_Page_Mens_Others)),row.names=NULL)
ATOP_Mens_Pants_Pair1 <- data.frame(t(Avg_Time_On_Page_Mens_Others_Pair1[,2:ncol(Avg_Time_On_Page_Mens_Others_Pair1)]),
                                    row.names=NULL)
names(ATOP_Mens_Pants_Pair1) <- Avg_Time_On_Page_Mens_Others_Pair1[,1]                        
ATOP_Mens_Pants_Pair1_Cor <- data.frame(cor(ATOP_Mens_Pants_Pair1))
print(ATOP_Mens_Pants_Pair1_Cor);rm(ATOP_Mens_Pants_Pair1,Avg_Time_On_Page_Mens_Others_Pair1)
write.csv(ATOP_Mens_Pants_Pair1_Cor,"C:/Yashwanth/Clustering/3.Correlation Matrix/5.Mens_Group_Five/ATOP_Pair1_Others.csv")

                                  #--------------------- # Pair Two #--------------------#

Avg_Time_On_Page_Mens_Others_Pair2 <- data.frame(subset(Avg_Time_On_Page_Mens_Others,
                                              Pages=="mens_fitted_dress_shirts" | Pages=="mens_cotton_pants",
                                              names(Avg_Time_On_Page_Mens_Others)),row.names=NULL)
ATOP_Mens_Others_Pair2 <- data.frame(t(Avg_Time_On_Page_Mens_Others_Pair2[,2:ncol(Avg_Time_On_Page_Mens_Others_Pair2)]),
                                    row.names=NULL)
names(ATOP_Mens_Others_Pair2) <- Avg_Time_On_Page_Mens_Others_Pair2[,1]                        
ATOP_Mens_Others_Pair2_Cor <- data.frame(cor(ATOP_Mens_Others_Pair2))
print(ATOP_Mens_Others_Pair2_Cor);rm(ATOP_Mens_Others_Pair2,Avg_Time_On_Page_Mens_Others_Pair2)
write.csv(ATOP_Mens_Others_Pair2_Cor,"C:/Yashwanth/Clustering/3.Correlation Matrix/5.Mens_Group_Five/ATOP_Pair2_Others.csv")

                                  #--------------------- # Pair Three #--------------------#

Avg_Time_On_Page_Mens_Others_Pair3 <- data.frame(subset(Avg_Time_On_Page_Mens_Others,
                                              Pages=="tall_dress_pants_for_men" | Pages=="shirts_men",
                                              names(Avg_Time_On_Page_Mens_Others)),row.names=NULL)
ATOP_Mens_Others_Pair3 <- data.frame(t(Avg_Time_On_Page_Mens_Others_Pair3[,2:ncol(Avg_Time_On_Page_Mens_Others_Pair3)]),
                                    row.names=NULL)
names(ATOP_Mens_Others_Pair3) <- Avg_Time_On_Page_Mens_Others_Pair3[,1]                        
ATOP_Mens_Others_Pair3_Cor <- data.frame(cor(ATOP_Mens_Others_Pair3))
print(ATOP_Mens_Others_Pair3_Cor);rm(ATOP_Mens_Others_Pair3,Avg_Time_On_Page_Mens_Others_Pair3)
write.csv(ATOP_Mens_Others_Pair3_Cor,"C:/Yashwanth/Clustering/3.Correlation Matrix/5.Mens_Group_Five/ATOP_Pair3_Others.csv")

                              #--------------------- # Pair Four #--------------------#

Avg_Time_On_Page_Mens_Others_Pair4 <- data.frame(subset(Avg_Time_On_Page_Mens_Others,
                                                        Pages=="navy_pants_for_men" | Pages=="mens_running_shorts",
                                                        names(Avg_Time_On_Page_Mens_Others)),row.names=NULL)
ATOP_Mens_Others_Pair4 <- data.frame(t(Avg_Time_On_Page_Mens_Others_Pair4[,2:ncol(Avg_Time_On_Page_Mens_Others_Pair4)]),
                                     row.names=NULL)
names(ATOP_Mens_Others_Pair4) <- Avg_Time_On_Page_Mens_Others_Pair4[,1]                        
ATOP_Mens_Others_Pair4_Cor <- data.frame(cor(ATOP_Mens_Others_Pair4))
print(ATOP_Mens_Others_Pair4_Cor);rm(ATOP_Mens_Others_Pair4,Avg_Time_On_Page_Mens_Others_Pair4)
write.csv(ATOP_Mens_Others_Pair4_Cor,"C:/Yashwanth/Clustering/3.Correlation Matrix/5.Mens_Group_Five/ATOP_Pair4_Others.csv")


                              #--------------------- # Pair Five #--------------------#

Avg_Time_On_Page_Mens_Others_Pair5 <- data.frame(subset(Avg_Time_On_Page_Mens_Others,
                                                        Pages=="mens_dress_pants" | Pages=="mens_swim_shorts",
                                                        names(Avg_Time_On_Page_Mens_Others)),row.names=NULL)
ATOP_Mens_Others_Pair5 <- data.frame(t(Avg_Time_On_Page_Mens_Others_Pair5[,2:ncol(Avg_Time_On_Page_Mens_Others_Pair5)]),
                                     row.names=NULL)
names(ATOP_Mens_Others_Pair5) <- Avg_Time_On_Page_Mens_Others_Pair5[,1]                        
ATOP_Mens_Others_Pair5_Cor <- data.frame(cor(ATOP_Mens_Others_Pair5))
print(ATOP_Mens_Others_Pair5_Cor);rm(ATOP_Mens_Others_Pair5,Avg_Time_On_Page_Mens_Others_Pair5)
write.csv(ATOP_Mens_Others_Pair5_Cor,"C:/Yashwanth/Clustering/3.Correlation Matrix/5.Mens_Group_Five/ATOP_Pair5_Others.csv")
rm(list=ls(pattern="Cor"))

#-----------------------------------------------------------------------------------------------------------------
#------------------------------------------------- Visits------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------
                            #--------------------- # Pair One #--------------------#

Visits_Mens_Others_Pair1 <- data.frame(subset(Visits_Mens_Others,
                                             Pages=="mens_tall_shorts" | Pages=="big_and_tall_mens_shirts",
                                             names(Visits_Mens_Others)),row.names=NULL)
Vis_Mens_Others_Pair1 <- data.frame(t(Visits_Mens_Others_Pair1[,2:ncol(Visits_Mens_Others_Pair1)]),
                                   row.names=NULL)
names(Vis_Mens_Others_Pair1) <- Visits_Mens_Others_Pair1[,1]                        
Vis_Mens_Others_Pair1_Cor <- data.frame(cor(Vis_Mens_Others_Pair1))
print(Vis_Mens_Others_Pair1_Cor);rm(Vis_Mens_Others_Pair1,Visits_Mens_Others_Pair1)
write.csv(Vis_Mens_Others_Pair1_Cor,"C:/Yashwanth/Clustering/3.Correlation Matrix/5.Mens_Group_Five/Vis_Pair1_Others.csv")

                            #--------------------- # Pair Two #--------------------#

Visits_Mens_Others_Pair2 <- data.frame(subset(Visits_Mens_Others,
                                             Pages=="mens_fitted_dress_shirts" | Pages=="mens_cotton_pants",
                                             names(Visits_Mens_Others)),row.names=NULL)
Vis_Mens_Others_Pair2 <- data.frame(t(Visits_Mens_Others_Pair2[,2:ncol(Visits_Mens_Others_Pair2)]),
                                   row.names=NULL)
names(Vis_Mens_Others_Pair2) <- Visits_Mens_Others_Pair2[,1]                        
Vis_Mens_Others_Pair2_Cor <- data.frame(cor(Vis_Mens_Others_Pair2))
print(Vis_Mens_Others_Pair2_Cor);rm(Vis_Mens_Others_Pair2,Visits_Mens_Others_Pair2)
write.csv(Vis_Mens_Others_Pair2_Cor,"C:/Yashwanth/Clustering/3.Correlation Matrix/5.Mens_Group_Five/Vis_Pair2_Others.csv")

                             #--------------------- # Pair Three #--------------------#

Visits_Mens_Others_Pair3 <- data.frame(subset(Visits_Mens_Others,
                                             Pages=="tall_dress_pants_for_men" | Pages=="shirts_men",
                                             names(Visits_Mens_Others)),row.names=NULL)
Vis_Mens_Others_Pair3 <- data.frame(t(Visits_Mens_Others_Pair3[,2:ncol(Visits_Mens_Others_Pair3)]),
                                   row.names=NULL)
names(Vis_Mens_Others_Pair3) <- Visits_Mens_Others_Pair3[,1]                        
Vis_Mens_Others_Pair3_Cor <- data.frame(cor(Vis_Mens_Others_Pair3))
print(Vis_Mens_Others_Pair3_Cor);rm(Vis_Mens_Others_Pair3,Visits_Mens_Others_Pair3)
write.csv(Vis_Mens_Others_Pair3_Cor,"C:/Yashwanth/Clustering/3.Correlation Matrix/5.Mens_Group_Five/Vis_Pair3_Others.csv")


                        #--------------------- # Pair Four #--------------------#


Visits_Mens_Others_Pair4 <- data.frame(subset(Visits_Mens_Others,
                                              Pages=="navy_pants_for_men" | Pages=="mens_running_shorts",
                                              names(Visits_Mens_Others)),row.names=NULL)
Vis_Mens_Others_Pair4 <- data.frame(t(Visits_Mens_Others_Pair4[,2:ncol(Visits_Mens_Others_Pair4)]),
                                    row.names=NULL)
names(Vis_Mens_Others_Pair4) <- Visits_Mens_Others_Pair4[,1]                        
Vis_Mens_Others_Pair4_Cor <- data.frame(cor(Vis_Mens_Others_Pair4))
print(Vis_Mens_Others_Pair4_Cor);rm(Vis_Mens_Others_Pair4,Visits_Mens_Others_Pair4)
write.csv(Vis_Mens_Others_Pair4_Cor,"C:/Yashwanth/Clustering/3.Correlation Matrix/5.Mens_Group_Five/Visits_Pair4_Others.csv")

                                  #--------------------- # Pair Five #--------------------#

Visits_Mens_Others_Pair5 <- data.frame(subset(Visits_Mens_Others,
                                              Pages=="mens_dress_pants" | Pages=="mens_swim_shorts",
                                              names(Visits_Mens_Others)),row.names=NULL)
Vis_Mens_Others_Pair5 <- data.frame(t(Visits_Mens_Others_Pair5[,2:ncol(Visits_Mens_Others_Pair5)]),
                                    row.names=NULL)
names(Vis_Mens_Others_Pair5) <- Visits_Mens_Others_Pair5[,1]                        
Vis_Mens_Others_Pair5_Cor <- data.frame(cor(Vis_Mens_Others_Pair5))
print(Vis_Mens_Others_Pair5_Cor);rm(Vis_Mens_Others_Pair5,Visits_Mens_Others_Pair5)
write.csv(Vis_Mens_Others_Pair5_Cor,"C:/Yashwanth/Clustering/3.Correlation Matrix/5.Mens_Group_Five/Visits_Pair5_Others.csv")
rm(list=ls(pattern="Cor"))

#-----------------------------------------------------------------------------------------------------------------
#------------------------------------------------- Product Page Views --------------------------------------------
#-----------------------------------------------------------------------------------------------------------------
                                #--------------------- # Pair One #--------------------#

Prod_Page_Views_Mens_Others_Pair1 <- data.frame(subset(Prod_Page_Views_Mens_Others,
                                              Pages=="mens_tall_shorts" | Pages=="big_and_tall_mens_shirts",
                                              names(Prod_Page_Views_Mens_Others)),row.names=NULL)
PPV_Mens_Others_Pair1 <- data.frame(t(Prod_Page_Views_Mens_Others_Pair1[,2:ncol(Prod_Page_Views_Mens_Others_Pair1)]),
                                   row.names=NULL)
names(PPV_Mens_Others_Pair1) <- Prod_Page_Views_Mens_Others_Pair1[,1]                        
PPV_Mens_Others_Pair1_Cor <- data.frame(cor(PPV_Mens_Others_Pair1))
print(PPV_Mens_Others_Pair1_Cor);rm(PPV_Mens_Others_Pair1,Prod_Page_Views_Mens_Others_Pair1)
write.csv(PPV_Mens_Others_Pair1_Cor,"C:/Yashwanth/Clustering/3.Correlation Matrix/5.Mens_Group_Five/PPV_Pair1_Others.csv")

                                #--------------------- # Pair Two #--------------------#

Prod_Page_Views_Mens_Others_Pair2 <- data.frame(subset(Prod_Page_Views_Mens_Others,
                                              Pages=="mens_fitted_dress_shirts" | Pages=="mens_cotton_pants",
                                              names(Prod_Page_Views_Mens_Others)),row.names=NULL)
PPV_Mens_Others_Pair2 <- data.frame(t(Prod_Page_Views_Mens_Others_Pair2[,2:ncol(Prod_Page_Views_Mens_Others_Pair2)]),
                                   row.names=NULL)
names(PPV_Mens_Others_Pair2) <- Prod_Page_Views_Mens_Others_Pair2[,1]                        
PPV_Mens_Others_Pair2_Cor <- data.frame(cor(PPV_Mens_Others_Pair2))
print(PPV_Mens_Others_Pair2_Cor);rm(PPV_Mens_Others_Pair2,Prod_Page_Views_Mens_Others_Pair2)
write.csv(PPV_Mens_Others_Pair2_Cor,"C:/Yashwanth/Clustering/3.Correlation Matrix/5.Mens_Group_Five/PPV_Pair2_Others.csv")

                                #--------------------- # Pair Three #--------------------#

Prod_Page_Views_Mens_Others_Pair3 <- data.frame(subset(Prod_Page_Views_Mens_Others,
                                              Pages=="tall_dress_pants_for_men" | Pages=="shirts_men",
                                              names(Prod_Page_Views_Mens_Others)),row.names=NULL)
PPV_Mens_Others_Pair3 <- data.frame(t(Prod_Page_Views_Mens_Others_Pair3[,2:ncol(Prod_Page_Views_Mens_Others_Pair3)]),
                                   row.names=NULL)
names(PPV_Mens_Others_Pair3) <- Prod_Page_Views_Mens_Others_Pair3[,1]                        
PPV_Mens_Others_Pair3_Cor <- data.frame(cor(PPV_Mens_Others_Pair3))
print(PPV_Mens_Others_Pair3_Cor);rm(PPV_Mens_Others_Pair3,Prod_Page_Views_Mens_Others_Pair3)
write.csv(PPV_Mens_Others_Pair3_Cor,"C:/Yashwanth/Clustering/3.Correlation Matrix/5.Mens_Group_Five/PPV_Pair3_Others.csv")

                                #--------------------- # Pair Four #--------------------#

Prod_Page_Views_Mens_Others_Pair4 <- data.frame(subset(Prod_Page_Views_Mens_Others,
                                               Pages=="navy_pants_for_men" | Pages=="mens_running_shorts",
                                               names(Prod_Page_Views_Mens_Others)),row.names=NULL)
PPV_Mens_Others_Pair4 <- data.frame(t(Prod_Page_Views_Mens_Others_Pair4[,2:ncol(Prod_Page_Views_Mens_Others_Pair4)]),
                                    row.names=NULL)
names(PPV_Mens_Others_Pair4) <- Prod_Page_Views_Mens_Others_Pair4[,1]                        
PPV_Mens_Others_Pair4_Cor <- data.frame(cor(PPV_Mens_Others_Pair4))
print(PPV_Mens_Others_Pair4_Cor);rm(PPV_Mens_Others_Pair4,Prod_Page_Views_Mens_Others_Pair4)
write.csv(PPV_Mens_Others_Pair4_Cor,"C:/Yashwanth/Clustering/3.Correlation Matrix/5.Mens_Group_Five/PPV_Pair4_Others.csv")



                              #--------------------- # Pair Five #--------------------#

Prod_Page_Views_Mens_Others_Pair5 <- data.frame(subset(Prod_Page_Views_Mens_Others,
                                               Pages=="mens_dress_pants" | Pages=="mens_swim_shorts",
                                               names(Prod_Page_Views_Mens_Others)),row.names=NULL)
PPV_Mens_Others_Pair5 <- data.frame(t(Prod_Page_Views_Mens_Others_Pair5[,2:ncol(Prod_Page_Views_Mens_Others_Pair5)]),
                                    row.names=NULL)
names(PPV_Mens_Others_Pair5) <- Prod_Page_Views_Mens_Others_Pair5[,1]                        
PPV_Mens_Others_Pair5_Cor <- data.frame(cor(PPV_Mens_Others_Pair5))
print(PPV_Mens_Others_Pair5_Cor);rm(PPV_Mens_Others_Pair5,Prod_Page_Views_Mens_Others_Pair5)
write.csv(PPV_Mens_Others_Pair5_Cor,"C:/Yashwanth/Clustering/3.Correlation Matrix/5.Mens_Group_Five/PPV_Pair5_Others.csv")
rm(list=ls(pattern="Cor"))

# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
