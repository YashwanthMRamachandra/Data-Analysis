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

Bounce_Rate_Mens_Jeans <- data.frame(subset(Bounce_Rate,
                                            Pages=="baggy_fit_jeans" | Pages=="loose_jeans" |
                                              Pages=="mens_button_fly_jeans" | Pages=="relaxed_fit_jeans" |
                                              Pages=="mens_low_rise_jeans" | Pages=="mens_slim_fit" |
                                              Pages=="button_fly_jeans_men" | Pages=="mens_tall_skinny_jeans" |  
                                              Pages=="tall_men_jeans" | Pages=="low_rise_mens_jeans" |
                                              Pages=="mens_carpenter_jeans" | Pages=="relaxed_jeans",select=names(Bounce_Rate)),row.names=NULL) 

Avg_Time_On_Page_Mens_Jeans <- data.frame(subset(Avg_Time_On_Page,
                                                 Pages=="baggy_fit_jeans" | Pages=="loose_jeans" |
                                                   Pages=="mens_button_fly_jeans" | Pages=="relaxed_fit_jeans" |
                                                   Pages=="mens_low_rise_jeans" | Pages=="mens_slim_fit" |
                                                   Pages=="button_fly_jeans_men" | Pages=="mens_tall_skinny_jeans" |  
                                                   Pages=="tall_men_jeans" | Pages=="low_rise_mens_jeans" |
                                                   Pages=="mens_carpenter_jeans" | Pages=="relaxed_jeans",select=names(Bounce_Rate)),row.names=NULL) 

Visits_Mens_Jeans <- data.frame(subset(Visits,
                                       Pages=="baggy_fit_jeans" | Pages=="loose_jeans" |
                                         Pages=="mens_button_fly_jeans" | Pages=="relaxed_fit_jeans" |
                                         Pages=="mens_low_rise_jeans" | Pages=="mens_slim_fit" |
                                         Pages=="button_fly_jeans_men" | Pages=="mens_tall_skinny_jeans" |  
                                         Pages=="tall_men_jeans" | Pages=="low_rise_mens_jeans" |
                                         Pages=="mens_carpenter_jeans" | Pages=="relaxed_jeans",select=names(Bounce_Rate)),row.names=NULL) 

Prod_Page_Views_Mens_Jeans <- data.frame(subset(Prod_Page_Views,
                                                Pages=="baggy_fit_jeans" | Pages=="loose_jeans" |
                                                  Pages=="mens_button_fly_jeans" | Pages=="relaxed_fit_jeans" |
                                                  Pages=="mens_low_rise_jeans" | Pages=="mens_slim_fit" |
                                                  Pages=="button_fly_jeans_men" | Pages=="mens_tall_skinny_jeans" |  
                                                  Pages=="tall_men_jeans" | Pages=="low_rise_mens_jeans" |
                                                  Pages=="mens_carpenter_jeans" | Pages=="relaxed_jeans",select=names(Bounce_Rate)),row.names=NULL) 
rm(Bounce_Rate,Avg_Time_On_Page,Visits,Prod_Page_Views)

# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
#                                       || Correlation matrix - One ||
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------

rownames(GAP_Pages_Mens_Jeans) <- GAP_Pages_Mens_Jeans[,1]
GAP_Pages_Mens_Jeans <- GAP_Pages_Mens_Jeans[,-1]
GAP_Pages_Mens_Jeans_T <- data.frame(t(GAP_Pages_Mens_Jeans))
GAP_Pages_Mens_Jeans_Cor <- data.frame(cor(GAP_Pages_Mens_Jeans))
write.csv(GAP_Pages_Mens_Jeans_Cor,"C:/Yashwanth/Clustering/Correlation Matrix/Corelation_Pages_Mens_Jeans.csv")
rm(list=ls(pattern="GAP"))

# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
#                                       || Correlation matrix  - Two ||
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------

#-----------------------------------------------------------------------------------------------------------------
#------------------------------------------------- Bounce Rate ------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------
                            --------------------- # Pair One #--------------------

Bounce_Rate_Mens_Jeans_Pair1 <- data.frame(subset(Bounce_Rate_Mens_Jeans,
                                                  Pages=="baggy_fit_jeans" | Pages=="mens_tall_skinny_jeans",
                                                  names(Bounce_Rate_Mens_Jeans)),row.names=NULL)
BR_Mens_Jeans_Pair1 <- data.frame(t(Bounce_Rate_Mens_Jeans_Pair1[,2:ncol(Bounce_Rate_Mens_Jeans_Pair1)]),
                                           row.names=NULL)
names(BR_Mens_Jeans_Pair1) <- Bounce_Rate_Mens_Jeans_Pair1[,1]                        
BR_Mens_Jeans_Pair1_Cor <- data.frame(cor(BR_Mens_Jeans_Pair1))
print(BR_Mens_Jeans_Pair1_Cor);rm(BR_Mens_Jeans_Pair1,Bounce_Rate_Mens_Jeans_Pair1)
write.csv(BR_Mens_Jeans_Pair1_Cor,"C:/Yashwanth/Clustering/Correlation Matrix/BR_Pair1.csv")

                            --------------------- # Pair Two #--------------------

Bounce_Rate_Mens_Jeans_Pair2 <- data.frame(subset(Bounce_Rate_Mens_Jeans,
                                                  Pages=="loose_jeans" | Pages=="relaxed_fit_jeans",
                                                  names(Bounce_Rate_Mens_Jeans)),row.names=NULL)
BR_Mens_Jeans_Pair2 <- data.frame(t(Bounce_Rate_Mens_Jeans_Pair2[,2:ncol(Bounce_Rate_Mens_Jeans_Pair2)]),
                                  row.names=NULL)
names(BR_Mens_Jeans_Pair2) <- Bounce_Rate_Mens_Jeans_Pair2[,1]                        
BR_Mens_Jeans_Pair2_Cor <- data.frame(cor(BR_Mens_Jeans_Pair2))
print(BR_Mens_Jeans_Pair2_Cor);rm(BR_Mens_Jeans_Pair2,Bounce_Rate_Mens_Jeans_Pair2)
write.csv(BR_Mens_Jeans_Pair2_Cor,"C:/Yashwanth/Clustering/Correlation Matrix/BR_Pair2.csv")

                              --------------------- # Pair Three #--------------------

Bounce_Rate_Mens_Jeans_Pair3 <- data.frame(subset(Bounce_Rate_Mens_Jeans,
                                                  Pages=="mens_button_fly_jeans" | Pages=="tall_men_jeans",
                                                  names(Bounce_Rate_Mens_Jeans)),row.names=NULL)
BR_Mens_Jeans_Pair3 <- data.frame(t(Bounce_Rate_Mens_Jeans_Pair3[,2:ncol(Bounce_Rate_Mens_Jeans_Pair3)]),
                                  row.names=NULL)
names(BR_Mens_Jeans_Pair3) <- Bounce_Rate_Mens_Jeans_Pair3[,1]                        
BR_Mens_Jeans_Pair3_Cor <- data.frame(cor(BR_Mens_Jeans_Pair3))
print(BR_Mens_Jeans_Pair3_Cor);rm(BR_Mens_Jeans_Pair3,Bounce_Rate_Mens_Jeans_Pair3)
write.csv(BR_Mens_Jeans_Pair3_Cor,"C:/Yashwanth/Clustering/Correlation Matrix/BR_Pair3.csv")

                              --------------------- # Pair Four #--------------------

Bounce_Rate_Mens_Jeans_Pair4 <- data.frame(subset(Bounce_Rate_Mens_Jeans,
                                                  Pages=="mens_low_rise_jeans" | Pages=="mens_carpenter_jeans",
                                                  names(Bounce_Rate_Mens_Jeans)),row.names=NULL)
BR_Mens_Jeans_Pair4 <- data.frame(t(Bounce_Rate_Mens_Jeans_Pair4[,2:ncol(Bounce_Rate_Mens_Jeans_Pair4)]),
                                  row.names=NULL)
names(BR_Mens_Jeans_Pair4) <- Bounce_Rate_Mens_Jeans_Pair4[,1]                        
BR_Mens_Jeans_Pair4_Cor <- data.frame(cor(BR_Mens_Jeans_Pair4))
print(BR_Mens_Jeans_Pair4_Cor);rm(BR_Mens_Jeans_Pair4,Bounce_Rate_Mens_Jeans_Pair4)
write.csv(BR_Mens_Jeans_Pair4_Cor,"C:/Yashwanth/Clustering/Correlation Matrix/BR_Pair4.csv")

                              --------------------- # Pair Five #--------------------

Bounce_Rate_Mens_Jeans_Pair5 <- data.frame(subset(Bounce_Rate_Mens_Jeans,
                                                  Pages=="mens_slim_fit" | Pages=="relaxed_jeans",
                                                  names(Bounce_Rate_Mens_Jeans)),row.names=NULL)
BR_Mens_Jeans_Pair5 <- data.frame(t(Bounce_Rate_Mens_Jeans_Pair5[,2:ncol(Bounce_Rate_Mens_Jeans_Pair5)]),
                                  row.names=NULL)
names(BR_Mens_Jeans_Pair5) <- Bounce_Rate_Mens_Jeans_Pair5[,1]                        
BR_Mens_Jeans_Pair5_Cor <- data.frame(cor(BR_Mens_Jeans_Pair5))
print(BR_Mens_Jeans_Pair5_Cor);rm(BR_Mens_Jeans_Pair5,Bounce_Rate_Mens_Jeans_Pair5)
write.csv(BR_Mens_Jeans_Pair5_Cor,"C:/Yashwanth/Clustering/Correlation Matrix/BR_Pair5.csv")

                                --------------------- # Pair Six #--------------------

Bounce_Rate_Mens_Jeans_Pair6 <- data.frame(subset(Bounce_Rate_Mens_Jeans,
                                                  Pages=="button_fly_jeans_men" | Pages=="low_rise_mens_jeans",
                                                  names(Bounce_Rate_Mens_Jeans)),row.names=NULL)
BR_Mens_Jeans_Pair6 <- data.frame(t(Bounce_Rate_Mens_Jeans_Pair6[,2:ncol(Bounce_Rate_Mens_Jeans_Pair6)]),
                                  row.names=NULL)
names(BR_Mens_Jeans_Pair6) <- Bounce_Rate_Mens_Jeans_Pair6[,1]                        
BR_Mens_Jeans_Pair6_Cor <- data.frame(cor(BR_Mens_Jeans_Pair6))
print(BR_Mens_Jeans_Pair6_Cor);rm(BR_Mens_Jeans_Pair6,Bounce_Rate_Mens_Jeans_Pair6)
write.csv(BR_Mens_Jeans_Pair6_Cor,"C:/Yashwanth/Clustering/Correlation Matrix/BR_Pair6.csv")
rm(list=ls(pattern="Cor"))

#-----------------------------------------------------------------------------------------------------------------
#------------------------------------------------- Avg TOP ------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------
                              --------------------- # Pair One #--------------------

Avg_TOP_Mens_Jeans_Pair1 <- data.frame(subset(Avg_TOP_Mens_Jeans,
                                              Pages=="baggy_fit_jeans" | Pages=="mens_tall_skinny_jeans",
                                              names(Avg_TOP_Mens_Jeans)),row.names=NULL)
ATOP_Mens_Jeans_Pair1 <- data.frame(t(Avg_TOP_Mens_Jeans_Pair1[,2:ncol(Avg_TOP_Mens_Jeans_Pair1)]),
                                    row.names=NULL)
names(ATOP_Mens_Jeans_Pair1) <- Avg_TOP_Mens_Jeans_Pair1[,1]                        
ATOP_Mens_Jeans_Pair1_Cor <- data.frame(cor(ATOP_Mens_Jeans_Pair1))
print(ATOP_Mens_Jeans_Pair1_Cor);rm(ATOP_Mens_Jeans_Pair1,Avg_TOP_Mens_Jeans_Pair1)
write.csv(ATOP_Mens_Jeans_Pair1_Cor,"C:/Yashwanth/Clustering/Correlation Matrix/ATOP_Pair1.csv")

                              --------------------- # Pair Two #--------------------

Avg_TOP_Mens_Jeans_Pair2 <- data.frame(subset(Avg_TOP_Mens_Jeans,
                                              Pages=="loose_jeans" | Pages=="relaxed_fit_jeans",
                                              names(Avg_TOP_Mens_Jeans)),row.names=NULL)
ATOP_Mens_Jeans_Pair2 <- data.frame(t(Avg_TOP_Mens_Jeans_Pair2[,2:ncol(Avg_TOP_Mens_Jeans_Pair2)]),
                                    row.names=NULL)
names(ATOP_Mens_Jeans_Pair2) <- Avg_TOP_Mens_Jeans_Pair2[,1]                        
ATOP_Mens_Jeans_Pair2_Cor <- data.frame(cor(ATOP_Mens_Jeans_Pair2))
print(ATOP_Mens_Jeans_Pair2_Cor);rm(ATOP_Mens_Jeans_Pair2,Avg_TOP_Mens_Jeans_Pair2)
write.csv(ATOP_Mens_Jeans_Pair2_Cor,"C:/Yashwanth/Clustering/Correlation Matrix/ATOP_Pair2.csv")

                                --------------------- # Pair Three #--------------------

Avg_TOP_Mens_Jeans_Pair3 <- data.frame(subset(Avg_TOP_Mens_Jeans,
                                              Pages=="mens_button_fly_jeans" | Pages=="tall_men_jeans",
                                              names(Avg_TOP_Mens_Jeans)),row.names=NULL)
ATOP_Mens_Jeans_Pair3 <- data.frame(t(Avg_TOP_Mens_Jeans_Pair3[,2:ncol(Avg_TOP_Mens_Jeans_Pair3)]),
                                    row.names=NULL)
names(ATOP_Mens_Jeans_Pair3) <- Avg_TOP_Mens_Jeans_Pair3[,1]                        
ATOP_Mens_Jeans_Pair3_Cor <- data.frame(cor(ATOP_Mens_Jeans_Pair3))
print(ATOP_Mens_Jeans_Pair3_Cor);rm(ATOP_Mens_Jeans_Pair3,Avg_TOP_Mens_Jeans_Pair3)
write.csv(ATOP_Mens_Jeans_Pair3_Cor,"C:/Yashwanth/Clustering/Correlation Matrix/ATOP_Pair3.csv")

                                --------------------- # Pair Four #--------------------

Avg_TOP_Mens_Jeans_Pair4 <- data.frame(subset(Avg_TOP_Mens_Jeans,
                                              Pages=="mens_low_rise_jeans" | Pages=="mens_carpenter_jeans",
                                              names(Avg_TOP_Mens_Jeans)),row.names=NULL)
ATOP_Mens_Jeans_Pair4 <- data.frame(t(Avg_TOP_Mens_Jeans_Pair4[,2:ncol(Avg_TOP_Mens_Jeans_Pair4)]),
                                    row.names=NULL)
names(ATOP_Mens_Jeans_Pair4) <- Avg_TOP_Mens_Jeans_Pair4[,1]                        
ATOP_Mens_Jeans_Pair4_Cor <- data.frame(cor(ATOP_Mens_Jeans_Pair4))
print(ATOP_Mens_Jeans_Pair4_Cor);rm(ATOP_Mens_Jeans_Pair4,Avg_TOP_Mens_Jeans_Pair4)
write.csv(ATOP_Mens_Jeans_Pair4_Cor,"C:/Yashwanth/Clustering/Correlation Matrix/ATOP_Pair4.csv")

                                  --------------------- # Pair Five #--------------------

Avg_TOP_Mens_Jeans_Pair5 <- data.frame(subset(Avg_TOP_Mens_Jeans,
                                              Pages=="mens_slim_fit" | Pages=="relaxed_jeans",
                                              names(Avg_TOP_Mens_Jeans)),row.names=NULL)
ATOP_Mens_Jeans_Pair5 <- data.frame(t(Avg_TOP_Mens_Jeans_Pair5[,2:ncol(Avg_TOP_Mens_Jeans_Pair5)]),
                                    row.names=NULL)
names(ATOP_Mens_Jeans_Pair5) <- Avg_TOP_Mens_Jeans_Pair5[,1]                        
ATOP_Mens_Jeans_Pair5_Cor <- data.frame(cor(ATOP_Mens_Jeans_Pair5))
print(ATOP_Mens_Jeans_Pair5_Cor);rm(ATOP_Mens_Jeans_Pair5,Avg_TOP_Mens_Jeans_Pair5)
write.csv(ATOP_Mens_Jeans_Pair5_Cor,"C:/Yashwanth/Clustering/Correlation Matrix/ATOP_Pair5.csv")

                                    --------------------- # Pair Six #--------------------

Avg_TOP_Mens_Jeans_Pair6 <- data.frame(subset(Avg_TOP_Mens_Jeans,
                                              Pages=="button_fly_jeans_men" | Pages=="low_rise_mens_jeans",
                                              names(Avg_TOP_Mens_Jeans)),row.names=NULL)
ATOP_Mens_Jeans_Pair6 <- data.frame(t(Avg_TOP_Mens_Jeans_Pair6[,2:ncol(Avg_TOP_Mens_Jeans_Pair6)]),
                                    row.names=NULL)
names(ATOP_Mens_Jeans_Pair6) <- Avg_TOP_Mens_Jeans_Pair6[,1]                        
ATOP_Mens_Jeans_Pair6_Cor <- data.frame(cor(ATOP_Mens_Jeans_Pair6))
print(ATOP_Mens_Jeans_Pair6_Cor);rm(ATOP_Mens_Jeans_Pair6,Avg_TOP_Mens_Jeans_Pair6)
write.csv(ATOP_Mens_Jeans_Pair6_Cor,"C:/Yashwanth/Clustering/Correlation Matrix/ATOP_Pair6.csv")
rm(list=ls(pattern="Cor"))

#-----------------------------------------------------------------------------------------------------------------
#------------------------------------------------- Visits------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------
                                    --------------------- # Pair One #--------------------

Visits_Mens_Jeans_Pair1 <- data.frame(subset(Visits_Mens_Jeans,
                                             Pages=="baggy_fit_jeans" | Pages=="mens_tall_skinny_jeans",
                                             names(Visits_Mens_Jeans)),row.names=NULL)
Vis_Mens_Jeans_Pair1 <- data.frame(t(Visits_Mens_Jeans_Pair1[,2:ncol(Visits_Mens_Jeans_Pair1)]),
                                   row.names=NULL)
names(Vis_Mens_Jeans_Pair1) <- Visits_Mens_Jeans_Pair1[,1]                        
Vis_Mens_Jeans_Pair1_Cor <- data.frame(cor(Vis_Mens_Jeans_Pair1))
print(Vis_Mens_Jeans_Pair1_Cor);rm(Vis_Mens_Jeans_Pair1,Visits_Mens_Jeans_Pair1)
write.csv(Vis_Mens_Jeans_Pair1_Cor,"C:/Yashwanth/Clustering/Correlation Matrix/Vis_Pair1.csv")

                                    --------------------- # Pair Two #--------------------

Visits_Mens_Jeans_Pair2 <- data.frame(subset(Visits_Mens_Jeans,
                                             Pages=="loose_jeans" | Pages=="relaxed_fit_jeans",
                                             names(Visits_Mens_Jeans)),row.names=NULL)
Vis_Mens_Jeans_Pair2 <- data.frame(t(Visits_Mens_Jeans_Pair2[,2:ncol(Visits_Mens_Jeans_Pair2)]),
                                   row.names=NULL)
names(Vis_Mens_Jeans_Pair2) <- Visits_Mens_Jeans_Pair2[,1]                        
Vis_Mens_Jeans_Pair2_Cor <- data.frame(cor(Vis_Mens_Jeans_Pair2))
print(Vis_Mens_Jeans_Pair2_Cor);rm(Vis_Mens_Jeans_Pair2,Visits_Mens_Jeans_Pair2)
write.csv(Vis_Mens_Jeans_Pair2_Cor,"C:/Yashwanth/Clustering/Correlation Matrix/Vis_Pair2.csv")

                                    --------------------- # Pair Three #--------------------

Visits_Mens_Jeans_Pair3 <- data.frame(subset(Visits_Mens_Jeans,
                                             Pages=="mens_button_fly_jeans" | Pages=="tall_men_jeans",
                                             names(Visits_Mens_Jeans)),row.names=NULL)
Vis_Mens_Jeans_Pair3 <- data.frame(t(Visits_Mens_Jeans_Pair3[,2:ncol(Visits_Mens_Jeans_Pair3)]),
                                   row.names=NULL)
names(Vis_Mens_Jeans_Pair3) <- Visits_Mens_Jeans_Pair3[,1]                        
Vis_Mens_Jeans_Pair3_Cor <- data.frame(cor(Vis_Mens_Jeans_Pair3))
print(Vis_Mens_Jeans_Pair3_Cor);rm(Vis_Mens_Jeans_Pair3,Visits_Mens_Jeans_Pair3)
write.csv(Vis_Mens_Jeans_Pair3_Cor,"C:/Yashwanth/Clustering/Correlation Matrix/Vis_Pair3.csv")

                                  --------------------- # Pair Four #--------------------

Visits_Mens_Jeans_Pair4 <- data.frame(subset(Visits_Mens_Jeans,
                                             Pages=="mens_low_rise_jeans" | Pages=="mens_carpenter_jeans",
                                             names(Visits_Mens_Jeans)),row.names=NULL)
Vis_Mens_Jeans_Pair4 <- data.frame(t(Visits_Mens_Jeans_Pair4[,2:ncol(Visits_Mens_Jeans_Pair4)]),
                                   row.names=NULL)
names(Vis_Mens_Jeans_Pair4) <- Visits_Mens_Jeans_Pair4[,1]                        
Vis_Mens_Jeans_Pair4_Cor <- data.frame(cor(Vis_Mens_Jeans_Pair4))
print(Vis_Mens_Jeans_Pair4_Cor);rm(Vis_Mens_Jeans_Pair4,Visits_Mens_Jeans_Pair4)
write.csv(Vis_Mens_Jeans_Pair4_Cor,"C:/Yashwanth/Clustering/Correlation Matrix/Vis_Pair4.csv")

                                --------------------- # Pair Five #--------------------

Visits_Mens_Jeans_Pair5 <- data.frame(subset(Visits_Mens_Jeans,
                                             Pages=="mens_slim_fit" | Pages=="relaxed_jeans",
                                             names(Visits_Mens_Jeans)),row.names=NULL)
Vis_Mens_Jeans_Pair5 <- data.frame(t(Visits_Mens_Jeans_Pair5[,2:ncol(Visits_Mens_Jeans_Pair5)]),
                                   row.names=NULL)
names(Vis_Mens_Jeans_Pair5) <- Visits_Mens_Jeans_Pair5[,1]                        
Vis_Mens_Jeans_Pair5_Cor <- data.frame(cor(Vis_Mens_Jeans_Pair5))
print(Vis_Mens_Jeans_Pair5_Cor);rm(Vis_Mens_Jeans_Pair5,Visits_Mens_Jeans_Pair5)
write.csv(Vis_Mens_Jeans_Pair5_Cor,"C:/Yashwanth/Clustering/Correlation Matrix/Vis_Pair5.csv")

                                --------------------- # Pair Six #--------------------

Visits_Mens_Jeans_Pair6 <- data.frame(subset(Visits_Mens_Jeans,
                                             Pages=="button_fly_jeans_men" | Pages=="low_rise_mens_jeans",
                                             names(Visits_Mens_Jeans)),row.names=NULL)
Vis_Mens_Jeans_Pair6 <- data.frame(t(Visits_Mens_Jeans_Pair6[,2:ncol(Visits_Mens_Jeans_Pair6)]),
                                   row.names=NULL)
names(Vis_Mens_Jeans_Pair6) <- Visits_Mens_Jeans_Pair6[,1]                        
Vis_Mens_Jeans_Pair6_Cor <- data.frame(cor(Vis_Mens_Jeans_Pair6))
print(Vis_Mens_Jeans_Pair6_Cor);rm(Vis_Mens_Jeans_Pair6,Visits_Mens_Jeans_Pair6)
write.csv(Vis_Mens_Jeans_Pair6_Cor,"C:/Yashwanth/Clustering/Correlation Matrix/Vis_Pair6.csv")
rm(list=ls(pattern="Cor"))

#-----------------------------------------------------------------------------------------------------------------
#------------------------------------------------- Product Page Views --------------------------------------------
#-----------------------------------------------------------------------------------------------------------------
                                      --------------------- # Pair One #--------------------

Prod_PV_Mens_Jeans_Pair1 <- data.frame(subset(Prod_Page_Views_Mens_Jeans,
                                              Pages=="baggy_fit_jeans" | Pages=="mens_tall_skinny_jeans",
                                              names(Prod_Page_Views_Mens_Jeans)),row.names=NULL)
PPV_Mens_Jeans_Pair1 <- data.frame(t(Prod_PV_Mens_Jeans_Pair1[,2:ncol(Prod_PV_Mens_Jeans_Pair1)]),
                                   row.names=NULL)
names(PPV_Mens_Jeans_Pair1) <- Prod_PV_Mens_Jeans_Pair1[,1]                        
PPV_Mens_Jeans_Pair1_Cor <- data.frame(cor(PPV_Mens_Jeans_Pair1))
print(PPV_Mens_Jeans_Pair1_Cor);rm(PPV_Mens_Jeans_Pair1,Prod_PV_Mens_Jeans_Pair1)
write.csv(PPV_Mens_Jeans_Pair1_Cor,"C:/Yashwanth/Clustering/Correlation Matrix/PPV_Pair1.csv")

                                    --------------------- # Pair Two #--------------------

Prod_PV_Mens_Jeans_Pair2 <- data.frame(subset(Prod_Page_Views_Mens_Jeans,
                                              Pages=="loose_jeans" | Pages=="relaxed_fit_jeans",
                                              names(Prod_Page_Views_Mens_Jeans)),row.names=NULL)
PPV_Mens_Jeans_Pair2 <- data.frame(t(Prod_PV_Mens_Jeans_Pair2[,2:ncol(Prod_PV_Mens_Jeans_Pair2)]),
                                   row.names=NULL)
names(PPV_Mens_Jeans_Pair2) <- Prod_PV_Mens_Jeans_Pair2[,1]                        
PPV_Mens_Jeans_Pair2_Cor <- data.frame(cor(PPV_Mens_Jeans_Pair2))
print(PPV_Mens_Jeans_Pair2_Cor);rm(PPV_Mens_Jeans_Pair2,Prod_PV_Mens_Jeans_Pair2)
write.csv(PPV_Mens_Jeans_Pair2_Cor,"C:/Yashwanth/Clustering/Correlation Matrix/PPV_Pair2.csv")

                                    --------------------- # Pair Three #--------------------

Prod_PV_Mens_Jeans_Pair3 <- data.frame(subset(Prod_Page_Views_Mens_Jeans,
                                              Pages=="mens_button_fly_jeans" | Pages=="tall_men_jeans",
                                              names(Prod_Page_Views_Mens_Jeans)),row.names=NULL)
PPV_Mens_Jeans_Pair3 <- data.frame(t(Prod_PV_Mens_Jeans_Pair3[,2:ncol(Prod_PV_Mens_Jeans_Pair3)]),
                                   row.names=NULL)
names(PPV_Mens_Jeans_Pair3) <- Prod_PV_Mens_Jeans_Pair3[,1]                        
PPV_Mens_Jeans_Pair3_Cor <- data.frame(cor(PPV_Mens_Jeans_Pair3))
print(PPV_Mens_Jeans_Pair3_Cor);rm(PPV_Mens_Jeans_Pair3,Prod_PV_Mens_Jeans_Pair3)
write.csv(PPV_Mens_Jeans_Pair3_Cor,"C:/Yashwanth/Clustering/Correlation Matrix/PPV_Pair3.csv")

                                    --------------------- # Pair Four #--------------------

Prod_PV_Mens_Jeans_Pair4 <- data.frame(subset(Prod_Page_Views_Mens_Jeans,
                                              Pages=="mens_low_rise_jeans" | Pages=="mens_carpenter_jeans",
                                              names(Prod_Page_Views_Mens_Jeans)),row.names=NULL)
PPV_Mens_Jeans_Pair4 <- data.frame(t(Prod_PV_Mens_Jeans_Pair4[,2:ncol(Prod_PV_Mens_Jeans_Pair4)]),
                                   row.names=NULL)
names(PPV_Mens_Jeans_Pair4) <- Prod_PV_Mens_Jeans_Pair4[,1]                        
PPV_Mens_Jeans_Pair4_Cor <- data.frame(cor(PPV_Mens_Jeans_Pair4))
print(PPV_Mens_Jeans_Pair4_Cor);rm(PPV_Mens_Jeans_Pair4,Prod_PV_Mens_Jeans_Pair4)
write.csv(PPV_Mens_Jeans_Pair4_Cor,"C:/Yashwanth/Clustering/Correlation Matrix/PPV_Pair4.csv")

                                  --------------------- # Pair Five #--------------------

Prod_PV_Mens_Jeans_Pair5 <- data.frame(subset(Prod_Page_Views_Mens_Jeans,
                                              Pages=="mens_slim_fit" | Pages=="relaxed_jeans",
                                              names(Prod_Page_Views_Mens_Jeans)),row.names=NULL)
PPV_Mens_Jeans_Pair5 <- data.frame(t(Prod_PV_Mens_Jeans_Pair5[,2:ncol(Prod_PV_Mens_Jeans_Pair5)]),
                                   row.names=NULL)
names(PPV_Mens_Jeans_Pair5) <- Prod_PV_Mens_Jeans_Pair5[,1]                        
PPV_Mens_Jeans_Pair5_Cor <- data.frame(cor(PPV_Mens_Jeans_Pair5))
print(PPV_Mens_Jeans_Pair5_Cor);rm(PPV_Mens_Jeans_Pair5,Prod_PV_Mens_Jeans_Pair5)
write.csv(PPV_Mens_Jeans_Pair5_Cor,"C:/Yashwanth/Clustering/Correlation Matrix/PPV_Pair5.csv")

                                --------------------- # Pair Six #--------------------

Prod_PV_Mens_Jeans_Pair6 <- data.frame(subset(Prod_Page_Views_Mens_Jeans,
                                              Pages=="button_fly_jeans_men" | Pages=="low_rise_mens_jeans",
                                              names(Prod_Page_Views_Mens_Jeans)),row.names=NULL)
PPV_Mens_Jeans_Pair6 <- data.frame(t(Prod_PV_Mens_Jeans_Pair6[,2:ncol(Prod_PV_Mens_Jeans_Pair6)]),
                                   row.names=NULL)
names(PPV_Mens_Jeans_Pair6) <- Prod_PV_Mens_Jeans_Pair6[,1]                        
PPV_Mens_Jeans_Pair6_Cor <- data.frame(cor(PPV_Mens_Jeans_Pair6))
print(PPV_Mens_Jeans_Pair6_Cor);rm(PPV_Mens_Jeans_Pair6,Prod_PV_Mens_Jeans_Pair6)
write.csv(PPV_Mens_Jeans_Pair6_Cor,"C:/Yashwanth/Clustering/Correlation Matrix/PPV_Pair6.csv")
rm(list=ls(pattern="Cor"))

# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
