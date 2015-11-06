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

Bounce_Rate_Mens_Pants <- data.frame(subset(Bounce_Rate,
                                            Pages=="men_jean_leggings" | Pages=="mens_linen_pants" |
                                              Pages=="mens_cargo_pants" | Pages=="mens_sweat_pants" |
                                              Pages=="mens_capri_pants" | Pages=="mens_khakis" |
                                              Pages=="mens_lounge_pants" | Pages=="mens_slim_fit_pants",select=names(Bounce_Rate)),row.names=NULL) 

Avg_Time_On_Page_Mens_Pants <- data.frame(subset(Avg_Time_On_Page,
                                                 Pages=="men_jean_leggings" | Pages=="mens_linen_pants" |
                                                   Pages=="mens_cargo_pants" | Pages=="mens_sweat_pants" |
                                                   Pages=="mens_capri_pants" | Pages=="mens_khakis" |
                                                   Pages=="mens_lounge_pants" | Pages=="mens_slim_fit_pants",select=names(Bounce_Rate)),row.names=NULL) 

Visits_Mens_Pants <- data.frame(subset(Visits,
                                       Pages=="men_jean_leggings" | Pages=="mens_linen_pants" |
                                         Pages=="mens_cargo_pants" | Pages=="mens_sweat_pants" |
                                         Pages=="mens_capri_pants" | Pages=="mens_khakis" |
                                         Pages=="mens_lounge_pants" | Pages=="mens_slim_fit_pants",select=names(Bounce_Rate)),row.names=NULL) 

Prod_Page_Views_Mens_Pants <- data.frame(subset(Prod_Page_Views,
                                                Pages=="men_jean_leggings" | Pages=="mens_linen_pants" |
                                                  Pages=="mens_cargo_pants" | Pages=="mens_sweat_pants" |
                                                  Pages=="mens_capri_pants" | Pages=="mens_khakis" |
                                                  Pages=="mens_lounge_pants" | Pages=="mens_slim_fit_pants",select=names(Bounce_Rate)),row.names=NULL) 

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
write.csv(GAP_Pages_Mens_Pants_Cor,"C:/Yashwanth/Clustering/3.Correlation Matrix/4.Mens_Pants_Group_Four/Corelation_Pages_Mens_Pants.csv")
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

Bounce_Rate_Mens_Pants_Pair1 <- data.frame(subset(Bounce_Rate_Mens_Pants,
                                                  Pages=="mens_linen_pants" | Pages=="men_jean_leggings",
                                                    names(Bounce_Rate_Mens_Pants)),row.names=NULL)
BR_Mens_Pants_Pair1 <- data.frame(t(Bounce_Rate_Mens_Pants_Pair1[,2:ncol(Bounce_Rate_Mens_Pants_Pair1)]),
                                  row.names=NULL)
names(BR_Mens_Pants_Pair1) <- Bounce_Rate_Mens_Pants_Pair1[,1]                        
BR_Mens_Pants_Pair1_Cor <- data.frame(cor(BR_Mens_Pants_Pair1))
print(BR_Mens_Pants_Pair1_Cor);rm(BR_Mens_Pants_Pair1,Bounce_Rate_Mens_Pants_Pair1)
write.csv(BR_Mens_Pants_Pair1_Cor,"C:/Yashwanth/Clustering/3.Correlation Matrix/4.Mens_Pants_Group_Four/BR_Pair1_Pants.csv")

                              #--------------------- # Pair Two #--------------------#

Bounce_Rate_Mens_Pants_Pair2 <- data.frame(subset(Bounce_Rate_Mens_Pants,
                                                  Pages=="mens_sweat_pants" | Pages=="mens_cargo_pants",
                                                  names(Bounce_Rate_Mens_Pants)),row.names=NULL)
BR_Mens_Pants_Pair2 <- data.frame(t(Bounce_Rate_Mens_Pants_Pair2[,2:ncol(Bounce_Rate_Mens_Pants_Pair2)]),
                                  row.names=NULL)
names(BR_Mens_Pants_Pair2) <- Bounce_Rate_Mens_Pants_Pair2[,1]                        
BR_Mens_Pants_Pair2_Cor <- data.frame(cor(BR_Mens_Pants_Pair2))
print(BR_Mens_Pants_Pair2_Cor);rm(BR_Mens_Pants_Pair2,Bounce_Rate_Mens_Pants_Pair2)
write.csv(BR_Mens_Pants_Pair2_Cor,"C:/Yashwanth/Clustering/3.Correlation Matrix/4.Mens_Pants_Group_Four/BR_Pair2_Pants.csv")

                                  #--------------------- # Pair Three #--------------------#

Bounce_Rate_Mens_Pants_Pair3 <- data.frame(subset(Bounce_Rate_Mens_Pants,
                                                  Pages=="mens_lounge_pants" | Pages=="mens_capri_pants",
                                                  names(Bounce_Rate_Mens_Pants)),row.names=NULL)
BR_Mens_Pants_Pair3 <- data.frame(t(Bounce_Rate_Mens_Pants_Pair3[,2:ncol(Bounce_Rate_Mens_Pants_Pair3)]),
                                  row.names=NULL)
names(BR_Mens_Pants_Pair3) <- Bounce_Rate_Mens_Pants_Pair3[,1]                        
BR_Mens_Pants_Pair3_Cor <- data.frame(cor(BR_Mens_Pants_Pair3))
print(BR_Mens_Pants_Pair3_Cor);rm(BR_Mens_Pants_Pair3,Bounce_Rate_Mens_Pants_Pair3)
write.csv(BR_Mens_Pants_Pair3_Cor,"C:/Yashwanth/Clustering/3.Correlation Matrix/4.Mens_Pants_Group_Four/BR_Pair3_Pants.csv")
rm(list=ls(pattern="Cor"))

#-----------------------------------------------------------------------------------------------------------------
#------------------------------------------------- Avg TOP ------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------
                                  #--------------------- # Pair One #--------------------#

Avg_Time_On_Page_Mens_Pants_Pair1 <- data.frame(subset(Avg_Time_On_Page_Mens_Pants,
                                              Pages=="mens_linen_pants" | Pages=="men_jean_leggings",
                                              names(Avg_Time_On_Page_Mens_Pants)),row.names=NULL)
ATOP_Mens_Pants_Pair1 <- data.frame(t(Avg_Time_On_Page_Mens_Pants_Pair1[,2:ncol(Avg_Time_On_Page_Mens_Pants_Pair1)]),
                                    row.names=NULL)
names(ATOP_Mens_Pants_Pair1) <- Avg_Time_On_Page_Mens_Pants_Pair1[,1]                        
ATOP_Mens_Pants_Pair1_Cor <- data.frame(cor(ATOP_Mens_Pants_Pair1))
print(ATOP_Mens_Pants_Pair1_Cor);rm(ATOP_Mens_Pants_Pair1,Avg_Time_On_Page_Mens_Pants_Pair1)
write.csv(ATOP_Mens_Pants_Pair1_Cor,"C:/Yashwanth/Clustering/3.Correlation Matrix/4.Mens_Pants_Group_Four/ATOP_Pair1_Pants.csv")

                                  #--------------------- # Pair Two #--------------------#

Avg_Time_On_Page_Mens_Pants_Pair2 <- data.frame(subset(Avg_Time_On_Page_Mens_Pants,
                                              Pages=="mens_sweat_pants" | Pages=="mens_cargo_pants",
                                              names(Avg_Time_On_Page_Mens_Pants)),row.names=NULL)
ATOP_Mens_Pants_Pair2 <- data.frame(t(Avg_Time_On_Page_Mens_Pants_Pair2[,2:ncol(Avg_Time_On_Page_Mens_Pants_Pair2)]),
                                    row.names=NULL)
names(ATOP_Mens_Pants_Pair2) <- Avg_Time_On_Page_Mens_Pants_Pair2[,1]                        
ATOP_Mens_Pants_Pair2_Cor <- data.frame(cor(ATOP_Mens_Pants_Pair2))
print(ATOP_Mens_Pants_Pair2_Cor);rm(ATOP_Mens_Pants_Pair2,Avg_Time_On_Page_Mens_Pants_Pair2)
write.csv(ATOP_Mens_Pants_Pair2_Cor,"C:/Yashwanth/Clustering/3.Correlation Matrix/4.Mens_Pants_Group_Four/ATOP_Pair2_Pants.csv")

                                  #--------------------- # Pair Three #--------------------#

Avg_Time_On_Page_Mens_Pants_Pair3 <- data.frame(subset(Avg_Time_On_Page_Mens_Pants,
                                              Pages=="mens_lounge_pants" | Pages=="mens_capri_pants",
                                              names(Avg_Time_On_Page_Mens_Pants)),row.names=NULL)
ATOP_Mens_Pants_Pair3 <- data.frame(t(Avg_Time_On_Page_Mens_Pants_Pair3[,2:ncol(Avg_Time_On_Page_Mens_Pants_Pair3)]),
                                    row.names=NULL)
names(ATOP_Mens_Pants_Pair3) <- Avg_Time_On_Page_Mens_Pants_Pair3[,1]                        
ATOP_Mens_Pants_Pair3_Cor <- data.frame(cor(ATOP_Mens_Pants_Pair3))
print(ATOP_Mens_Pants_Pair3_Cor);rm(ATOP_Mens_Pants_Pair3,Avg_Time_On_Page_Mens_Pants_Pair3)
write.csv(ATOP_Mens_Pants_Pair3_Cor,"C:/Yashwanth/Clustering/3.Correlation Matrix/4.Mens_Pants_Group_Four/ATOP_Pair3_Pants.csv")
rm(list=ls(pattern="Cor"))

#-----------------------------------------------------------------------------------------------------------------
#------------------------------------------------- Visits------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------
                            #--------------------- # Pair One #--------------------#

Visits_Mens_Pants_Pair1 <- data.frame(subset(Visits_Mens_Pants,
                                             Pages=="mens_linen_pants" | Pages=="men_jean_leggings",
                                             names(Visits_Mens_Pants)),row.names=NULL)
Vis_Mens_Pants_Pair1 <- data.frame(t(Visits_Mens_Pants_Pair1[,2:ncol(Visits_Mens_Pants_Pair1)]),
                                   row.names=NULL)
names(Vis_Mens_Pants_Pair1) <- Visits_Mens_Pants_Pair1[,1]                        
Vis_Mens_Pants_Pair1_Cor <- data.frame(cor(Vis_Mens_Pants_Pair1))
print(Vis_Mens_Pants_Pair1_Cor);rm(Vis_Mens_Pants_Pair1,Visits_Mens_Pants_Pair1)
write.csv(Vis_Mens_Pants_Pair1_Cor,"C:/Yashwanth/Clustering/3.Correlation Matrix/4.Mens_Pants_Group_Four/Vis_Pair1_Pants.csv")

                            #--------------------- # Pair Two #--------------------#

Visits_Mens_Pants_Pair2 <- data.frame(subset(Visits_Mens_Pants,
                                             Pages=="mens_sweat_pants" | Pages=="mens_cargo_pants",
                                             names(Visits_Mens_Pants)),row.names=NULL)
Vis_Mens_Pants_Pair2 <- data.frame(t(Visits_Mens_Pants_Pair2[,2:ncol(Visits_Mens_Pants_Pair2)]),
                                   row.names=NULL)
names(Vis_Mens_Pants_Pair2) <- Visits_Mens_Pants_Pair2[,1]                        
Vis_Mens_Pants_Pair2_Cor <- data.frame(cor(Vis_Mens_Pants_Pair2))
print(Vis_Mens_Pants_Pair2_Cor);rm(Vis_Mens_Pants_Pair2,Visits_Mens_Pants_Pair2)
write.csv(Vis_Mens_Pants_Pair2_Cor,"C:/Yashwanth/Clustering/3.Correlation Matrix/4.Mens_Pants_Group_Four/Vis_Pair2_Pants.csv")

                             #--------------------- # Pair Three #--------------------#

Visits_Mens_Pants_Pair3 <- data.frame(subset(Visits_Mens_Pants,
                                             Pages=="mens_lounge_pants" | Pages=="mens_capri_pants",
                                             names(Visits_Mens_Pants)),row.names=NULL)
Vis_Mens_Pants_Pair3 <- data.frame(t(Visits_Mens_Pants_Pair3[,2:ncol(Visits_Mens_Pants_Pair3)]),
                                   row.names=NULL)
names(Vis_Mens_Pants_Pair3) <- Visits_Mens_Pants_Pair3[,1]                        
Vis_Mens_Pants_Pair3_Cor <- data.frame(cor(Vis_Mens_Pants_Pair3))
print(Vis_Mens_Pants_Pair3_Cor);rm(Vis_Mens_Pants_Pair3,Visits_Mens_Pants_Pair3)
write.csv(Vis_Mens_Pants_Pair3_Cor,"C:/Yashwanth/Clustering/3.Correlation Matrix/4.Mens_Pants_Group_Four/Vis_Pair3_Pants.csv")
rm(list=ls(pattern="Cor"))

#-----------------------------------------------------------------------------------------------------------------
#------------------------------------------------- Product Page Views --------------------------------------------
#-----------------------------------------------------------------------------------------------------------------
                                #--------------------- # Pair One #--------------------#

Prod_PV_Mens_Pants_Pair1 <- data.frame(subset(Prod_Page_Views_Mens_Pants,
                                              Pages=="mens_linen_pants" | Pages=="men_jean_leggings",
                                              names(Prod_Page_Views_Mens_Pants)),row.names=NULL)
PPV_Mens_Pants_Pair1 <- data.frame(t(Prod_PV_Mens_Pants_Pair1[,2:ncol(Prod_PV_Mens_Pants_Pair1)]),
                                   row.names=NULL)
names(PPV_Mens_Pants_Pair1) <- Prod_PV_Mens_Pants_Pair1[,1]                        
PPV_Mens_Pants_Pair1_Cor <- data.frame(cor(PPV_Mens_Pants_Pair1))
print(PPV_Mens_Pants_Pair1_Cor);rm(PPV_Mens_Pants_Pair1,Prod_PV_Mens_Pants_Pair1)
write.csv(PPV_Mens_Pants_Pair1_Cor,"C:/Yashwanth/Clustering/3.Correlation Matrix/4.Mens_Pants_Group_Four/PPV_Pair1_Pants.csv")

                                #--------------------- # Pair Two #--------------------#

Prod_PV_Mens_Pants_Pair2 <- data.frame(subset(Prod_Page_Views_Mens_Pants,
                                              Pages=="mens_sweat_pants" | Pages=="mens_cargo_pants",
                                              names(Prod_Page_Views_Mens_Pants)),row.names=NULL)
PPV_Mens_Pants_Pair2 <- data.frame(t(Prod_PV_Mens_Pants_Pair2[,2:ncol(Prod_PV_Mens_Pants_Pair2)]),
                                   row.names=NULL)
names(PPV_Mens_Pants_Pair2) <- Prod_PV_Mens_Pants_Pair2[,1]                        
PPV_Mens_Pants_Pair2_Cor <- data.frame(cor(PPV_Mens_Pants_Pair2))
print(PPV_Mens_Pants_Pair2_Cor);rm(PPV_Mens_Pants_Pair2,Prod_PV_Mens_Pants_Pair2)
write.csv(PPV_Mens_Pants_Pair2_Cor,"C:/Yashwanth/Clustering/3.Correlation Matrix/4.Mens_Pants_Group_Four/PPV_Pair2_Pants.csv")

                                #--------------------- # Pair Three #--------------------#

Prod_PV_Mens_Pants_Pair3 <- data.frame(subset(Prod_Page_Views_Mens_Pants,
                                              Pages=="mens_lounge_pants" | Pages=="mens_capri_pants",
                                              names(Prod_Page_Views_Mens_Pants)),row.names=NULL)
PPV_Mens_Pants_Pair3 <- data.frame(t(Prod_PV_Mens_Pants_Pair3[,2:ncol(Prod_PV_Mens_Pants_Pair3)]),
                                   row.names=NULL)
names(PPV_Mens_Pants_Pair3) <- Prod_PV_Mens_Pants_Pair3[,1]                        
PPV_Mens_Pants_Pair3_Cor <- data.frame(cor(PPV_Mens_Pants_Pair3))
print(PPV_Mens_Pants_Pair3_Cor);rm(PPV_Mens_Pants_Pair3,Prod_PV_Mens_Pants_Pair3)
write.csv(PPV_Mens_Pants_Pair3_Cor,"C:/Yashwanth/Clustering/3.Correlation Matrix/4.Mens_Pants_Group_Four/PPV_Pair3_Pants.csv")
rm(list=ls(pattern="Cor"))

# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
