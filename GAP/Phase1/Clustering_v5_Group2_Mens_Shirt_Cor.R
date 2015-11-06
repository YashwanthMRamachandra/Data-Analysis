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

Bounce_Rate_Mens_Shirts <- data.frame(subset(Bounce_Rate,
                                            Pages=="mens_polo" | Pages=="mens_tees" |
                                              Pages=="deep_v_neck_t_shirts" | Pages=="mens_fitted_short_sleeve_shirts" |
                                              Pages=="mens_v_neck" | Pages=="mens_sweatshirt" |
                                              Pages=="v_neck_shirts_for_men" | Pages=="denim_shirts_for_men" |  
                                              Pages=="mens_flannel_shirt" | Pages=="funny_t_shirts_for_men" |
                                              Pages=="mens_tank_tops" | Pages=="mens_plaid_shirts",select=names(Bounce_Rate)),row.names=NULL) 

Avg_Time_On_Page_Mens_Shirts <- data.frame(subset(Avg_Time_On_Page,
                                                  Pages=="mens_polo" | Pages=="mens_tees" |
                                                    Pages=="deep_v_neck_t_shirts" | Pages=="mens_fitted_short_sleeve_shirts" |
                                                    Pages=="mens_v_neck" | Pages=="mens_sweatshirt" |
                                                    Pages=="v_neck_shirts_for_men" | Pages=="denim_shirts_for_men" |  
                                                    Pages=="mens_flannel_shirt" | Pages=="funny_t_shirts_for_men" |
                                                    Pages=="mens_tank_tops" | Pages=="mens_plaid_shirts",select=names(Avg_Time_On_Page)),row.names=NULL) 

Visits_Mens_Shirts <- data.frame(subset(Visits,
                                        Pages=="mens_polo" | Pages=="mens_tees" |
                                          Pages=="deep_v_neck_t_shirts" | Pages=="mens_fitted_short_sleeve_shirts" |
                                          Pages=="mens_v_neck" | Pages=="mens_sweatshirt" |
                                          Pages=="v_neck_shirts_for_men" | Pages=="denim_shirts_for_men" |  
                                          Pages=="mens_flannel_shirt" | Pages=="funny_t_shirts_for_men" |
                                          Pages=="mens_tank_tops" | Pages=="mens_plaid_shirts",select=names(Visits)),row.names=NULL) 

Prod_Page_Views_Mens_Shirts <- data.frame(subset(Prod_Page_Views,
                                                 Pages=="mens_polo" | Pages=="mens_tees" |
                                                   Pages=="deep_v_neck_t_shirts" | Pages=="mens_fitted_short_sleeve_shirts" |
                                                   Pages=="mens_v_neck" | Pages=="mens_sweatshirt" |
                                                   Pages=="v_neck_shirts_for_men" | Pages=="denim_shirts_for_men" |  
                                                   Pages=="mens_flannel_shirt" | Pages=="funny_t_shirts_for_men" |
                                                   Pages=="mens_tank_tops" | Pages=="mens_plaid_shirts",select=names(Prod_Page_Views)),row.names=NULL) 

rm(Bounce_Rate,Avg_Time_On_Page,Visits,Prod_Page_Views)

# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
#                                       || Correlation matrix - One ||
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------

rownames(GAP_Pages_Mens_Shirts) <- GAP_Pages_Mens_Shirts[,1]
GAP_Pages_Mens_Shirts <- GAP_Pages_Mens_Shirts[,-1]
GAP_Pages_Mens_Shirts_T <- data.frame(t(GAP_Pages_Mens_Shirts))
GAP_Pages_Mens_Shirts_Cor <- data.frame(cor(GAP_Pages_Mens_Shirts))
write.csv(GAP_Pages_Mens_Shirts_Cor,"C:/Yashwanth/Clustering/3.Correlation Matrix/2.Mens_Shirt_Group_Two/Corelation_Pages_Mens_Shirts.csv")
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

Bounce_Rate_Mens_Shirts_Pair1 <- data.frame(subset(Bounce_Rate_Mens_Shirts,
                                                  Pages=="mens_polo" | Pages=="mens_tees",
                                                  names(Bounce_Rate_Mens_Shirts)),row.names=NULL)
BR_Mens_Shirts_Pair1 <- data.frame(t(Bounce_Rate_Mens_Shirts_Pair1[,2:ncol(Bounce_Rate_Mens_Shirts_Pair1)]),
                                  row.names=NULL)
names(BR_Mens_Shirts_Pair1) <- Bounce_Rate_Mens_Shirts_Pair1[,1]                        
BR_Mens_Shirts_Pair1_Cor <- data.frame(cor(BR_Mens_Shirts_Pair1))
print(BR_Mens_Shirts_Pair1_Cor);rm(BR_Mens_Shirts_Pair1,Bounce_Rate_Mens_Shirts_Pair1)
write.csv(BR_Mens_Shirts_Pair1_Cor,"C:/Yashwanth/Clustering/3.Correlation Matrix/2.Mens_Shirt_Group_Two/BR_Pair1_Shirts.csv")

                              #--------------------- # Pair Two #--------------------#

Bounce_Rate_Mens_Shirts_Pair2 <- data.frame(subset(Bounce_Rate_Mens_Shirts,
                                                  Pages=="deep_v_neck_t_shirts" | Pages=="mens_fitted_short_sleeve_shirts",
                                                  names(Bounce_Rate_Mens_Shirts)),row.names=NULL)
BR_Mens_Shirts_Pair2 <- data.frame(t(Bounce_Rate_Mens_Shirts_Pair2[,2:ncol(Bounce_Rate_Mens_Shirts_Pair2)]),
                                  row.names=NULL)
names(BR_Mens_Shirts_Pair2) <- Bounce_Rate_Mens_Shirts_Pair2[,1]                        
BR_Mens_Shirts_Pair2_Cor <- data.frame(cor(BR_Mens_Shirts_Pair2))
print(BR_Mens_Shirts_Pair2_Cor);rm(BR_Mens_Shirts_Pair2,Bounce_Rate_Mens_Shirts_Pair2)
write.csv(BR_Mens_Shirts_Pair2_Cor,"C:/Yashwanth/Clustering/3.Correlation Matrix/2.Mens_Shirt_Group_Two/BR_Pair2_Shirts.csv")

                                  #--------------------- # Pair Three #--------------------#

Bounce_Rate_Mens_Shirts_Pair3 <- data.frame(subset(Bounce_Rate_Mens_Shirts,
                                                  Pages=="mens_v_neck" | Pages=="mens_sweatshirt",
                                                  names(Bounce_Rate_Mens_Shirts)),row.names=NULL)
BR_Mens_Shirts_Pair3 <- data.frame(t(Bounce_Rate_Mens_Shirts_Pair3[,2:ncol(Bounce_Rate_Mens_Shirts_Pair3)]),
                                  row.names=NULL)
names(BR_Mens_Shirts_Pair3) <- Bounce_Rate_Mens_Shirts_Pair3[,1]                        
BR_Mens_Shirts_Pair3_Cor <- data.frame(cor(BR_Mens_Shirts_Pair3))
print(BR_Mens_Shirts_Pair3_Cor);rm(BR_Mens_Shirts_Pair3,Bounce_Rate_Mens_Shirts_Pair3)
write.csv(BR_Mens_Shirts_Pair3_Cor,"C:/Yashwanth/Clustering/3.Correlation Matrix/2.Mens_Shirt_Group_Two/BR_Pair3_Shirts.csv")

                                  #--------------------- # Pair Four #--------------------#

Bounce_Rate_Mens_Shirts_Pair4 <- data.frame(subset(Bounce_Rate_Mens_Shirts,
                                                  Pages=="denim_shirts_for_men" | Pages=="v_neck_shirts_for_men",
                                                  names(Bounce_Rate_Mens_Shirts)),row.names=NULL)
BR_Mens_Shirts_Pair4 <- data.frame(t(Bounce_Rate_Mens_Shirts_Pair4[,2:ncol(Bounce_Rate_Mens_Shirts_Pair4)]),
                                  row.names=NULL)
names(BR_Mens_Shirts_Pair4) <- Bounce_Rate_Mens_Shirts_Pair4[,1]                        
BR_Mens_Shirts_Pair4_Cor <- data.frame(cor(BR_Mens_Shirts_Pair4))
print(BR_Mens_Shirts_Pair4_Cor);rm(BR_Mens_Shirts_Pair4,Bounce_Rate_Mens_Shirts_Pair4)
write.csv(BR_Mens_Shirts_Pair4_Cor,"C:/Yashwanth/Clustering/3.Correlation Matrix/2.Mens_Shirt_Group_Two/BR_Pair4_Shirts.csv")

                                    #--------------------- # Pair Five #--------------------#

Bounce_Rate_Mens_Shirts_Pair5 <- data.frame(subset(Bounce_Rate_Mens_Shirts,
                                                  Pages=="mens_flannel_shirt" | Pages=="funny_t_shirts_for_men",
                                                  names(Bounce_Rate_Mens_Shirts)),row.names=NULL)
BR_Mens_Shirts_Pair5 <- data.frame(t(Bounce_Rate_Mens_Shirts_Pair5[,2:ncol(Bounce_Rate_Mens_Shirts_Pair5)]),
                                  row.names=NULL)
names(BR_Mens_Shirts_Pair5) <- Bounce_Rate_Mens_Shirts_Pair5[,1]                        
BR_Mens_Shirts_Pair5_Cor <- data.frame(cor(BR_Mens_Shirts_Pair5))
print(BR_Mens_Shirts_Pair5_Cor);rm(BR_Mens_Shirts_Pair5,Bounce_Rate_Mens_Shirts_Pair5)
write.csv(BR_Mens_Shirts_Pair5_Cor,"C:/Yashwanth/Clustering/3.Correlation Matrix/2.Mens_Shirt_Group_Two/BR_Pair5_Shirts.csv")

                                      #--------------------- # Pair Six #--------------------#

Bounce_Rate_Mens_Shirts_Pair6 <- data.frame(subset(Bounce_Rate_Mens_Shirts,
                                                  Pages=="mens_tank_tops" | Pages=="mens_plaid_shirts",
                                                  names(Bounce_Rate_Mens_Shirts)),row.names=NULL)
BR_Mens_Shirts_Pair6 <- data.frame(t(Bounce_Rate_Mens_Shirts_Pair6[,2:ncol(Bounce_Rate_Mens_Shirts_Pair6)]),
                                  row.names=NULL)
names(BR_Mens_Shirts_Pair6) <- Bounce_Rate_Mens_Shirts_Pair6[,1]                        
BR_Mens_Shirts_Pair6_Cor <- data.frame(cor(BR_Mens_Shirts_Pair6))
print(BR_Mens_Shirts_Pair6_Cor);rm(BR_Mens_Shirts_Pair6,Bounce_Rate_Mens_Shirts_Pair6)
write.csv(BR_Mens_Shirts_Pair6_Cor,"C:/Yashwanth/Clustering/3.Correlation Matrix/2.Mens_Shirt_Group_Two/BR_Pair6_Shirts.csv")
rm(list=ls(pattern="Cor"))

#-----------------------------------------------------------------------------------------------------------------
#------------------------------------------------- Avg TOP ------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------
                                  #--------------------- # Pair One #--------------------#

Avg_Time_On_Page_Mens_Shirts_Pair1 <- data.frame(subset(Avg_Time_On_Page_Mens_Shirts,
                                              Pages=="mens_polo" | Pages=="mens_tees",
                                              names(Avg_Time_On_Page_Mens_Shirts)),row.names=NULL)
ATOP_Mens_Shirts_Pair1 <- data.frame(t(Avg_Time_On_Page_Mens_Shirts_Pair1[,2:ncol(Avg_Time_On_Page_Mens_Shirts_Pair1)]),
                                    row.names=NULL)
names(ATOP_Mens_Shirts_Pair1) <- Avg_Time_On_Page_Mens_Shirts_Pair1[,1]                        
ATOP_Mens_Shirts_Pair1_Cor <- data.frame(cor(ATOP_Mens_Shirts_Pair1))
print(ATOP_Mens_Shirts_Pair1_Cor);rm(ATOP_Mens_Shirts_Pair1,Avg_Time_On_Page_Mens_Shirts_Pair1)
write.csv(ATOP_Mens_Shirts_Pair1_Cor,"C:/Yashwanth/Clustering/3.Correlation Matrix/2.Mens_Shirt_Group_Two/ATOP_Pair1_Shirts.csv")

                                  #--------------------- # Pair Two #--------------------#

Avg_Time_On_Page_Mens_Shirts_Pair2 <- data.frame(subset(Avg_Time_On_Page_Mens_Shirts,
                                              Pages=="deep_v_neck_t_shirts" | Pages=="mens_fitted_short_sleeve_shirts",
                                              names(Avg_Time_On_Page_Mens_Shirts)),row.names=NULL)
ATOP_Mens_Shirts_Pair2 <- data.frame(t(Avg_Time_On_Page_Mens_Shirts_Pair2[,2:ncol(Avg_Time_On_Page_Mens_Shirts_Pair2)]),
                                    row.names=NULL)
names(ATOP_Mens_Shirts_Pair2) <- Avg_Time_On_Page_Mens_Shirts_Pair2[,1]                        
ATOP_Mens_Shirts_Pair2_Cor <- data.frame(cor(ATOP_Mens_Shirts_Pair2))
print(ATOP_Mens_Shirts_Pair2_Cor);rm(ATOP_Mens_Shirts_Pair2,Avg_Time_On_Page_Mens_Shirts_Pair2)
write.csv(ATOP_Mens_Shirts_Pair2_Cor,"C:/Yashwanth/Clustering/3.Correlation Matrix/2.Mens_Shirt_Group_Two/ATOP_Pair2_Shirts.csv")

                                  #--------------------- # Pair Three #--------------------#

Avg_Time_On_Page_Mens_Shirts_Pair3 <- data.frame(subset(Avg_Time_On_Page_Mens_Shirts,
                                              Pages=="mens_v_neck" | Pages=="mens_sweatshirt",
                                              names(Avg_Time_On_Page_Mens_Shirts)),row.names=NULL)
ATOP_Mens_Shirts_Pair3 <- data.frame(t(Avg_Time_On_Page_Mens_Shirts_Pair3[,2:ncol(Avg_Time_On_Page_Mens_Shirts_Pair3)]),
                                    row.names=NULL)
names(ATOP_Mens_Shirts_Pair3) <- Avg_Time_On_Page_Mens_Shirts_Pair3[,1]                        
ATOP_Mens_Shirts_Pair3_Cor <- data.frame(cor(ATOP_Mens_Shirts_Pair3))
print(ATOP_Mens_Shirts_Pair3_Cor);rm(ATOP_Mens_Shirts_Pair3,Avg_Time_On_Page_Mens_Shirts_Pair3)
write.csv(ATOP_Mens_Shirts_Pair3_Cor,"C:/Yashwanth/Clustering/3.Correlation Matrix/2.Mens_Shirt_Group_Two/ATOP_Pair3_Shirts.csv")

                                    #--------------------- # Pair Four #--------------------#

Avg_Time_On_Page_Mens_Shirts_Pair4 <- data.frame(subset(Avg_Time_On_Page_Mens_Shirts,
                                              Pages=="denim_shirts_for_men" | Pages=="v_neck_shirts_for_men",
                                              names(Avg_Time_On_Page_Mens_Shirts)),row.names=NULL)
ATOP_Mens_Shirts_Pair4 <- data.frame(t(Avg_Time_On_Page_Mens_Shirts_Pair4[,2:ncol(Avg_Time_On_Page_Mens_Shirts_Pair4)]),
                                    row.names=NULL)
names(ATOP_Mens_Shirts_Pair4) <- Avg_Time_On_Page_Mens_Shirts_Pair4[,1]                        
ATOP_Mens_Shirts_Pair4_Cor <- data.frame(cor(ATOP_Mens_Shirts_Pair4))
print(ATOP_Mens_Shirts_Pair4_Cor);rm(ATOP_Mens_Shirts_Pair4,Avg_Time_On_Page_Mens_Shirts_Pair4)
write.csv(ATOP_Mens_Shirts_Pair4_Cor,"C:/Yashwanth/Clustering/3.Correlation Matrix/2.Mens_Shirt_Group_Two/ATOP_Pair4_Shirts.csv")

                                     #--------------------- # Pair Five #--------------------#

Avg_Time_On_Page_Mens_Shirts_Pair5 <- data.frame(subset(Avg_Time_On_Page_Mens_Shirts,
                                              Pages=="mens_flannel_shirt" | Pages=="funny_t_shirts_for_men",
                                              names(Avg_Time_On_Page_Mens_Shirts)),row.names=NULL)
ATOP_Mens_Shirts_Pair5 <- data.frame(t(Avg_Time_On_Page_Mens_Shirts_Pair5[,2:ncol(Avg_Time_On_Page_Mens_Shirts_Pair5)]),
                                    row.names=NULL)
names(ATOP_Mens_Shirts_Pair5) <- Avg_Time_On_Page_Mens_Shirts_Pair5[,1]                        
ATOP_Mens_Shirts_Pair5_Cor <- data.frame(cor(ATOP_Mens_Shirts_Pair5))
print(ATOP_Mens_Shirts_Pair5_Cor);rm(ATOP_Mens_Shirts_Pair5,Avg_Time_On_Page_Mens_Shirts_Pair5)
write.csv(ATOP_Mens_Shirts_Pair5_Cor,"C:/Yashwanth/Clustering/3.Correlation Matrix/2.Mens_Shirt_Group_Two/ATOP_Pair5_Shirts.csv")

                                      #--------------------- # Pair Six #--------------------#

Avg_Time_On_Page_Mens_Shirts_Pair6 <- data.frame(subset(Avg_Time_On_Page_Mens_Shirts,
                                              Pages=="mens_tank_tops" | Pages=="mens_plaid_shirts",
                                              names(Avg_Time_On_Page_Mens_Shirts)),row.names=NULL)
ATOP_Mens_Shirts_Pair6 <- data.frame(t(Avg_Time_On_Page_Mens_Shirts_Pair6[,2:ncol(Avg_Time_On_Page_Mens_Shirts_Pair6)]),
                                    row.names=NULL)
names(ATOP_Mens_Shirts_Pair6) <- Avg_Time_On_Page_Mens_Shirts_Pair6[,1]                        
ATOP_Mens_Shirts_Pair6_Cor <- data.frame(cor(ATOP_Mens_Shirts_Pair6))
print(ATOP_Mens_Shirts_Pair6_Cor);rm(ATOP_Mens_Shirts_Pair6,Avg_Time_On_Page_Mens_Shirts_Pair6)
write.csv(ATOP_Mens_Shirts_Pair6_Cor,"C:/Yashwanth/Clustering/3.Correlation Matrix/2.Mens_Shirt_Group_Two/ATOP_Pair6_Shirts.csv")
rm(list=ls(pattern="Cor"))

#-----------------------------------------------------------------------------------------------------------------
#------------------------------------------------- Visits------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------
                            #--------------------- # Pair One #--------------------#

Visits_Mens_Shirts_Pair1 <- data.frame(subset(Visits_Mens_Shirts,
                                             Pages=="mens_polo" | Pages=="mens_tees",
                                             names(Visits_Mens_Shirts)),row.names=NULL)
Vis_Mens_Shirts_Pair1 <- data.frame(t(Visits_Mens_Shirts_Pair1[,2:ncol(Visits_Mens_Shirts_Pair1)]),
                                   row.names=NULL)
names(Vis_Mens_Shirts_Pair1) <- Visits_Mens_Shirts_Pair1[,1]                        
Vis_Mens_Shirts_Pair1_Cor <- data.frame(cor(Vis_Mens_Shirts_Pair1))
print(Vis_Mens_Shirts_Pair1_Cor);rm(Vis_Mens_Shirts_Pair1,Visits_Mens_Shirts_Pair1)
write.csv(Vis_Mens_Shirts_Pair1_Cor,"C:/Yashwanth/Clustering/3.Correlation Matrix/2.Mens_Shirt_Group_Two/Vis_Pair1_Shirts.csv")

                            #--------------------- # Pair Two #--------------------#

Visits_Mens_Shirts_Pair2 <- data.frame(subset(Visits_Mens_Shirts,
                                             Pages=="deep_v_neck_t_shirts" | Pages=="mens_fitted_short_sleeve_shirts",
                                             names(Visits_Mens_Shirts)),row.names=NULL)
Vis_Mens_Shirts_Pair2 <- data.frame(t(Visits_Mens_Shirts_Pair2[,2:ncol(Visits_Mens_Shirts_Pair2)]),
                                   row.names=NULL)
names(Vis_Mens_Shirts_Pair2) <- Visits_Mens_Shirts_Pair2[,1]                        
Vis_Mens_Shirts_Pair2_Cor <- data.frame(cor(Vis_Mens_Shirts_Pair2))
print(Vis_Mens_Shirts_Pair2_Cor);rm(Vis_Mens_Shirts_Pair2,Visits_Mens_Shirts_Pair2)
write.csv(Vis_Mens_Shirts_Pair2_Cor,"C:/Yashwanth/Clustering/3.Correlation Matrix/2.Mens_Shirt_Group_Two/Vis_Pair2_Shirts.csv")

                             #--------------------- # Pair Three #--------------------#

Visits_Mens_Shirts_Pair3 <- data.frame(subset(Visits_Mens_Shirts,
                                             Pages=="mens_v_neck" | Pages=="mens_sweatshirt",
                                             names(Visits_Mens_Shirts)),row.names=NULL)
Vis_Mens_Shirts_Pair3 <- data.frame(t(Visits_Mens_Shirts_Pair3[,2:ncol(Visits_Mens_Shirts_Pair3)]),
                                   row.names=NULL)
names(Vis_Mens_Shirts_Pair3) <- Visits_Mens_Shirts_Pair3[,1]                        
Vis_Mens_Shirts_Pair3_Cor <- data.frame(cor(Vis_Mens_Shirts_Pair3))
print(Vis_Mens_Shirts_Pair3_Cor);rm(Vis_Mens_Shirts_Pair3,Visits_Mens_Shirts_Pair3)
write.csv(Vis_Mens_Shirts_Pair3_Cor,"C:/Yashwanth/Clustering/3.Correlation Matrix/2.Mens_Shirt_Group_Two/Vis_Pair3_Shirts.csv")

                                #--------------------- # Pair Four #--------------------#

Visits_Mens_Shirts_Pair4 <- data.frame(subset(Visits_Mens_Shirts,
                                             Pages=="denim_shirts_for_men" | Pages=="v_neck_shirts_for_men",
                                             names(Visits_Mens_Shirts)),row.names=NULL)
Vis_Mens_Shirts_Pair4 <- data.frame(t(Visits_Mens_Shirts_Pair4[,2:ncol(Visits_Mens_Shirts_Pair4)]),
                                   row.names=NULL)
names(Vis_Mens_Shirts_Pair4) <- Visits_Mens_Shirts_Pair4[,1]                        
Vis_Mens_Shirts_Pair4_Cor <- data.frame(cor(Vis_Mens_Shirts_Pair4))
print(Vis_Mens_Shirts_Pair4_Cor);rm(Vis_Mens_Shirts_Pair4,Visits_Mens_Shirts_Pair4)
write.csv(Vis_Mens_Shirts_Pair4_Cor,"C:/Yashwanth/Clustering/3.Correlation Matrix/2.Mens_Shirt_Group_Two/Vis_Pair4_Shirts.csv")

                                  #--------------------- # Pair Five #--------------------#

Visits_Mens_Shirts_Pair5 <- data.frame(subset(Visits_Mens_Shirts,
                                             Pages=="mens_flannel_shirt" | Pages=="funny_t_shirts_for_men",
                                             names(Visits_Mens_Shirts)),row.names=NULL)
Vis_Mens_Shirts_Pair5 <- data.frame(t(Visits_Mens_Shirts_Pair5[,2:ncol(Visits_Mens_Shirts_Pair5)]),
                                   row.names=NULL)
names(Vis_Mens_Shirts_Pair5) <- Visits_Mens_Shirts_Pair5[,1]                        
Vis_Mens_Shirts_Pair5_Cor <- data.frame(cor(Vis_Mens_Shirts_Pair5))
print(Vis_Mens_Shirts_Pair5_Cor);rm(Vis_Mens_Shirts_Pair5,Visits_Mens_Shirts_Pair5)
write.csv(Vis_Mens_Shirts_Pair5_Cor,"C:/Yashwanth/Clustering/3.Correlation Matrix/2.Mens_Shirt_Group_Two/Vis_Pair5_Shirts.csv")

                                  #--------------------- # Pair Six #--------------------#

Visits_Mens_Shirts_Pair6 <- data.frame(subset(Visits_Mens_Shirts,
                                             Pages=="mens_tank_tops" | Pages=="mens_plaid_shirts",
                                             names(Visits_Mens_Shirts)),row.names=NULL)
Vis_Mens_Shirts_Pair6 <- data.frame(t(Visits_Mens_Shirts_Pair6[,2:ncol(Visits_Mens_Shirts_Pair6)]),
                                   row.names=NULL)
names(Vis_Mens_Shirts_Pair6) <- Visits_Mens_Shirts_Pair6[,1]                        
Vis_Mens_Shirts_Pair6_Cor <- data.frame(cor(Vis_Mens_Shirts_Pair6))
print(Vis_Mens_Shirts_Pair6_Cor);rm(Vis_Mens_Shirts_Pair6,Visits_Mens_Shirts_Pair6)
write.csv(Vis_Mens_Shirts_Pair6_Cor,"C:/Yashwanth/Clustering/3.Correlation Matrix/2.Mens_Shirt_Group_Two/Vis_Pair6_Shirts.csv")
rm(list=ls(pattern="Cor"))

#-----------------------------------------------------------------------------------------------------------------
#------------------------------------------------- Product Page Views --------------------------------------------
#-----------------------------------------------------------------------------------------------------------------
                                #--------------------- # Pair One #--------------------#

Prod_PV_Mens_Shirts_Pair1 <- data.frame(subset(Prod_Page_Views_Mens_Shirts,
                                              Pages=="mens_polo" | Pages=="mens_tees",
                                              names(Prod_Page_Views_Mens_Shirts)),row.names=NULL)
PPV_Mens_Shirts_Pair1 <- data.frame(t(Prod_PV_Mens_Shirts_Pair1[,2:ncol(Prod_PV_Mens_Shirts_Pair1)]),
                                   row.names=NULL)
names(PPV_Mens_Shirts_Pair1) <- Prod_PV_Mens_Shirts_Pair1[,1]                        
PPV_Mens_Shirts_Pair1_Cor <- data.frame(cor(PPV_Mens_Shirts_Pair1))
print(PPV_Mens_Shirts_Pair1_Cor);rm(PPV_Mens_Shirts_Pair1,Prod_PV_Mens_Shirts_Pair1)
write.csv(PPV_Mens_Shirts_Pair1_Cor,"C:/Yashwanth/Clustering/3.Correlation Matrix/2.Mens_Shirt_Group_Two/PPV_Pair1_Shirts.csv")

                                #--------------------- # Pair Two #--------------------#

Prod_PV_Mens_Shirts_Pair2 <- data.frame(subset(Prod_Page_Views_Mens_Shirts,
                                              Pages=="deep_v_neck_t_shirts" | Pages=="mens_fitted_short_sleeve_shirts",
                                              names(Prod_Page_Views_Mens_Shirts)),row.names=NULL)
PPV_Mens_Shirts_Pair2 <- data.frame(t(Prod_PV_Mens_Shirts_Pair2[,2:ncol(Prod_PV_Mens_Shirts_Pair2)]),
                                   row.names=NULL)
names(PPV_Mens_Shirts_Pair2) <- Prod_PV_Mens_Shirts_Pair2[,1]                        
PPV_Mens_Shirts_Pair2_Cor <- data.frame(cor(PPV_Mens_Shirts_Pair2))
print(PPV_Mens_Shirts_Pair2_Cor);rm(PPV_Mens_Shirts_Pair2,Prod_PV_Mens_Shirts_Pair2)
write.csv(PPV_Mens_Shirts_Pair2_Cor,"C:/Yashwanth/Clustering/3.Correlation Matrix/2.Mens_Shirt_Group_Two/PPV_Pair2_Shirts.csv")

                                #--------------------- # Pair Three #--------------------#

Prod_PV_Mens_Shirts_Pair3 <- data.frame(subset(Prod_Page_Views_Mens_Shirts,
                                              Pages=="mens_v_neck" | Pages=="mens_sweatshirt",
                                              names(Prod_Page_Views_Mens_Shirts)),row.names=NULL)
PPV_Mens_Shirts_Pair3 <- data.frame(t(Prod_PV_Mens_Shirts_Pair3[,2:ncol(Prod_PV_Mens_Shirts_Pair3)]),
                                   row.names=NULL)
names(PPV_Mens_Shirts_Pair3) <- Prod_PV_Mens_Shirts_Pair3[,1]                        
PPV_Mens_Shirts_Pair3_Cor <- data.frame(cor(PPV_Mens_Shirts_Pair3))
print(PPV_Mens_Shirts_Pair3_Cor);rm(PPV_Mens_Shirts_Pair3,Prod_PV_Mens_Shirts_Pair3)
write.csv(PPV_Mens_Shirts_Pair3_Cor,"C:/Yashwanth/Clustering/3.Correlation Matrix/2.Mens_Shirt_Group_Two/PPV_Pair3_Shirts.csv")

                                #--------------------- # Pair Four #--------------------#

Prod_PV_Mens_Shirts_Pair4 <- data.frame(subset(Prod_Page_Views_Mens_Shirts,
                                              Pages=="denim_shirts_for_men" | Pages=="v_neck_shirts_for_men",
                                              names(Prod_Page_Views_Mens_Shirts)),row.names=NULL)
PPV_Mens_Shirts_Pair4 <- data.frame(t(Prod_PV_Mens_Shirts_Pair4[,2:ncol(Prod_PV_Mens_Shirts_Pair4)]),
                                   row.names=NULL)
names(PPV_Mens_Shirts_Pair4) <- Prod_PV_Mens_Shirts_Pair4[,1]                        
PPV_Mens_Shirts_Pair4_Cor <- data.frame(cor(PPV_Mens_Shirts_Pair4))
print(PPV_Mens_Shirts_Pair4_Cor);rm(PPV_Mens_Shirts_Pair4,Prod_PV_Mens_Shirts_Pair4)
write.csv(PPV_Mens_Shirts_Pair4_Cor,"C:/Yashwanth/Clustering/3.Correlation Matrix/2.Mens_Shirt_Group_Two/PPV_Pair4_Shirts.csv")

                                  #--------------------- # Pair Five #--------------------#

Prod_PV_Mens_Shirts_Pair5 <- data.frame(subset(Prod_Page_Views_Mens_Shirts,
                                              Pages=="mens_flannel_shirt" | Pages=="funny_t_shirts_for_men",
                                              names(Prod_Page_Views_Mens_Shirts)),row.names=NULL)
PPV_Mens_Shirts_Pair5 <- data.frame(t(Prod_PV_Mens_Shirts_Pair5[,2:ncol(Prod_PV_Mens_Shirts_Pair5)]),
                                   row.names=NULL)
names(PPV_Mens_Shirts_Pair5) <- Prod_PV_Mens_Shirts_Pair5[,1]                        
PPV_Mens_Shirts_Pair5_Cor <- data.frame(cor(PPV_Mens_Shirts_Pair5))
print(PPV_Mens_Shirts_Pair5_Cor);rm(PPV_Mens_Shirts_Pair5,Prod_PV_Mens_Shirts_Pair5)
write.csv(PPV_Mens_Shirts_Pair5_Cor,"C:/Yashwanth/Clustering/3.Correlation Matrix/2.Mens_Shirt_Group_Two/PPV_Pair5_Shirts.csv")

                                #--------------------- # Pair Six #--------------------#

Prod_PV_Mens_Shirts_Pair6 <- data.frame(subset(Prod_Page_Views_Mens_Shirts,
                                              Pages=="mens_tank_tops" | Pages=="mens_plaid_shirts",
                                              names(Prod_Page_Views_Mens_Shirts)),row.names=NULL)
PPV_Mens_Shirts_Pair6 <- data.frame(t(Prod_PV_Mens_Shirts_Pair6[,2:ncol(Prod_PV_Mens_Shirts_Pair6)]),
                                   row.names=NULL)
names(PPV_Mens_Shirts_Pair6) <- Prod_PV_Mens_Shirts_Pair6[,1]                        
PPV_Mens_Shirts_Pair6_Cor <- data.frame(cor(PPV_Mens_Shirts_Pair6))
print(PPV_Mens_Shirts_Pair6_Cor);rm(PPV_Mens_Shirts_Pair6,Prod_PV_Mens_Shirts_Pair6)
write.csv(PPV_Mens_Shirts_Pair6_Cor,"C:/Yashwanth/Clustering/3.Correlation Matrix/2.Mens_Shirt_Group_Two/PPV_Pair6_Shirts.csv")
rm(list=ls(pattern="Cor"))

# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
