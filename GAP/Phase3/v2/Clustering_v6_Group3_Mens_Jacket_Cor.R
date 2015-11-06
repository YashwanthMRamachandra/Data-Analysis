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

Bounce_Rate_Mens_Jacket <- data.frame(subset(Bounce_Rate,
                                             Pages=="mens_printed_hoodies" | Pages=="mens_wool_coats" |
                                               Pages=="mens_fleece_hoodie" | Pages=="mens_denim_jacket" |
                                               Pages=="mens_jean_jacket" | Pages=="mens_leather_jackets" |
                                               Pages=="sport_jacket" | Pages=="mens_vests" ,select=names(Bounce_Rate)),row.names=NULL) 

Avg_Time_On_Page_Mens_Jacket <- data.frame(subset(Avg_Time_On_Page,
                                                  Pages=="mens_printed_hoodies" | Pages=="mens_wool_coats" |
                                                    Pages=="mens_fleece_hoodie" | Pages=="mens_denim_jacket" |
                                                    Pages=="mens_jean_jacket" | Pages=="mens_leather_jackets" |
                                                    Pages=="sport_jacket" | Pages=="mens_vests" ,select=names(Bounce_Rate)),row.names=NULL) 

Visits_Mens_Jacket <- data.frame(subset(Visits,
                                        Pages=="mens_printed_hoodies" | Pages=="mens_wool_coats" |
                                          Pages=="mens_fleece_hoodie" | Pages=="mens_denim_jacket" |
                                          Pages=="mens_jean_jacket" | Pages=="mens_leather_jackets" |
                                          Pages=="sport_jacket" | Pages=="mens_vests" ,select=names(Bounce_Rate)),row.names=NULL) 

Prod_Page_Views_Mens_Jacket <- data.frame(subset(Prod_Page_Views,
                                                 Pages=="mens_printed_hoodies" | Pages=="mens_wool_coats" |
                                                   Pages=="mens_fleece_hoodie" | Pages=="mens_denim_jacket" |
                                                   Pages=="mens_jean_jacket" | Pages=="mens_leather_jackets" |
                                                   Pages=="sport_jacket" | Pages=="mens_vests" ,select=names(Bounce_Rate)),row.names=NULL) 

rm(Bounce_Rate,Avg_Time_On_Page,Visits,Prod_Page_Views)

# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
#                                       || Correlation matrix - One ||
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------

rownames(GAP_Pages_Mens_Jacket) <- GAP_Pages_Mens_Jacket[,1]
GAP_Pages_Mens_Jacket <- GAP_Pages_Mens_Jacket[,-1]
GAP_Pages_Mens_Jacket_T <- data.frame(t(GAP_Pages_Mens_Jacket))
GAP_Pages_Mens_Jacket_Cor <- data.frame(cor(GAP_Pages_Mens_Jacket))
write.csv(GAP_Pages_Mens_Jacket_Cor,"C:/Yashwanth/Clustering/3.Correlation Matrix/3.Mens_Jacket_Group_Three/Corelation_Pages_Mens_Jacket.csv")
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

Bounce_Rate_Mens_Jacket_Pair1 <- data.frame(subset(Bounce_Rate_Mens_Jacket,
                                                  Pages=="mens_printed_hoodies" | Pages=="sport_jacket",
                                                  names(Bounce_Rate_Mens_Jacket)),row.names=NULL)
BR_Mens_Jacket_Pair1 <- data.frame(t(Bounce_Rate_Mens_Jacket_Pair1[,2:ncol(Bounce_Rate_Mens_Jacket_Pair1)]),
                                  row.names=NULL)
names(BR_Mens_Jacket_Pair1) <- Bounce_Rate_Mens_Jacket_Pair1[,1]                        
BR_Mens_Jacket_Pair1_Cor <- data.frame(cor(BR_Mens_Jacket_Pair1))
print(BR_Mens_Jacket_Pair1_Cor);rm(BR_Mens_Jacket_Pair1,Bounce_Rate_Mens_Jacket_Pair1)
write.csv(BR_Mens_Jacket_Pair1_Cor,"C:/Yashwanth/Clustering/3.Correlation Matrix/3.Mens_Jacket_Group_Three/BR_Pair1_Jacket.csv")

                              #--------------------- # Pair Two #--------------------#

Bounce_Rate_Mens_Jacket_Pair2 <- data.frame(subset(Bounce_Rate_Mens_Jacket,
                                                  Pages=="mens_leather_jackets" | Pages=="mens_wool_coats",
                                                  names(Bounce_Rate_Mens_Jacket)),row.names=NULL)
BR_Mens_Jacket_Pair2 <- data.frame(t(Bounce_Rate_Mens_Jacket_Pair2[,2:ncol(Bounce_Rate_Mens_Jacket_Pair2)]),
                                  row.names=NULL)
names(BR_Mens_Jacket_Pair2) <- Bounce_Rate_Mens_Jacket_Pair2[,1]                        
BR_Mens_Jacket_Pair2_Cor <- data.frame(cor(BR_Mens_Jacket_Pair2))
print(BR_Mens_Jacket_Pair2_Cor);rm(BR_Mens_Jacket_Pair2,Bounce_Rate_Mens_Jacket_Pair2)
write.csv(BR_Mens_Jacket_Pair2_Cor,"C:/Yashwanth/Clustering/3.Correlation Matrix/3.Mens_Jacket_Group_Three/BR_Pair2_Jacket.csv")
rm(list=ls(pattern="Cor"))

#-----------------------------------------------------------------------------------------------------------------
#------------------------------------------------- Avg TOP ------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------
                                  #--------------------- # Pair One #--------------------#

Avg_Time_On_Page_Mens_Jacket_Pair1 <- data.frame(subset(Avg_Time_On_Page_Mens_Jacket,
                                              Pages=="mens_printed_hoodies" | Pages=="sport_jacket",
                                              names(Avg_Time_On_Page_Mens_Jacket)),row.names=NULL)
ATOP_Mens_Jacket_Pair1 <- data.frame(t(Avg_Time_On_Page_Mens_Jacket_Pair1[,2:ncol(Avg_Time_On_Page_Mens_Jacket_Pair1)]),
                                    row.names=NULL)
names(ATOP_Mens_Jacket_Pair1) <- Avg_Time_On_Page_Mens_Jacket_Pair1[,1]                        
ATOP_Mens_Jacket_Pair1_Cor <- data.frame(cor(ATOP_Mens_Jacket_Pair1))
print(ATOP_Mens_Jacket_Pair1_Cor);rm(ATOP_Mens_Jacket_Pair1,Avg_Time_On_Page_Mens_Jacket_Pair1)
write.csv(ATOP_Mens_Jacket_Pair1_Cor,"C:/Yashwanth/Clustering/3.Correlation Matrix/3.Mens_Jacket_Group_Three/ATOP_Pair1_Jacket.csv")

                                  #--------------------- # Pair Two #--------------------#

Avg_Time_On_Page_Mens_Jacket_Pair2 <- data.frame(subset(Avg_Time_On_Page_Mens_Jacket,
                                              Pages=="mens_leather_jackets" | Pages=="mens_wool_coats",
                                              names(Avg_Time_On_Page_Mens_Jacket)),row.names=NULL)
ATOP_Mens_Jacket_Pair2 <- data.frame(t(Avg_Time_On_Page_Mens_Jacket_Pair2[,2:ncol(Avg_Time_On_Page_Mens_Jacket_Pair2)]),
                                    row.names=NULL)
names(ATOP_Mens_Jacket_Pair2) <- Avg_Time_On_Page_Mens_Jacket_Pair2[,1]                        
ATOP_Mens_Jacket_Pair2_Cor <- data.frame(cor(ATOP_Mens_Jacket_Pair2))
print(ATOP_Mens_Jacket_Pair2_Cor);rm(ATOP_Mens_Jacket_Pair2,Avg_Time_On_Page_Mens_Jacket_Pair2)
write.csv(ATOP_Mens_Jacket_Pair2_Cor,"C:/Yashwanth/Clustering/3.Correlation Matrix/3.Mens_Jacket_Group_Three/ATOP_Pair2_Jacket.csv")
rm(list=ls(pattern="Cor"))

#-----------------------------------------------------------------------------------------------------------------
#------------------------------------------------- Visits------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------
                            #--------------------- # Pair One #--------------------#

Visits_Mens_Jacket_Pair1 <- data.frame(subset(Visits_Mens_Jacket,
                                             Pages=="mens_printed_hoodies" | Pages=="sport_jacket",
                                             names(Visits_Mens_Jacket)),row.names=NULL)
Vis_Mens_Jacket_Pair1 <- data.frame(t(Visits_Mens_Jacket_Pair1[,2:ncol(Visits_Mens_Jacket_Pair1)]),
                                   row.names=NULL)
names(Vis_Mens_Jacket_Pair1) <- Visits_Mens_Jacket_Pair1[,1]                        
Vis_Mens_Jacket_Pair1_Cor <- data.frame(cor(Vis_Mens_Jacket_Pair1))
print(Vis_Mens_Jacket_Pair1_Cor);rm(Vis_Mens_Jacket_Pair1,Visits_Mens_Jacket_Pair1)
write.csv(Vis_Mens_Jacket_Pair1_Cor,"C:/Yashwanth/Clustering/3.Correlation Matrix/3.Mens_Jacket_Group_Three/Vis_Pair1_Jacket.csv")

                            #--------------------- # Pair Two #--------------------#

Visits_Mens_Jacket_Pair2 <- data.frame(subset(Visits_Mens_Jacket,
                                             Pages=="mens_leather_jackets" | Pages=="mens_wool_coats",
                                             names(Visits_Mens_Jacket)),row.names=NULL)
Vis_Mens_Jacket_Pair2 <- data.frame(t(Visits_Mens_Jacket_Pair2[,2:ncol(Visits_Mens_Jacket_Pair2)]),
                                   row.names=NULL)
names(Vis_Mens_Jacket_Pair2) <- Visits_Mens_Jacket_Pair2[,1]                        
Vis_Mens_Jacket_Pair2_Cor <- data.frame(cor(Vis_Mens_Jacket_Pair2))
print(Vis_Mens_Jacket_Pair2_Cor);rm(Vis_Mens_Jacket_Pair2,Visits_Mens_Jacket_Pair2)
write.csv(Vis_Mens_Jacket_Pair2_Cor,"C:/Yashwanth/Clustering/3.Correlation Matrix/3.Mens_Jacket_Group_Three/Vis_Pair2_Jacket.csv")
rm(list=ls(pattern="Cor"))

#-----------------------------------------------------------------------------------------------------------------
#------------------------------------------------- Product Page Views --------------------------------------------
#-----------------------------------------------------------------------------------------------------------------
                                #--------------------- # Pair One #--------------------#

Prod_PV_Mens_Jacket_Pair1 <- data.frame(subset(Prod_Page_Views_Mens_Jacket,
                                              Pages=="mens_printed_hoodies" | Pages=="sport_jacket",
                                              names(Prod_Page_Views_Mens_Jacket)),row.names=NULL)
PPV_Mens_Jacket_Pair1 <- data.frame(t(Prod_PV_Mens_Jacket_Pair1[,2:ncol(Prod_PV_Mens_Jacket_Pair1)]),
                                   row.names=NULL)
names(PPV_Mens_Jacket_Pair1) <- Prod_PV_Mens_Jacket_Pair1[,1]                        
PPV_Mens_Jacket_Pair1_Cor <- data.frame(cor(PPV_Mens_Jacket_Pair1))
print(PPV_Mens_Jacket_Pair1_Cor);rm(PPV_Mens_Jacket_Pair1,Prod_PV_Mens_Jacket_Pair1)
write.csv(PPV_Mens_Jacket_Pair1_Cor,"C:/Yashwanth/Clustering/3.Correlation Matrix/3.Mens_Jacket_Group_Three/PPV_Pair1_Jacket.csv")

                                #--------------------- # Pair Two #--------------------#

Prod_PV_Mens_Jacket_Pair2 <- data.frame(subset(Prod_Page_Views_Mens_Jacket,
                                              Pages=="mens_leather_jackets" | Pages=="mens_wool_coats",
                                              names(Prod_Page_Views_Mens_Jacket)),row.names=NULL)
PPV_Mens_Jacket_Pair2 <- data.frame(t(Prod_PV_Mens_Jacket_Pair2[,2:ncol(Prod_PV_Mens_Jacket_Pair2)]),
                                   row.names=NULL)
names(PPV_Mens_Jacket_Pair2) <- Prod_PV_Mens_Jacket_Pair2[,1]                        
PPV_Mens_Jacket_Pair2_Cor <- data.frame(cor(PPV_Mens_Jacket_Pair2))
print(PPV_Mens_Jacket_Pair2_Cor);rm(PPV_Mens_Jacket_Pair2,Prod_PV_Mens_Jacket_Pair2)
write.csv(PPV_Mens_Jacket_Pair2_Cor,"C:/Yashwanth/Clustering/3.Correlation Matrix/3.Mens_Jacket_Group_Three/PPV_Pair2_Jacket.csv")
rm(list=ls(pattern="Cor"))

# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
