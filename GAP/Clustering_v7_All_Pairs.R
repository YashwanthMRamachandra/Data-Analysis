# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
#                                       ||     Import Data    ||
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

# --------------------------------------------------------------------------------------------------------------
#                                       ||     Pants    ||
# --------------------------------------------------------------------------------------------------------------

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
#                                       || Merge data  ||
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------

BR_Mens_Pants <- data.frame(Pages=Bounce_Rate_Mens_Pants[,1],Avg_BR_Mens_Pants=rowMeans(Bounce_Rate_Mens_Pants[,-1]))
ATOP_Mens_Pants <- data.frame(Pages=Avg_Time_On_Page_Mens_Pants[,1],Avg_ATOP_Mens_Pants=rowMeans(Avg_Time_On_Page_Mens_Pants[,-1]))
Vis_Mens_Pants <- data.frame(Pages=Visits_Mens_Pants[,1],Avg_Vis_Mens_Pants=rowMeans(Visits_Mens_Pants[,-1]))
PPV_Mens_Pants <- data.frame(Pages=Prod_Page_Views_Mens_Pants[,1],Avg_PPV_Mens_Pants=rowMeans(Prod_Page_Views_Mens_Pants[,-1]))

GAP_Pages_Mens_Pants <- data.frame(BR_Mens_Pants,ATOP_Mens_Pants[,-1],
                                   Vis_Mens_Pants[,-1],PPV_Mens_Pants[,-1])
names(GAP_Pages_Mens_Pants) <- c("Pages","Bounce_Rate","Avg_Time_On_Page","Visits","Prod_Page_Views")

# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
#                                       || standardization of data  ||
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------

library(clusterSim)
GAP_Pages_Mens_Pants_Std <- data.Normalization(GAP_Pages_Mens_Pants[,-1],type="n1")
rownames(GAP_Pages_Mens_Pants_Std) <- GAP_Pages_Mens_Pants[,1]

# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
#                                       || Distance Matrix  ||
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------

GAP_Pages_Mens_Pants_Dist <- data.frame(as.matrix(dist(GAP_Pages_Mens_Pants_Std,upper=TRUE)))
colnames(GAP_Pages_Mens_Pants_Dist) <- GAP_Pages_Mens_Pants[,1]
rownames(GAP_Pages_Mens_Pants_Dist) <- GAP_Pages_Mens_Pants[,1]
write.csv(GAP_Pages_Mens_Pants_Dist,"C:/Yashwanth/Clustering/2.Distance_Matrix/4.Mens Pants Group Four/GAP_Pages_Mens_Pants_Dist.csv")




# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
