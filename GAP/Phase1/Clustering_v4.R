# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
#                                       ||     Bounce Rate    ||
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------

Bounce_Rate <- read.table("C:/Yashwanth/Clustering/Input_data/Bounce_rate.csv", 
                          header = TRUE, sep = ",", quote = "\"", dec = "." ,fill=TRUE, comment.char="", as.is=TRUE)

Avg_Time_On_Page <- read.table("C:/Yashwanth/Clustering/Input_data/AvgTimeOnPage.csv", 
                               header = TRUE, sep = ",", quote = "\"", dec = "." ,fill=TRUE, comment.char="", as.is=TRUE)

Visits <- read.table("C:/Yashwanth/Clustering/Input_data/Visits.csv", 
                     header = TRUE, sep = ",", quote = "\"", dec = "." ,fill=TRUE, comment.char="", as.is=TRUE)

Prod_Page_Views <- read.table("C:/Yashwanth/Clustering/Input_data/ProductPageViews.csv", 
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
#                                       || Merge data  ||
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------

BR_Mens_Jeans <- data.frame(Pages=Bounce_Rate_Mens_Jeans[,1],Avg_BR_Mens_Jeans=rowMeans(Bounce_Rate_Mens_Jeans[,-1]))
ATOP_Mens_Jeans <- data.frame(Pages=Avg_Time_On_Page_Mens_Jeans[,1],Avg_ATOP_Mens_Jeans=rowMeans(Avg_Time_On_Page_Mens_Jeans[,-1]))
Vis_Mens_Jeans <- data.frame(Pages=Visits_Mens_Jeans[,1],Avg_Vis_Mens_Jeans=rowMeans(Visits_Mens_Jeans[,-1]))
PPV_Mens_Jeans <- data.frame(Pages=Prod_Page_Views_Mens_Jeans[,1],Avg_PPV_Mens_Jeans=rowMeans(Prod_Page_Views_Mens_Jeans[,-1]))

GAP_Pages_Mens_Jeans <- data.frame(BR_Mens_Jeans,ATOP_Mens_Jeans[,-1],
                                   Vis_Mens_Jeans[,-1],PPV_Mens_Jeans[,-1])
names(GAP_Pages_Mens_Jeans) <- c("Pages","Bounce_Rate","Avg_Time_On_Page","Visits","Prod_Page_Views")

# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
#                                       || standardization of data  ||
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------

library(clusterSim)
GAP_Pages_Mens_Jeans_Std <- data.Normalization(GAP_Pages_Mens_Jeans[,-1],type="n1")
rownames(GAP_Pages_Mens_Jeans_Std) <- GAP_Pages_Mens_Jeans[,1]

# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
#                                       || Distance Matrix  ||
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------

GAP_Pages_Mens_Jeans_Dist <- data.frame(as.matrix(dist(GAP_Pages_Mens_Jeans_Std,upper=TRUE)))
colnames(GAP_Pages_Mens_Jeans_Dist) <- GAP_Pages_Mens_Jeans[,1]
rownames(GAP_Pages_Mens_Jeans_Dist) <- GAP_Pages_Mens_Jeans[,1]
write.csv(GAP_Pages_Mens_Jeans_Dist,"C:/Yashwanth/Clustering/Distance Matrix/GAP_Pages_Mens_Jeans_Dist.csv")


# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
#                                       || Correlation matrix  ||
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
#                                       || Paired t-test ||
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------

GAP_Pages_Mens_Jeans <- data.frame(GAP_Pages_Mens_Jeans,row.names=FALSE)
names(GAP_Pages_Mens_Jeans) <- 
GAP_Pages_Mens_Jeans_Sample_One <- data.frame(subset(GAP_Pages_Mens_Jeans,
                                                     row.names=="baggy_fit_jeans" | row.names== "loose_jeans"))


# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
#                                       || K-means Clustering ||
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------

fit_Kmeans <- kmeans(Final_Cluster_data[,2:5],3)
Cluster_Tab <- data.frame(Final_Cluster_data[,1],fit_Kmeans$cluster)
names(Cluster_Tab) <- c("Pages","Clusters")
Cluster_Tab <- data.frame(with(Cluster_Tab,Cluster_Tab[order(Clusters),]),row.names=NULL)


# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
