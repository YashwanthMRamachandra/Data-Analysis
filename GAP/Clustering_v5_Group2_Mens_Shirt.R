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

Bounce_Rate_Mens_Shirt <- data.frame(subset(Bounce_Rate,
                          Pages=="mens_polo" | Pages=="mens_tees" |
                          Pages=="deep_v_neck_t_shirts" | Pages=="mens_sweatshirt" |
                          Pages=="v_neck_shirts_for_men" | Pages=="funny_t_shirts_for_men" |
                          Pages=="mens_fitted_short_sleeve_shirts" | Pages=="denim_shirts_for_men" |  
                          Pages=="mens_flannel_shirt" | Pages=="mens_plaid_shirts" |
                          Pages=="mens_tank_tops" | Pages=="mens_v_neck",select=names(Bounce_Rate)),row.names=NULL) 

Avg_Time_On_Page_Mens_Shirt <- data.frame(subset(Bounce_Rate,
                                                 Pages=="mens_polo" | Pages=="mens_tees" |
                                                   Pages=="deep_v_neck_t_shirts" | Pages=="mens_sweatshirt" |
                                                   Pages=="v_neck_shirts_for_men" | Pages=="funny_t_shirts_for_men" |
                                                   Pages=="mens_fitted_short_sleeve_shirts" | Pages=="denim_shirts_for_men" |  
                                                   Pages=="mens_flannel_shirt" | Pages=="mens_plaid_shirts" |
                                                   Pages=="mens_tank_tops" | Pages=="mens_v_neck",select=names(Bounce_Rate)),row.names=NULL) 

Visits_Mens_Shirt <- data.frame(subset(Bounce_Rate,
                                       Pages=="mens_polo" | Pages=="mens_tees" |
                                         Pages=="deep_v_neck_t_shirts" | Pages=="mens_sweatshirt" |
                                         Pages=="v_neck_shirts_for_men" | Pages=="funny_t_shirts_for_men" |
                                         Pages=="mens_fitted_short_sleeve_shirts" | Pages=="denim_shirts_for_men" |  
                                         Pages=="mens_flannel_shirt" | Pages=="mens_plaid_shirts" |
                                         Pages=="mens_tank_tops" | Pages=="mens_v_neck",select=names(Bounce_Rate)),row.names=NULL) 

Prod_Page_Views_Mens_Shirt <- data.frame(subset(Bounce_Rate,
                                          Pages=="mens_polo" | Pages=="mens_tees" |
                                          Pages=="deep_v_neck_t_shirts" | Pages=="mens_sweatshirt" |
                                          Pages=="v_neck_shirts_for_men" | Pages=="funny_t_shirts_for_men" |
                                          Pages=="mens_fitted_short_sleeve_shirts" | Pages=="denim_shirts_for_men" |  
                                          Pages=="mens_flannel_shirt" | Pages=="mens_plaid_shirts" |
                                          Pages=="mens_tank_tops" | Pages=="mens_v_neck",select=names(Bounce_Rate)),row.names=NULL) 

rm(Bounce_Rate,Avg_Time_On_Page,Visits,Prod_Page_Views)

# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
#                                       || Merge data  ||
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------

BR_Mens_Shirt <- data.frame(Pages=Bounce_Rate_Mens_Shirt[,1],Avg_BR_Mens_Shirt=rowMeans(Bounce_Rate_Mens_Shirt[,-1]))
ATOP_Mens_Shirt <- data.frame(Pages=Avg_Time_On_Page_Mens_Shirt[,1],Avg_ATOP_Mens_Shirt=rowMeans(Avg_Time_On_Page_Mens_Shirt[,-1]))
Vis_Mens_Shirt <- data.frame(Pages=Visits_Mens_Shirt[,1],Avg_Vis_Mens_Shirt=rowMeans(Visits_Mens_Shirt[,-1]))
PPV_Mens_Shirt <- data.frame(Pages=Prod_Page_Views_Mens_Shirt[,1],Avg_PPV_Mens_Shirt=rowMeans(Prod_Page_Views_Mens_Shirt[,-1]))

GAP_Pages_Mens_Shirt <- data.frame(BR_Mens_Shirt,ATOP_Mens_Shirt[,-1],
                                   Vis_Mens_Shirt[,-1],PPV_Mens_Shirt[,-1])
names(GAP_Pages_Mens_Shirt) <- c("Pages","Bounce_Rate","Avg_Time_On_Page","Visits","Prod_Page_Views")

# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
#                                       || standardization of data  ||
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------

library(clusterSim)
GAP_Pages_Mens_Shirt_Std <- data.Normalization(GAP_Pages_Mens_Shirt[,-1],type="n1")
rownames(GAP_Pages_Mens_Shirt_Std) <- GAP_Pages_Mens_Shirt[,1]

# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
#                                       || Distance Matrix  ||
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------

GAP_Pages_Mens_Shirt_Dist <- data.frame(as.matrix(dist(GAP_Pages_Mens_Shirt_Std,upper=TRUE)))
colnames(GAP_Pages_Mens_Shirt_Dist) <- GAP_Pages_Mens_Shirt[,1]
rownames(GAP_Pages_Mens_Shirt_Dist) <- GAP_Pages_Mens_Shirt[,1]
write.csv(GAP_Pages_Mens_Shirt_Dist,"C:/Yashwanth/Clustering/2.Distance_Matrix/2. Mens Shirts Group Two/GAP_Pages_Mens_Shirt_Dist.csv")


# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
#                                       || Correlation matrix  ||
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------

rownames(GAP_Pages_Mens_Shirt) <- GAP_Pages_Mens_Shirt[,1]
GAP_Pages_Mens_Shirt <- GAP_Pages_Mens_Shirt[,-1]
GAP_Pages_Mens_Shirt_T <- data.frame(t(GAP_Pages_Mens_Shirt))
GAP_Pages_Mens_Shirt_Cor <- data.frame(cor(GAP_Pages_Mens_Shirt))
write.csv(GAP_Pages_Mens_Shirt_Cor,"C:/Yashwanth/Clustering/Correlation Matrix/Corelation_Pages_Mens_Shirt.csv")
rm(list=ls(pattern="GAP"))

# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
#                                       || Paired t-test ||
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------

GAP_Pages_Mens_Shirt <- data.frame(GAP_Pages_Mens_Shirt,row.names=FALSE)
names(GAP_Pages_Mens_Shirt) <- 
  GAP_Pages_Mens_Shirt_Sample_One <- data.frame(subset(GAP_Pages_Mens_Shirt,
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
