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
#                                       || Merge data  ||
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------

BR_Mens_Others <- data.frame(Pages=Bounce_Rate_Mens_Others[,1],Avg_BR_Mens_Pants=rowMeans(Bounce_Rate_Mens_Others[,-1]))
ATOP_Mens_Others <- data.frame(Pages=Avg_Time_On_Page_Mens_Others[,1],Avg_ATOP_Mens_Pants=rowMeans(Avg_Time_On_Page_Mens_Others[,-1]))
Vis_Mens_Others <- data.frame(Pages=Visits_Mens_Others[,1],Avg_Vis_Mens_Pants=rowMeans(Visits_Mens_Others[,-1]))
PPV_Mens_Others <- data.frame(Pages=Prod_Page_Views_Mens_Others[,1],Avg_PPV_Mens_Pants=rowMeans(Prod_Page_Views_Mens_Others[,-1]))

GAP_Pages_Mens_Others <- data.frame(BR_Mens_Others,ATOP_Mens_Others[,-1],
                                   Vis_Mens_Others[,-1],PPV_Mens_Others[,-1])
names(GAP_Pages_Mens_Others) <- c("Pages","Bounce_Rate","Avg_Time_On_Page","Visits","Prod_Page_Views")

# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
#                                       || standardization of data  ||
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------

library(clusterSim)
GAP_Pages_Mens_Others_Std <- data.Normalization(GAP_Pages_Mens_Others[,-1],type="n1")
rownames(GAP_Pages_Mens_Others_Std) <- GAP_Pages_Mens_Others[,1]

# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
#                                       || Distance Matrix  ||
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------

GAP_Pages_Mens_Others_Dist <- data.frame(as.matrix(dist(GAP_Pages_Mens_Others_Std,upper=TRUE)))
colnames(GAP_Pages_Mens_Others_Dist) <- GAP_Pages_Mens_Others[,1]
rownames(GAP_Pages_Mens_Others_Dist) <- GAP_Pages_Mens_Others[,1]
write.csv(GAP_Pages_Mens_Others_Dist,"C:/Yashwanth/Clustering/2.Distance_Matrix/5.Mens Group Five/GAP_Pages_Mens_Others_Dist.csv")


# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
#                                       || Correlation matrix  ||
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------

rownames(GAP_Pages_Mens_Pants) <- GAP_Pages_Mens_Others[,1]
GAP_Pages_Mens_Others <- GAP_Pages_Mens_Others[,-1]
GAP_Pages_Mens_Pants_T <- data.frame(t(GAP_Pages_Mens_Pants))
GAP_Pages_Mens_Pants_Cor <- data.frame(cor(GAP_Pages_Mens_Pants))
write.csv(GAP_Pages_Mens_Pants_Cor,"C:/Yashwanth/Clustering/Correlation Matrix/Corelation_Pages_Mens_Pants.csv")
rm(list=ls(pattern="GAP"))

# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
#                                       || Paired t-test ||
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------

GAP_Pages_Mens_Others <- data.frame(GAP_Pages_Mens_Pants,row.names=FALSE)
names(GAP_Pages_Mens_Pants) <- 
  GAP_Pages_Mens_Pants_Sample_One <- data.frame(subset(GAP_Pages_Mens_Pants,
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
