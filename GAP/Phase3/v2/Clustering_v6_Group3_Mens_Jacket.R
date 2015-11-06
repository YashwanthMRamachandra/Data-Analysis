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

Avg_Time_On_Page_Mens_Jacket <- data.frame(subset(Bounce_Rate,
                                                 Pages=="mens_printed_hoodies" | Pages=="mens_wool_coats" |
                                                   Pages=="mens_fleece_hoodie" | Pages=="mens_denim_jacket" |
                                                   Pages=="mens_jean_jacket" | Pages=="mens_leather_jackets" |
                                                   Pages=="sport_jacket" | Pages=="mens_vests" ,select=names(Bounce_Rate)),row.names=NULL) 

Visits_Mens_Jacket <- data.frame(subset(Bounce_Rate,
                                       Pages=="mens_printed_hoodies" | Pages=="mens_wool_coats" |
                                         Pages=="mens_fleece_hoodie" | Pages=="mens_denim_jacket" |
                                         Pages=="mens_jean_jacket" | Pages=="mens_leather_jackets" |
                                         Pages=="sport_jacket" | Pages=="mens_vests" ,select=names(Bounce_Rate)),row.names=NULL) 

Prod_Page_Views_Mens_Jacket <- data.frame(subset(Bounce_Rate,
                                                Pages=="mens_printed_hoodies" | Pages=="mens_wool_coats" |
                                                  Pages=="mens_fleece_hoodie" | Pages=="mens_denim_jacket" |
                                                  Pages=="mens_jean_jacket" | Pages=="mens_leather_jackets" |
                                                  Pages=="sport_jacket" | Pages=="mens_vests" ,select=names(Bounce_Rate)),row.names=NULL) 

rm(Bounce_Rate,Avg_Time_On_Page,Visits,Prod_Page_Views)

# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
#                                       || Merge data  ||
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------

BR_Mens_Jacket <- data.frame(Pages=Bounce_Rate_Mens_Jacket[,1],Avg_BR_Mens_Jacket=rowMeans(Bounce_Rate_Mens_Jacket[,-1]))
ATOP_Mens_Jacket <- data.frame(Pages=Avg_Time_On_Page_Mens_Jacket[,1],Avg_ATOP_Mens_Jacket=rowMeans(Avg_Time_On_Page_Mens_Jacket[,-1]))
Vis_Mens_Jacket <- data.frame(Pages=Visits_Mens_Jacket[,1],Avg_Vis_Mens_Jacket=rowMeans(Visits_Mens_Jacket[,-1]))
PPV_Mens_Jacket <- data.frame(Pages=Prod_Page_Views_Mens_Jacket[,1],Avg_PPV_Mens_Jacket=rowMeans(Prod_Page_Views_Mens_Jacket[,-1]))

GAP_Pages_Mens_Jacket <- data.frame(BR_Mens_Jacket,ATOP_Mens_Jacket[,-1],
                                   Vis_Mens_Jacket[,-1],PPV_Mens_Jacket[,-1])
names(GAP_Pages_Mens_Jacket) <- c("Pages","Bounce_Rate","Avg_Time_On_Page","Visits","Prod_Page_Views")

# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
#                                       || standardization of data  ||
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------

library(clusterSim)
GAP_Pages_Mens_Jacket_Std <- data.Normalization(GAP_Pages_Mens_Jacket[,-1],type="n1")
rownames(GAP_Pages_Mens_Jacket_Std) <- GAP_Pages_Mens_Jacket[,1]

# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
#                                       || Distance Matrix  ||
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------

GAP_Pages_Mens_Jacket_Dist <- data.frame(as.matrix(dist(GAP_Pages_Mens_Jacket_Std,upper=TRUE)))
colnames(GAP_Pages_Mens_Jacket_Dist) <- GAP_Pages_Mens_Jacket[,1]
rownames(GAP_Pages_Mens_Jacket_Dist) <- GAP_Pages_Mens_Jacket[,1]
write.csv(GAP_Pages_Mens_Jacket_Dist,"C:/Yashwanth/Clustering/2.Distance_Matrix/3.Mens Jacket Group Three/GAP_Pages_Mens_Jacket_Dist.csv")


# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
#                                       || Correlation matrix  ||
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------

rownames(GAP_Pages_Mens_Jacket) <- GAP_Pages_Mens_Jacket[,1]
GAP_Pages_Mens_Jacket <- GAP_Pages_Mens_Jacket[,-1]
GAP_Pages_Mens_Jacket_T <- data.frame(t(GAP_Pages_Mens_Jacket))
GAP_Pages_Mens_Jacket_Cor <- data.frame(cor(GAP_Pages_Mens_Jacket))
write.csv(GAP_Pages_Mens_Jacket_Cor,"C:/Yashwanth/Clustering/Correlation Matrix/Corelation_Pages_Mens_Jacket.csv")
rm(list=ls(pattern="GAP"))

# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
#                                       || Paired t-test ||
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------

GAP_Pages_Mens_Jacket <- data.frame(GAP_Pages_Mens_Jacket,row.names=FALSE)
names(GAP_Pages_Mens_Jacket) <- 
  GAP_Pages_Mens_Jacket_Sample_One <- data.frame(subset(GAP_Pages_Mens_Jacket,
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
