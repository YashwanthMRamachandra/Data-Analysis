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


# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
#                                       || Merge data  ||
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------

BR_Mens_Jeans <- data.frame(Pages=Bounce_Rate_Mens_Jeans[,1],Avg_BR_Mens_Jeans=rowSums(Bounce_Rate_Mens_Jeans[,-1]))
ATOP_Mens_Jeans <- data.frame(Pages=Avg_Time_On_Page_Mens_Jeans[,1],Avg_ATOP_Mens_Jeans=rowSums(Avg_Time_On_Page_Mens_Jeans[,-1]))
Vis_Mens_Jeans <- data.frame(Pages=Visits_Mens_Jeans[,1],Avg_Vis_Mens_Jeans=rowSums(Visits_Mens_Jeans[,-1]))
PPV_Mens_Jeans <- data.frame(Pages=Prod_Page_Views_Mens_Jeans[,1],Avg_PPV_Mens_Jeans=rowSums(Prod_Page_Views_Mens_Jeans[,-1]))

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

GAP_Pages_Mens_Jeans_Dist <- data.frame(as.matrix(dist(GAP_Pages_Mens_Jeans,upper=TRUE)))
colnames(GAP_Pages_Mens_Jeans_Dist) <- GAP_Pages_Mens_Jeans[,1]
rownames(GAP_Pages_Mens_Jeans_Dist) <- GAP_Pages_Mens_Jeans[,1]
write.csv(GAP_Pages_Mens_Jeans_Dist,"C:/Yashwanth/Clustering/Distance Matrix/GAP_Pages_Mens_Jeans_Dist.csv")


# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
#                                       || Transpose data  ||
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------

#Bounce_Rate <- data.frame(t(Bounce_Rate_Raw),row.names=NULL)
#rm(list=ls())

# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
#                                       || K-means Clustering  ||
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------

fit_Kmeans_BR <- kmeans(Bounce_Rate[,c(2:ncol(Bounce_Rate))],3)
Cluster_Tab_Bounce <- data.frame(Bounce_Rate[,1],fit_Kmeans_BR$cluster)
names(Cluster_Tab_Bounce) <- c("Pages","Clusters")
Cluster_Tab_Bounce <- data.frame(with(Cluster_Tab_Bounce,Cluster_Tab_Bounce[order(Clusters),]),row.names=NULL)

# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
#                                       ||     Average Time on Page    ||
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------

Avg_Time_On_Page <- read.table("C:/Yashwanth/Clustering/AvgTimeOnPage.csv", 
                               header = TRUE, sep = ",", quote = "\"", dec = "." ,fill=TRUE, comment.char="", as.is=TRUE)

Avg_Time_On_Page_Std <- data.Normalization(Avg_Time_On_Page,type="n1")

# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
#                                       || Distance Matrix  ||
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------

ATOP_Dist <- data.frame(as.matrix(dist(Avg_Time_On_Page_Std,upper=TRUE)))

# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
#                                       || Moving Averages  ||
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------

Avg_Time_On_Page_MA <- SMA(Avg_Time_On_Page[,c(2:ncol(Avg_Time_On_Page))],n=10)
Avg_Time_On_Page_New <- data.frame(Avg_Time_On_Page[,1],Avg_Time_On_Page_MA)
names(Avg_Time_On_Page_New) <- c("Pages","Avg_Time_On_Page")
Avg_Time_On_Page_New$Avg_Time_On_Page[is.na(Avg_Time_On_Page_New$Avg_Time_On_Page)] <- mean(Avg_Time_On_Page_New$Avg_Time_On_Page,na.rm=T)

# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
#                                       || K-means Clustering  ||
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------

fit_Kmeans_ATOP <- kmeans(Avg_Time_On_Page[,c(2:ncol(Avg_Time_On_Page))],3)
Cluster_Tab_ATOP <- data.frame(Avg_Time_On_Page[,1],fit_Kmeans_ATOP$cluster)
names(Cluster_Tab_ATOP) <- c("Pages","Clusters")
Cluster_Tab_ATOP <- data.frame(with(Cluster_Tab_ATOP,Cluster_Tab_ATOP[order(Clusters),]),row.names=NULL)

# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
#                                       ||     Visits    ||
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------

Visits <- read.table("C:/Yashwanth/Clustering/Visits.csv", 
                     header = TRUE, sep = ",", quote = "\"", dec = "." ,fill=TRUE, comment.char="", as.is=TRUE)

Visits_Std <- data.Normalization(Visits,type="n1")

# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
#                                       || Distance Matrix  ||
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------

Visits_Dist <- data.frame(as.matrix(dist(Visits_Std,upper=TRUE)))

# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
#                                       || Moving Averages  ||
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------

Visits_MA <- SMA(Visits[,c(2:ncol(Visits))],n=10)
Visits_New <- data.frame(Visits[,1],Visits_MA)
names(Visits_New) <- c("Pages","Visits")
Visits_New$Visits[is.na(Visits_New$Visits)] <- mean(Visits_New$Visits,na.rm=T)

# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
#                                       || K-means Clustering  ||
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------

fit_Kmeans_Vis <- kmeans(Visits[,c(2:ncol(Visits))],3)
Cluster_Tab_Vis <- data.frame(Visits[,1],fit_Kmeans_Vis$cluster)
names(Cluster_Tab_Vis) <- c("Pages","Clusters")
Cluster_Tab_Vis <- data.frame(with(Cluster_Tab_Vis,Cluster_Tab_Vis[order(Clusters),]),row.names=NULL)

# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
#                                       ||     Product Page Views    ||
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------

Prod_Page_Views <- read.table("C:/Yashwanth/Clustering/ProductPageViews.csv", 
                              header = TRUE, sep = ",", quote = "\"", dec = "." ,fill=TRUE, comment.char="", as.is=TRUE)

Prod_Page_Views_Std <- data.Normalization(Visits,type="n1")

# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
#                                       || Distance Matrix  ||
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------

PPV_Dist <- data.frame(as.matrix(dist(Prod_Page_Views_Std,upper=TRUE)))

# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
#                                       || Moving Averages  ||
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------

Prod_Page_Views_MA <- SMA(Prod_Page_Views[,c(2:ncol(Prod_Page_Views))],n=10)
Prod_Page_Views_New <- data.frame(Prod_Page_Views[,1],Prod_Page_Views_MA)
names(Prod_Page_Views_New) <- c("Pages","Prod_Page_Views")
Prod_Page_Views_New$Prod_Page_Views[is.na(Prod_Page_Views_New$Prod_Page_Views)] <- mean(Prod_Page_Views_New$Prod_Page_Views,na.rm=T)

# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
#                                       || K-means Clustering  ||
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------

fit_Kmeans_PPV <- kmeans(Prod_Page_Views[,c(2:ncol(Prod_Page_Views))],3)
Cluster_Tab_PPV <- data.frame(Prod_Page_Views[,1],fit_Kmeans_PPV$cluster)
names(Cluster_Tab_PPV) <- c("Pages","Clusters")
Cluster_Tab_PPV <- data.frame(with(Cluster_Tab_PPV,Cluster_Tab_PPV[order(Clusters),]),row.names=NULL)


# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
#                                       || Combine all Variables  ||
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------

Final_Cluster_data <- cbind(Bounce_Rate_New,Avg_Time_On_Page_New[,2],Visits_New[,2],Prod_Page_Views_New[,2])
colnames(Final_Cluster_data) <- c("Pages","Bounce_Cluster","Avg_Time_On_Page","Visits","Prod_Page_Views")

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
