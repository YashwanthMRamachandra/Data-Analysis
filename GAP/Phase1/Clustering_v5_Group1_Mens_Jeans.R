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

Bounce_Rate_Mens_Jeans_Pair1 <- data.frame(subset(Bounce_Rate_Mens_Jeans,
                                                  Pages=="baggy_fit_jeans" | Pages=="mens_tall_skinny_jeans",
                                                  names(Bounce_Rate_Mens_Jeans)),row.names=NULL)
BR_Mens_Jeans_Pair1 <- data.frame(t(Bounce_Rate_Mens_Jeans_Pair1[,2:ncol(Bounce_Rate_Mens_Jeans_Pair1)]),
                                           row.names=NULL)
names(BR_Mens_Jeans_Pair1) <- Bounce_Rate_Mens_Jeans_Pair1[,1]                        
BR_Mens_Jeans_Pair1_Cor <- cor(BR_Mens_Jeans_Pair1)
print(BR_Mens_Jeans_Pair1_Cor)


Bounce_Rate_Mens_Jeans_Pair2 <- data.frame(subset(Bounce_Rate_Mens_Jeans,
                                                  Pages=="loose_jeans" | Pages=="relaxed_fit_jeans",
                                                  names(Bounce_Rate_Mens_Jeans)),row.names=NULL)
BR_Mens_Jeans_Pair2 <- data.frame(t(Bounce_Rate_Mens_Jeans_Pair2[,2:ncol(Bounce_Rate_Mens_Jeans_Pair2)]),
                                  row.names=NULL)
names(BR_Mens_Jeans_Pair2) <- Bounce_Rate_Mens_Jeans_Pair2[,1]                        
BR_Mens_Jeans_Pair2_Cor <- cor(BR_Mens_Jeans_Pair2)
print(BR_Mens_Jeans_Pair2_Cor)



# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
