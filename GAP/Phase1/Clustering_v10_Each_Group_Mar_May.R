# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
#                                       ||     Bounce Rate    ||
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------

setwd("C:/Yashwanth/Clustering/1.Input_data/Data_2")

Bounce_Rate <- read.table(paste0(getwd(),"/Bounce_rate.csv"), 
                          header = TRUE, sep = ",", quote = "\"", dec = "." ,fill=TRUE, comment.char="", as.is=TRUE)

Avg_Time_On_Page <- read.table(paste0(getwd(),"/AvgTimeOnPage.csv"), 
                               header = TRUE, sep = ",", quote = "\"", dec = "." ,fill=TRUE, comment.char="", as.is=TRUE)

Visits <- read.table(paste0(getwd(),"/Visits.csv"), 
                     header = TRUE, sep = ",", quote = "\"", dec = "." ,fill=TRUE, comment.char="", as.is=TRUE)

# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
#                                       ||     Distribution : Last three months    ||
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------

Bounce_Rate_Means <- data.frame(Pages=Bounce_Rate[,1],Means=rowMeans(Bounce_Rate[,-1]))
hist(Bounce_Rate_Means$Means,prob=T,xlab="Bounce Rate",main="Bounce Rate distribution")
curve(dnorm(x,mean=mean(Bounce_Rate_Means$Means),sd=sd(Bounce_Rate_Means$Means)),add=TRUE)

Avg_Time_On_Page_Means <- data.frame(Pages=Avg_Time_On_Page[,1],Means=rowMeans(Avg_Time_On_Page[,-1]))
hist(Avg_Time_On_Page_Means$Means,prob=T,xlab="Avg Time On Page",main="Avg TOP distribution")
curve(dnorm(x,mean=mean(Avg_Time_On_Page_Means$Means),sd=sd(Avg_Time_On_Page_Means$Means)),add=TRUE)

Visits_Means <- data.frame(Pages=Visits[,1],Means=rowMeans(Visits[,-1]))
hist(Visits_Means$Means,prob=T,xlab="Visits",main="Visits distribution")
curve(dnorm(x,mean=mean(Visits_Means$Means),sd=sd(Visits_Means$Means)),add=TRUE)
boxplot(Visits_Means$Means)
rm(list=ls(pattern="Means"))

# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
#                                       ||     Group One    ||
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------

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
                                         Pages=="mens_carpenter_jeans" | Pages=="relaxed_jeans",select=names(Visits)),row.names=NULL) 

rm(Bounce_Rate,Avg_Time_On_Page,Visits,Prod_Page_Views,Page_Views)

# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
#                                       || Merge data  ||
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------

GAP_Pages <- data.frame(Bounce_Rate_Mens_Jeans[,1],as.numeric(rowMeans(Bounce_Rate_Mens_Jeans[,-1])),
                        as.numeric(rowMeans(Avg_Time_On_Page_Mens_Jeans[,-1])),
                        as.numeric(rowMeans(Visits_Mens_Jeans[,-1])),row.names=NULL)

names(GAP_Pages) <- c("Pages","Bounce_Rate","Avg_Time_On_Page","Visits")#"Prod_Page_Views","Page_Views"

# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
#                                       || standardization of data  ||
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------

library(clusterSim)
GAP_Pages_Std <- data.Normalization(GAP_Pages[,-1],type="n1")
rownames(GAP_Pages_Std) <- GAP_Pages[,1]

# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
#                                       || Distance Matrix  ||
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------

GAP_Pages_Dist <- data.frame(as.matrix(dist(GAP_Pages_Std,upper=TRUE)))
colnames(GAP_Pages_Dist) <- GAP_Pages[,1]
rownames(GAP_Pages_Dist) <- GAP_Pages[,1]
write.csv(GAP_Pages_Dist,"C:/Yashwanth/Clustering/2.Distance_Matrix/1.Mens Jeans Group One/GAP_Pages_Mens_Jeans_Dist_v3.csv")
rm(list=ls(pattern="GAP"));rm(list=ls(pattern="Jeans"));rm(list=ls(pattern="Means"))

# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
#                                       ||     Group Two    ||
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------

Bounce_Rate_Mens_Shirt <- data.frame(subset(Bounce_Rate,
                                            Pages=="mens_polo" | Pages=="mens_tees" |
                                              Pages=="deep_v_neck_t_shirts" | Pages=="mens_sweatshirt" |
                                              Pages=="v_neck_shirts_for_men" | Pages=="funny_t_shirts_for_men" |
                                              Pages=="mens_fitted_short_sleeve_shirts" | Pages=="denim_shirts_for_men" |  
                                              Pages=="mens_flannel_shirt" | Pages=="mens_plaid_shirts" |
                                              Pages=="mens_tank_tops" | Pages=="mens_v_neck",select=names(Bounce_Rate)),row.names=NULL) 

Avg_Time_On_Page_Mens_Shirt <- data.frame(subset(Avg_Time_On_Page,
                                                 Pages=="mens_polo" | Pages=="mens_tees" |
                                                   Pages=="deep_v_neck_t_shirts" | Pages=="mens_sweatshirt" |
                                                   Pages=="v_neck_shirts_for_men" | Pages=="funny_t_shirts_for_men" |
                                                   Pages=="mens_fitted_short_sleeve_shirts" | Pages=="denim_shirts_for_men" |  
                                                   Pages=="mens_flannel_shirt" | Pages=="mens_plaid_shirts" |
                                                   Pages=="mens_tank_tops" | Pages=="mens_v_neck",select=names(Bounce_Rate)),row.names=NULL) 

Visits_Mens_Shirt <- data.frame(subset(Visits,
                                       Pages=="mens_polo" | Pages=="mens_tees" |
                                         Pages=="deep_v_neck_t_shirts" | Pages=="mens_sweatshirt" |
                                         Pages=="v_neck_shirts_for_men" | Pages=="funny_t_shirts_for_men" |
                                         Pages=="mens_fitted_short_sleeve_shirts" | Pages=="denim_shirts_for_men" |  
                                         Pages=="mens_flannel_shirt" | Pages=="mens_plaid_shirts" |
                                         Pages=="mens_tank_tops" | Pages=="mens_v_neck",select=names(Bounce_Rate)),row.names=NULL) 

#rm(Bounce_Rate,Avg_Time_On_Page,Visits,Prod_Page_Views,Page_Views)

# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
#                                       || Merge data  ||
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------

GAP_Pages <- data.frame(Bounce_Rate_Mens_Shirt[,1],as.numeric(rowMeans(Bounce_Rate_Mens_Shirt[,-1])),
                        as.numeric(rowMeans(Avg_Time_On_Page_Mens_Shirt[,-1])),
                        as.numeric(rowMeans(Visits_Mens_Shirt[,-1])),row.names=NULL)
names(GAP_Pages) <- c("Pages","Bounce_Rate","Avg_Time_On_Page","Visits")

# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
#                                       || standardization of data  ||
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------

library(clusterSim)
GAP_Pages_Std <- data.Normalization(GAP_Pages[,-1],type="n1")
rownames(GAP_Pages_Std) <- GAP_Pages[,1]

# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
#                                       || Distance Matrix  ||
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------

GAP_Pages_Dist <- data.frame(as.matrix(dist(GAP_Pages_Std,upper=TRUE)))
colnames(GAP_Pages_Dist) <- GAP_Pages[,1]
rownames(GAP_Pages_Dist) <- GAP_Pages[,1]
write.csv(GAP_Pages_Dist,"C:/Yashwanth/Clustering/2.Distance_Matrix/2.Mens Shirts Group Two/GAP_Pages_Mens_Shirt_Dist_v3.csv")
rm(list=ls(pattern="GAP"));rm(list=ls(pattern="Shirt"))

# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
#                                       ||     Group Three    ||
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------

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

#rm(Bounce_Rate,Avg_Time_On_Page,Visits,Prod_Page_Views,Page_Views)

# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
#                                       || Merge data  ||
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------

GAP_Pages <- data.frame(Bounce_Rate_Mens_Jacket[,1],as.numeric(rowMeans(Bounce_Rate_Mens_Jacket[,-1])),
                        as.numeric(rowMeans(Avg_Time_On_Page_Mens_Jacket[,-1])),
                        as.numeric(rowMeans(Visits_Mens_Jacket[,-1])),row.names=NULL)
names(GAP_Pages) <- c("Pages","Bounce_Rate","Avg_Time_On_Page","Visits")

# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
#                                       || standardization of data  ||
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------

library(clusterSim)
GAP_Pages_Std <- data.Normalization(GAP_Pages[,-1],type="n1")
rownames(GAP_Pages_Std) <- GAP_Pages[,1]

# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
#                                       || Distance Matrix  ||
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------

GAP_Pages_Dist <- data.frame(as.matrix(dist(GAP_Pages_Std,upper=TRUE)))
colnames(GAP_Pages_Dist) <- GAP_Pages[,1]
rownames(GAP_Pages_Dist) <- GAP_Pages[,1]
write.csv(GAP_Pages_Dist,"C:/Yashwanth/Clustering/2.Distance_Matrix/3.Mens Jacket Group Three/GAP_Pages_Mens_Jacket_Dist_v3.csv")
rm(list=ls(pattern="GAP"));rm(list=ls(pattern="Jacket"))

# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
#                                       ||     Group Four    ||
# --------------------------------------------------------------------------------------------------------------
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

#rm(Bounce_Rate,Avg_Time_On_Page,Visits,Prod_Page_Views,Page_Views)

# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
#                                       || Merge data  ||
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------

GAP_Pages <- data.frame(Bounce_Rate_Mens_Pants[,1],as.numeric(rowMeans(Bounce_Rate_Mens_Pants[,-1])),
                        as.numeric(rowMeans(Avg_Time_On_Page_Mens_Pants[,-1])),
                        as.numeric(rowMeans(Visits_Mens_Pants[,-1])),row.names=NULL)
names(GAP_Pages) <- c("Pages","Bounce_Rate","Avg_Time_On_Page","Visits")

# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
#                                       || standardization of data  ||
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------

library(clusterSim)
GAP_Pages_Std <- data.Normalization(GAP_Pages[,-1],type="n1")
rownames(GAP_Pages_Std) <- GAP_Pages[,1]

# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
#                                       || Distance Matrix  ||
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------

GAP_Pages_Dist <- data.frame(as.matrix(dist(GAP_Pages_Std,upper=TRUE)))
colnames(GAP_Pages_Dist) <- GAP_Pages[,1]
rownames(GAP_Pages_Dist) <- GAP_Pages[,1]
write.csv(GAP_Pages_Dist,"C:/Yashwanth/Clustering/2.Distance_Matrix/4.Mens Pants Group Four/GAP_Pages_Mens_Pants_Dist_v3.csv")
rm(list=ls(pattern="GAP"));rm(list=ls(pattern="Pants"))

# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
