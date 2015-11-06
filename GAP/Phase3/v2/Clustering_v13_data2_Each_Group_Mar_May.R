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
#                                       ||     Distribution : March to May    ||
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

Bounce_Rate_Mens_Shirts <- data.frame(subset(Bounce_Rate,
                                             Pages=="men_new_arrival_ts_and_polos"|Pages=="printed_and_patterned_long_sleeve_shirts_for_men"|
                                               Pages=="polos_new_arrivals_men_C1026439"|Pages=="solid_long_sleeve_shirts_for_men"|
                                               Pages=="mens_new_arrival_sweatshirts"|Pages=="mens_long_sleeve_plaid_and_checkered_shirts"|
                                               Pages=="shirts_slim_shop_men_C1018344"|Pages=="blue_long_sleeve_shirts_for_men"|
                                               Pages=="mens_polo_shirts"|Pages=="mens_striped_long_sleeve_shirts"|
                                               Pages=="mens_pique_polo_shirts"|Pages=="discount_mens_shirts"|
                                               Pages=="lived_in_polo_shirts_for_men"|Pages=="discount_polo_shirts"|
                                               Pages=="mens_sweatshirts"|Pages=="cool_mens_shirts"|
                                               Pages=="crew_neck_sweatshirts_for_men"|Pages=="mens_shirts_on_sale"|
                                               Pages=="mens_short_sleeve_shirts",select=names(Bounce_Rate)),row.names=NULL) 

Avg_Time_On_Page_Mens_Shirts <- data.frame(subset(Avg_Time_On_Page,
                                                  Pages=="men_new_arrival_ts_and_polos"|Pages=="printed_and_patterned_long_sleeve_shirts_for_men"|
                                                    Pages=="polos_new_arrivals_men_C1026439"|Pages=="solid_long_sleeve_shirts_for_men"|
                                                    Pages=="mens_new_arrival_sweatshirts"|Pages=="mens_long_sleeve_plaid_and_checkered_shirts"|
                                                    Pages=="shirts_slim_shop_men_C1018344"|Pages=="blue_long_sleeve_shirts_for_men"|
                                                    Pages=="mens_polo_shirts"|Pages=="mens_striped_long_sleeve_shirts"|
                                                    Pages=="mens_pique_polo_shirts"|Pages=="discount_mens_shirts"|
                                                    Pages=="lived_in_polo_shirts_for_men"|Pages=="discount_polo_shirts"|
                                                    Pages=="mens_sweatshirts"|Pages=="cool_mens_shirts"|
                                                    Pages=="crew_neck_sweatshirts_for_men"|Pages=="mens_shirts_on_sale"|
                                                    Pages=="mens_short_sleeve_shirts",select=names(Bounce_Rate)),row.names=NULL) 

Visits_Mens_Shirts <- data.frame(subset(Visits,
                                        Pages=="men_new_arrival_ts_and_polos"|Pages=="printed_and_patterned_long_sleeve_shirts_for_men"|
                                          Pages=="polos_new_arrivals_men_C1026439"|Pages=="solid_long_sleeve_shirts_for_men"|
                                          Pages=="mens_new_arrival_sweatshirts"|Pages=="mens_long_sleeve_plaid_and_checkered_shirts"|
                                          Pages=="shirts_slim_shop_men_C1018344"|Pages=="blue_long_sleeve_shirts_for_men"|
                                          Pages=="mens_polo_shirts"|Pages=="mens_striped_long_sleeve_shirts"|
                                          Pages=="mens_pique_polo_shirts"|Pages=="discount_mens_shirts"|
                                          Pages=="lived_in_polo_shirts_for_men"|Pages=="discount_polo_shirts"|
                                          Pages=="mens_sweatshirts"|Pages=="cool_mens_shirts"|
                                          Pages=="crew_neck_sweatshirts_for_men"|Pages=="mens_shirts_on_sale"|
                                          Pages=="mens_short_sleeve_shirts",select=names(Bounce_Rate)),row.names=NULL) 

rm(Bounce_Rate,Avg_Time_On_Page,Visits,Prod_Page_Views,Page_Views)

# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
#                                       || Merge data  ||
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------

GAP_Pages <- data.frame(Bounce_Rate_Mens_Shirts[,1],as.numeric(rowMeans(Bounce_Rate_Mens_Shirts[,-1])),
                        as.numeric(rowMeans(Avg_Time_On_Page_Mens_Shirts[,-1])),
                        as.numeric(rowMeans(Visits_Mens_Shirts[,-1])),row.names=NULL)

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
write.csv(GAP_Pages_Dist,"C:/Yashwanth/Clustering/2.Distance_Matrix/1.Mens Jeans Group One/GAP_Pages_data2_Mens_Shirts_Dist_v2.csv")
rm(list=ls(pattern="GAP"));rm(list=ls(pattern="Shirts"));rm(list=ls(pattern="Means"))

# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
#                                       ||     Group Two    ||
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------

Bounce_Rate_Mens_Jeans <- data.frame(subset(Bounce_Rate,
                                            Pages=="cool_jeans_for_men"|Pages=="mens_tapered_jeans"|
                                              Pages=="trendy_mens_shorts"|Pages=="relaxed_fit_jeans_for_men"|
                                              Pages=="jeans_slim_shop_men_C1017204"|Pages=="shorts_for_men"|
                                              Pages=="mens_jeans"|Pages=="mens_flat_front_shorts"|
                                              Pages=="meet_the_carpenter_jeans_men_C1026427"|Pages=="cargo_shorts_for_men"|
                                              Pages=="mens_skinny_jeans"|Pages=="shorts_sale_men_C1013532"|
                                              Pages=="slim_jeans_for_men"|Pages=="discount_mens_pants"|
                                              Pages=="mens_straight_leg_jeans"|Pages=="mens_boot_cut_jeans"|
                                              Pages=="mens_standard_fit_jeans",
                                            select=names(Bounce_Rate)),row.names=NULL) 

Avg_Time_On_Page_Mens_Jeans <- data.frame(subset(Avg_Time_On_Page,
                                                 Pages=="cool_jeans_for_men"|Pages=="mens_tapered_jeans"|
                                                   Pages=="trendy_mens_shorts"|Pages=="relaxed_fit_jeans_for_men"|
                                                   Pages=="jeans_slim_shop_men_C1017204"|Pages=="shorts_for_men"|
                                                   Pages=="mens_jeans"|Pages=="mens_flat_front_shorts"|
                                                   Pages=="meet_the_carpenter_jeans_men_C1026427"|Pages=="cargo_shorts_for_men"|
                                                   Pages=="mens_skinny_jeans"|Pages=="shorts_sale_men_C1013532"|
                                                   Pages=="slim_jeans_for_men"|Pages=="discount_mens_pants"|
                                                   Pages=="mens_straight_leg_jeans"|Pages=="mens_boot_cut_jeans"|
                                                   Pages=="mens_standard_fit_jeans",
                                                 select=names(Bounce_Rate)),row.names=NULL) 

Visits_Mens_Jeans <- data.frame(subset(Visits,
                                       Pages=="cool_jeans_for_men"|Pages=="mens_tapered_jeans"|
                                         Pages=="trendy_mens_shorts"|Pages=="relaxed_fit_jeans_for_men"|
                                         Pages=="jeans_slim_shop_men_C1017204"|Pages=="shorts_for_men"|
                                         Pages=="mens_jeans"|Pages=="mens_flat_front_shorts"|
                                         Pages=="meet_the_carpenter_jeans_men_C1026427"|Pages=="cargo_shorts_for_men"|
                                         Pages=="mens_skinny_jeans"|Pages=="shorts_sale_men_C1013532"|
                                         Pages=="slim_jeans_for_men"|Pages=="discount_mens_pants"|
                                         Pages=="mens_straight_leg_jeans"|Pages=="mens_boot_cut_jeans"|
                                         Pages=="mens_standard_fit_jeans",
                                       select=names(Bounce_Rate)),row.names=NULL) 

#rm(Bounce_Rate,Avg_Time_On_Page,Visits,Prod_Page_Views,Page_Views)

# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
#                                       || Merge data  ||
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------

GAP_Pages <- data.frame(Bounce_Rate_Mens_Jeans[,1],as.numeric(rowMeans(Bounce_Rate_Mens_Jeans[,-1])),
                        as.numeric(rowMeans(Avg_Time_On_Page_Mens_Jeans[,-1])),
                        as.numeric(rowMeans(Visits_Mens_Jeans[,-1])),row.names=NULL)
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
write.csv(GAP_Pages_Dist,"C:/Yashwanth/Clustering/2.Distance_Matrix/2.Mens Shirts Group Two/GAP_Pages_Mens_shorts_Dist_data2_v2.csv")
rm(list=ls(pattern="GAP"));rm(list=ls(pattern="Jeans"));rm(list=ls(pattern="Means"))

# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
#                                       ||     Group Three    ||
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------

Bounce_Rate_Mens_Pants <- data.frame(subset(Bounce_Rate,
                                            Pages=="mens_accessories"|Pages=="mens_jackets_sale"|
                                              Pages=="mens_cord_jeans"|Pages=="discount_mens_pajamas"|
                                              Pages=="mens_jogging_pants"|Pages=="mens_short_sleeve_shirts"|
                                              Pages=="mens_pants"|Pages=="mens_shoes"|
                                              Pages=="cool_accessories_for_men"|Pages=="sweaters_for_men"|
                                              Pages=="mens_head_to_toe_looks"|Pages=="mens_sweatshirts"|
                                              Pages=="sleep_lounge"|Pages=="mens_t_shirts"|
                                              Pages=="trendy_sweaters_for_men"|Pages=="sweaters_tall_shop_men_C1026765"|
                                              Pages=="mens_new_arrival_underwear"|Pages=="mens_crewneck_sweaters"|
                                              Pages=="mens_outerwear"|Pages=="wool_sweaters_men_C51258"|
                                              Pages=="mens_accessories_sale"|Pages=="mens_cashmere_sweaters"|
                                              Pages=="mens_sweaters_on_sale",
                                            select=names(Bounce_Rate)),row.names=NULL) 

Avg_Time_On_Page_Mens_Pants <- data.frame(subset(Avg_Time_On_Page,
                                                 Pages=="mens_accessories"|Pages=="mens_jackets_sale"|
                                                   Pages=="mens_cord_jeans"|Pages=="discount_mens_pajamas"|
                                                   Pages=="mens_jogging_pants"|Pages=="mens_short_sleeve_shirts"|
                                                   Pages=="mens_pants"|Pages=="mens_shoes"|
                                                   Pages=="cool_accessories_for_men"|Pages=="sweaters_for_men"|
                                                   Pages=="mens_head_to_toe_looks"|Pages=="mens_sweatshirts"|
                                                   Pages=="sleep_lounge"|Pages=="mens_t_shirts"|
                                                   Pages=="trendy_sweaters_for_men"|Pages=="sweaters_tall_shop_men_C1026765"|
                                                   Pages=="mens_new_arrival_underwear"|Pages=="mens_crewneck_sweaters"|
                                                   Pages=="mens_outerwear"|Pages=="wool_sweaters_men_C51258"|
                                                   Pages=="mens_accessories_sale"|Pages=="mens_cashmere_sweaters"|
                                                   Pages=="mens_sweaters_on_sale",
                                                 select=names(Bounce_Rate)),row.names=NULL) 

Visits_Mens_Pants <- data.frame(subset(Visits,
                                       Pages=="mens_accessories"|Pages=="mens_jackets_sale"|
                                         Pages=="mens_cord_jeans"|Pages=="discount_mens_pajamas"|
                                         Pages=="mens_jogging_pants"|Pages=="mens_short_sleeve_shirts"|
                                         Pages=="mens_pants"|Pages=="mens_shoes"|
                                         Pages=="cool_accessories_for_men"|Pages=="sweaters_for_men"|
                                         Pages=="mens_head_to_toe_looks"|Pages=="mens_sweatshirts"|
                                         Pages=="sleep_lounge"|Pages=="mens_t_shirts"|
                                         Pages=="trendy_sweaters_for_men"|Pages=="sweaters_tall_shop_men_C1026765"|
                                         Pages=="mens_new_arrival_underwear"|Pages=="mens_crewneck_sweaters"|
                                         Pages=="mens_outerwear"|Pages=="wool_sweaters_men_C51258"|
                                         Pages=="mens_accessories_sale"|Pages=="mens_cashmere_sweaters"|
                                         Pages=="mens_sweaters_on_sale",
                                       select=names(Bounce_Rate)),row.names=NULL) 

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
write.csv(GAP_Pages_Dist,"C:/Yashwanth/Clustering/2.Distance_Matrix/3.Mens Jacket Group Three/GAP_Pages_Mens_Jackets_Dist_data_2_v2.csv")
rm(list=ls(pattern="GAP"));rm(list=ls(pattern="Pants"))

# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
#                                       ||     Group Four    ||
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------

Bounce_Rate_Mens_Underwear <- data.frame(subset(Bounce_Rate,
                                                Pages=="mens_underwear"|Pages=="mens_briefs"|
                                                  Pages=="mens_underwear_packs"|Pages=="mens_socks"|
                                                  Pages=="boxer_briefs"|Pages=="stretch_mens_underwear"|
                                                  Pages=="mens_boxers"|Pages=="mens_undershirts",
                                                  select=names(Bounce_Rate)),row.names=NULL) 

Avg_Time_On_Page_Mens_Underwear <- data.frame(subset(Avg_Time_On_Page,
                                                     Pages=="mens_underwear"|Pages=="mens_briefs"|
                                                       Pages=="mens_underwear_packs"|Pages=="mens_socks"|
                                                       Pages=="boxer_briefs"|Pages=="stretch_mens_underwear"|
                                                       Pages=="mens_boxers"|Pages=="mens_undershirts",
                                                     select=names(Bounce_Rate)),row.names=NULL) 

Visits_Mens_Underwear <- data.frame(subset(Visits,
                                           Pages=="mens_underwear"|Pages=="mens_briefs"|
                                             Pages=="mens_underwear_packs"|Pages=="mens_socks"|
                                             Pages=="boxer_briefs"|Pages=="stretch_mens_underwear"|
                                             Pages=="mens_boxers"|Pages=="mens_undershirts",
                                           select=names(Bounce_Rate)),row.names=NULL) 

#rm(Bounce_Rate,Avg_Time_On_Page,Visits,Prod_Page_Views,Page_Views)

# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
#                                       || Merge data  ||
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------

GAP_Pages <- data.frame(Bounce_Rate_Mens_Underwear[,1],as.numeric(rowMeans(Bounce_Rate_Mens_Underwear[,-1])),
                        as.numeric(rowMeans(Avg_Time_On_Page_Mens_Underwear[,-1])),
                        as.numeric(rowMeans(Visits_Mens_Underwear[,-1])),row.names=NULL)
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
write.csv(GAP_Pages_Dist,"C:/Yashwanth/Clustering/2.Distance_Matrix/4.Mens Pants Group Four/GAP_Pages_Mens_Under_Dist_data2_v2.csv")
rm(list=ls(pattern="GAP"));rm(list=ls(pattern="Under"));rm(list=ls(pattern="Means"))

# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
#                                       ||     Group Five    ||
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------

Bounce_Rate_Mens_Sweater <- data.frame(subset(Bounce_Rate,
                                              Pages=="joggers_new_arrivals_men_C1018759"|Pages=="mens_polo_shirts"|
                                                Pages=="khakis_slim_shop_men_C1017207"|Pages=="mens_pique_polo_shirts"|
                                                Pages=="meet_the_new_khaki_khakis_C1026594"|Pages=="lived_in_polo_shirts_for_men"|
                                                Pages=="skinny_pants_for_men"|Pages=="mens_sweatshirts"|
                                                Pages=="mens_khaki_pants"|Pages=="straight_leg_pants_for_men"|Pages=="mens_short_sleeve_shirts"|
                                                Pages=="relaxed_fit_pants_for_men"|Pages=="printed_and_patterned_long_sleeve_shirts_for_men"|
                                                Pages=="mens_jogging_pants"|Pages=="solid_long_sleeve_shirts_for_men"|
                                                Pages=="men_new_arrival_ts_and_polos"|Pages=="mens_long_sleeve_plaid_and_checkered_shirts"|
                                                Pages=="polos_new_arrivals_men_C1026439"|Pages=="blue_long_sleeve_shirts_for_men"|
                                                Pages=="mens_new_arrival_sweatshirts"|Pages=="mens_striped_long_sleeve_shirts"|
                                                Pages=="shirts_slim_shop_men_C1018344"|Pages=="discount_mens_shirts"|
                                                Pages=="discount_polo_shirts",
                                                select=names(Bounce_Rate)),row.names=NULL) 

Avg_Time_On_Page_Mens_Sweater <- data.frame(subset(Avg_Time_On_Page,
                                                   Pages=="joggers_new_arrivals_men_C1018759"|Pages=="mens_polo_shirts"|
                                                     Pages=="khakis_slim_shop_men_C1017207"|Pages=="mens_pique_polo_shirts"|
                                                     Pages=="meet_the_new_khaki_khakis_C1026594"|Pages=="lived_in_polo_shirts_for_men"|
                                                     Pages=="skinny_pants_for_men"|Pages=="mens_sweatshirts"|
                                                     Pages=="mens_khaki_pants"|Pages=="straight_leg_pants_for_men"|Pages=="mens_short_sleeve_shirts"|
                                                     Pages=="relaxed_fit_pants_for_men"|Pages=="printed_and_patterned_long_sleeve_shirts_for_men"|
                                                     Pages=="mens_jogging_pants"|Pages=="solid_long_sleeve_shirts_for_men"|
                                                     Pages=="men_new_arrival_ts_and_polos"|Pages=="mens_long_sleeve_plaid_and_checkered_shirts"|
                                                     Pages=="polos_new_arrivals_men_C1026439"|Pages=="blue_long_sleeve_shirts_for_men"|
                                                     Pages=="mens_new_arrival_sweatshirts"|Pages=="mens_striped_long_sleeve_shirts"|
                                                     Pages=="shirts_slim_shop_men_C1018344"|Pages=="discount_mens_shirts"|
                                                     Pages=="discount_polo_shirts",
                                                   select=names(Bounce_Rate)),row.names=NULL) 

Visits_Mens_Sweater <- data.frame(subset(Visits,
                                         Pages=="joggers_new_arrivals_men_C1018759"|Pages=="mens_polo_shirts"|
                                           Pages=="khakis_slim_shop_men_C1017207"|Pages=="mens_pique_polo_shirts"|
                                           Pages=="meet_the_new_khaki_khakis_C1026594"|Pages=="lived_in_polo_shirts_for_men"|
                                           Pages=="skinny_pants_for_men"|Pages=="mens_sweatshirts"|
                                           Pages=="mens_khaki_pants"|Pages=="straight_leg_pants_for_men"|
                                           Pages=="mens_short_sleeve_shirts"|Pages=="relaxed_fit_pants_for_men"|
                                           Pages=="printed_and_patterned_long_sleeve_shirts_for_men"|
                                           Pages=="mens_jogging_pants"|Pages=="solid_long_sleeve_shirts_for_men"|
                                           Pages=="men_new_arrival_ts_and_polos"|Pages=="mens_long_sleeve_plaid_and_checkered_shirts"|
                                           Pages=="polos_new_arrivals_men_C1026439"|Pages=="blue_long_sleeve_shirts_for_men"|
                                           Pages=="mens_new_arrival_sweatshirts"|Pages=="mens_striped_long_sleeve_shirts"|
                                           Pages=="shirts_slim_shop_men_C1018344"|Pages=="discount_mens_shirts"|
                                           Pages=="discount_polo_shirts",
                                         select=names(Bounce_Rate)),row.names=NULL) 

#rm(Bounce_Rate,Avg_Time_On_Page,Visits,Prod_Page_Views,Page_Views)

# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
#                                       || Merge data  ||
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------

GAP_Pages <- data.frame(Bounce_Rate_Mens_Sweater[,1],as.numeric(rowMeans(Bounce_Rate_Mens_Sweater[,-1])),
                        as.numeric(rowMeans(Avg_Time_On_Page_Mens_Sweater[,-1])),
                        as.numeric(rowMeans(Visits_Mens_Sweater[,-1])),row.names=NULL)
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
write.csv(GAP_Pages_Dist,"C:/Yashwanth/Clustering/2.Distance_Matrix/5.Mens Sweater Group Five/GAP_Pages_Mens_Sweater_Dist_data2_v2.csv")
rm(list=ls(pattern="GAP"));rm(list=ls(pattern="Sweater"));rm(list=ls(pattern="Means"))

# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
