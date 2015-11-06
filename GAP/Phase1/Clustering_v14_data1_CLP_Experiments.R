# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
#                                       ||     Randomised Block Design    ||
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------

setwd("C:/Yashwanth/Clustering/1.Input_data/Data_1")
Bounce_Rate <- read.table(paste0(getwd(),"/Bounce_rate.csv"), 
                          header = TRUE, sep = ",", quote = "\"", dec = "." ,fill=TRUE, comment.char="", as.is=TRUE)

Avg_Time_On_Page <- read.table(paste0(getwd(),"/AvgTimeOnPage.csv"), 
                               header = TRUE, sep = ",", quote = "\"", dec = "." ,fill=TRUE, comment.char="", as.is=TRUE)

Visits <- read.table(paste0(getwd(),"/Visits.csv"), 
                     header = TRUE, sep = ",", quote = "\"", dec = "." ,fill=TRUE, comment.char="", as.is=TRUE)

# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
#                                       ||  Create Control & Test group   ||
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------

#--------------------------------------------- Control Group --------------------------------------------------

PAGES_CLP_BR_CG  <- data.frame(subset(Bounce_Rate,Pages=="mens_low_rise_jeans "|Pages=="mens_wool_coats"|
                                        Pages=="mens_button_fly_jeans "|Pages=="mens_clothing_collections"|
                                        Pages=="mens_flannel_shirt "|Pages=="mens_gloves"|
                                        Pages=="mens_jean_jacket "|Pages=="men_shorts"|
                                        Pages=="mens_vests "|Pages=="mens_polo"|
                                        Pages=="tall_men_jeans "|Pages=="navy_pants_for_men"|
                                        Pages=="v_neck_shirts_for_men"|Pages=="1969_jeans_store",
                                      select=names(Bounce_Rate)),row.names=NULL)

PAGES_CLP_AVTOP_CG <- data.frame(subset(Avg_Time_On_Page,Pages=="mens_low_rise_jeans "|Pages=="mens_wool_coats"|
                                          Pages=="mens_button_fly_jeans "|Pages=="mens_clothing_collections"|
                                          Pages=="mens_flannel_shirt "|Pages=="mens_gloves"|
                                          Pages=="mens_jean_jacket "|Pages=="men_shorts"|
                                          Pages=="mens_vests "|Pages=="mens_polo"|
                                          Pages=="tall_men_jeans "|Pages=="navy_pants_for_men"|
                                          Pages=="v_neck_shirts_for_men"|Pages=="1969_jeans_store",
                                        select=names(Avg_Time_On_Page)),row.names=NULL)

PAGES_CLP_Visits_CG <- data.frame(subset(Visits,Pages=="mens_low_rise_jeans "|Pages=="mens_wool_coats"|
                                           Pages=="mens_button_fly_jeans "|Pages=="mens_clothing_collections"|
                                           Pages=="mens_flannel_shirt "|Pages=="mens_gloves"|
                                           Pages=="mens_jean_jacket "|Pages=="men_shorts"|
                                           Pages=="mens_vests "|Pages=="mens_polo"|
                                           Pages=="tall_men_jeans "|Pages=="navy_pants_for_men"|
                                           Pages=="v_neck_shirts_for_men"|Pages=="1969_jeans_store",
                                         select=names(Visits)),row.names=NULL)

#--------------------------------------------- Test Group --------------------------------------------------

PAGES_CLP_BR_TG  <- data.frame(subset(Bounce_Rate,Pages=="mens_carpenter_jeans"|Pages=="mens_leather_jackets"|
                                        Pages=="tall_men_jeans"|Pages=="mens_tall"|
                                        Pages=="mens_plaid_shirts"|Pages=="baggy_fit_jeans"|
                                        Pages=="sport_jacket"|Pages=="relaxed_jeans"|
                                        Pages=="mens_denim_jacket"|Pages=="mens_tees"|
                                        Pages=="mens_button_fly_jeans"|Pages=="mens_gap_jean_reviews"|
                                        Pages=="funny_t_shirts_for_men"|Pages=="loose_jeans",
                                      select=names(Bounce_Rate)),row.names=NULL)

PAGES_CLP_AVTOP_TG <- data.frame(subset(Avg_Time_On_Page,Pages=="mens_carpenter_jeans"|Pages=="mens_leather_jackets"|
                                          Pages=="tall_men_jeans"|Pages=="mens_tall"|
                                          Pages=="mens_plaid_shirts"|Pages=="baggy_fit_jeans"|
                                          Pages=="sport_jacket"|Pages=="relaxed_jeans"|
                                          Pages=="mens_denim_jacket"|Pages=="mens_tees"|
                                          Pages=="mens_button_fly_jeans"|Pages=="mens_gap_jean_reviews"|
                                          Pages=="funny_t_shirts_for_men"|Pages=="loose_jeans",
                                        select=names(Avg_Time_On_Page)),row.names=NULL)

PAGES_CLP_Visits_TG <- data.frame(subset(Visits,Pages=="mens_carpenter_jeans"|Pages=="mens_leather_jackets"|
                                           Pages=="tall_men_jeans"|Pages=="mens_tall"|
                                           Pages=="mens_plaid_shirts"|Pages=="baggy_fit_jeans"|
                                           Pages=="sport_jacket"|Pages=="relaxed_jeans"|
                                           Pages=="mens_denim_jacket"|Pages=="mens_tees"|
                                           Pages=="mens_button_fly_jeans"|Pages=="mens_gap_jean_reviews"|
                                           Pages=="funny_t_shirts_for_men"|Pages=="loose_jeans",
                                         select=names(Visits)),row.names=NULL)

rm(Bounce_Rate,Avg_Time_On_Page,Visits)

# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
#                                       ||    Merge Control & Test group    ||
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------

GAP_Pages_CLP_CG <- data.frame(PAGES_CLP_BR_CG[,1],as.numeric(rowMeans(PAGES_CLP_BR_CG[,-1])),
                               as.numeric(rowMeans(PAGES_CLP_AVTOP_CG[,-1])),
                               as.numeric(rowMeans(PAGES_CLP_Visits_CG[,-1])),row.names=NULL)
names(GAP_Pages_CLP_CG) <- c("Pages","Bounce_Rate","Avg_Time_On_Page","Visits")

GAP_Pages_CLP_TG <- data.frame(PAGES_CLP_BR_TG[,1],as.numeric(rowMeans(PAGES_CLP_BR_TG[,-1])),
                               as.numeric(rowMeans(PAGES_CLP_AVTOP_TG[,-1])),
                               as.numeric(rowMeans(PAGES_CLP_Visits_TG[,-1])),row.names=NULL)
names(GAP_Pages_CLP_TG) <- c("Pages","Bounce_Rate","Avg_Time_On_Page","Visits")

rm(list=ls(pattern="PAGES"))

GAP_Pages_CLP_BR <- data.frame(GAP_Pages_CLP_CG[,1:2],GAP_Pages_CLP_TG[,1:2],row.names=NULL)
names(GAP_Pages_CLP_BR) <- c("Control","Bounce_Rate_CG","Test","Bounce_Rate_TG")

GAP_Pages_CLP_ATOP <- data.frame(GAP_Pages_CLP_CG[,c(1,3)],GAP_Pages_CLP_TG[,c(1,3)],row.names=NULL)
names(GAP_Pages_CLP_ATOP) <- c("Control","ATOP_CG","Test","ATOP_TG")

GAP_Pages_CLP_Vis <- data.frame(GAP_Pages_CLP_CG[,c(1,4)],GAP_Pages_CLP_TG[,c(1,4)],row.names=NULL)
names(GAP_Pages_CLP_Vis) <- c("Control","Visits_CG","Test","Visits_TG")

# ---------------------------------------------------------------------------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------------------------------------------------------------------------
