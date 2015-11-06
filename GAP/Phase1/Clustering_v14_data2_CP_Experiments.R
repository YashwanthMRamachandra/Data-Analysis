# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
#                                       ||     Randomised Block Design    ||
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------

setwd("C:/Yashwanth/Clustering/1.Input_data/Data_2")
Bounce_Rate <- read.table(paste0(getwd(),"/Bounce_rate.csv"), 
                          header = TRUE, sep = ",", quote = "\"", dec = "." ,fill=TRUE, comment.char="", as.is=TRUE)

Avg_Time_On_Page <- read.table(paste0(getwd(),"/AverageTimeOnPage.csv"), 
                               header = TRUE, sep = ",", quote = "\"", dec = "." ,fill=TRUE, comment.char="", as.is=TRUE)

Visits <- read.table(paste0(getwd(),"/Visits.csv"), 
                     header = TRUE, sep = ",", quote = "\"", dec = "." ,fill=TRUE, comment.char="", as.is=TRUE)

# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
#                                       ||  Create Control & Test group   ||
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------

#--------------------------------------------- Control Group --------------------------------------------------

PAGES_CP_BR_CG  <- data.frame(subset(Bounce_Rate,Pages=="mens_sweatshirts"|Pages=="mens_crewneck_sweaters"|
                                      Pages=="mens_pants"|Pages=="mens_skinny_jeans"|
                                      Pages=="slim_jeans_for_men"|Pages=="sweaters_for_men"|
                                      Pages=="discount_mens_shirts"|Pages=="mens_polo_shirts"|
                                      Pages=="mens_socks"|Pages=="sleep_lounge_new_arrivals_C90802"|
                                      Pages=="relaxed_fit_pants_for_men"|Pages=="mens_pique_polo_shirts"|
                                      Pages=="mens_boot_cut_jeans"|Pages=="trendy_sweaters_for_men"|
                                      Pages=="mens_new_arrival_sweatshirts",
                                      select=names(Bounce_Rate)),row.names=NULL)

PAGES_CP_AVTOP_CG <- data.frame(subset(Avg_Time_On_Page,Pages=="mens_sweatshirts"|Pages=="mens_crewneck_sweaters"|
                                        Pages=="mens_pants"|Pages=="mens_skinny_jeans"|
                                        Pages=="slim_jeans_for_men"|Pages=="sweaters_for_men"|
                                        Pages=="discount_mens_shirts"|Pages=="mens_polo_shirts"|
                                        Pages=="mens_socks"|Pages=="sleep_lounge_new_arrivals_C90802"|
                                        Pages=="relaxed_fit_pants_for_men"|Pages=="mens_pique_polo_shirts"|
                                        Pages=="mens_boot_cut_jeans"|Pages=="trendy_sweaters_for_men"|
                                        Pages=="mens_new_arrival_sweatshirts",
                                      select=names(Avg_Time_On_Page)),row.names=NULL)

PAGES_CP_Visits_CG <- data.frame(subset(Visits,Pages=="mens_sweatshirts"|Pages=="mens_crewneck_sweaters"|
                                         Pages=="mens_pants"|Pages=="mens_skinny_jeans"|
                                         Pages=="slim_jeans_for_men"|Pages=="sweaters_for_men"|
                                         Pages=="discount_mens_shirts"|Pages=="mens_polo_shirts"|
                                         Pages=="mens_socks"|Pages=="sleep_lounge_new_arrivals_C90802"|
                                         Pages=="relaxed_fit_pants_for_men"|Pages=="mens_pique_polo_shirts"|
                                         Pages=="mens_boot_cut_jeans"|Pages=="trendy_sweaters_for_men"|
                                         Pages=="mens_new_arrival_sweatshirts",
                                       select=names(Visits)),row.names=NULL)

#--------------------------------------------- Test Group --------------------------------------------------

PAGES_CP_BR_TG  <- data.frame(subset(Bounce_Rate,Pages=="mens_short_sleeve_shirts"|Pages=="mens_flat_front_shorts"|
                                      Pages=="mens_t_shirts"|Pages=="mens_shoes"|
                                      Pages=="cargo_shorts_for_men"|Pages=="mens_khaki_pants"|
                                      Pages=="mens_shirts_on_sale"|Pages=="polos_new_arrivals_men_C1026439"|
                                      Pages=="boxer_briefs"|Pages=="mens_briefs"|
                                      Pages=="straight_leg_pants_for_men"|Pages=="new_pants_for_men"|
                                      Pages=="mens_straight_leg_jeans"|Pages=="trendy_mens_shorts"|
                                      Pages=="mens_sweaters_on_sale",
                                      select=names(Bounce_Rate)),row.names=NULL)

PAGES_CP_AVTOP_TG <- data.frame(subset(Avg_Time_On_Page,Pages=="mens_short_sleeve_shirts"|Pages=="mens_flat_front_shorts"|
                                        Pages=="mens_t_shirts"|Pages=="mens_shoes"|
                                        Pages=="cargo_shorts_for_men"|Pages=="mens_khaki_pants"|
                                        Pages=="mens_shirts_on_sale"|Pages=="polos_new_arrivals_men_C1026439"|
                                        Pages=="boxer_briefs"|Pages=="mens_briefs"|
                                        Pages=="straight_leg_pants_for_men"|Pages=="new_pants_for_men"|
                                        Pages=="mens_straight_leg_jeans"|Pages=="trendy_mens_shorts"|
                                        Pages=="mens_sweaters_on_sale",
                                      select=names(Avg_Time_On_Page)),row.names=NULL)

PAGES_CP_Visits_TG <- data.frame(subset(Visits,Pages=="mens_short_sleeve_shirts"|Pages=="mens_flat_front_shorts"|
                                         Pages=="mens_t_shirts"|Pages=="mens_shoes"|
                                         Pages=="cargo_shorts_for_men"|Pages=="mens_khaki_pants"|
                                         Pages=="mens_shirts_on_sale"|Pages=="polos_new_arrivals_men_C1026439"|
                                         Pages=="boxer_briefs"|Pages=="mens_briefs"|
                                         Pages=="straight_leg_pants_for_men"|Pages=="new_pants_for_men"|
                                         Pages=="mens_straight_leg_jeans"|Pages=="trendy_mens_shorts"|
                                         Pages=="mens_sweaters_on_sale",
                                       select=names(Visits)),row.names=NULL)

rm(Bounce_Rate,Avg_Time_On_Page,Visits)

# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
#                                       ||    Merge Control & Test group    ||
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------

GAP_Pages_CP_CG <- data.frame(PAGES_CP_BR_CG[,1],as.numeric(rowMeans(PAGES_CP_BR_CG[,-1])),
                        as.numeric(rowMeans(PAGES_CP_AVTOP_CG[,-1])),
                        as.numeric(rowMeans(PAGES_CP_Visits_CG[,-1])),row.names=NULL)
names(GAP_Pages_CP_CG) <- c("Pages","Bounce_Rate","Avg_Time_On_Page","Visits")

GAP_Pages_CP_TG <- data.frame(PAGES_CP_BR_TG[,1],as.numeric(rowMeans(PAGES_CP_BR_TG[,-1])),
                               as.numeric(rowMeans(PAGES_CP_AVTOP_TG[,-1])),
                               as.numeric(rowMeans(PAGES_CP_Visits_TG[,-1])),row.names=NULL)
names(GAP_Pages_CP_TG) <- c("Pages","Bounce_Rate","Avg_Time_On_Page","Visits")

rm(list=ls(pattern="PAGES"))

GAP_Pages_CP_BR <- data.frame(GAP_Pages_CP_CG[,1:2],GAP_Pages_CP_TG[,1:2],row.names=NULL)
names(GAP_Pages_CP_BR) <- c("Control","Bounce_Rate_CG","Test","Bounce_Rate_TG")

GAP_Pages_CP_ATOP <- data.frame(GAP_Pages_CP_CG[,c(1,3)],GAP_Pages_CP_TG[,c(1,3)],row.names=NULL)
names(GAP_Pages_CP_ATOP) <- c("Control","ATOP_CG","Test","ATOP_TG")

GAP_Pages_CP_Vis <- data.frame(GAP_Pages_CP_CG[,c(1,4)],GAP_Pages_CP_TG[,c(1,4)],row.names=NULL)
names(GAP_Pages_CP_Vis) <- c("Control","Visits_CG","Test","Visits_TG")

# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
#                                       ||    Paired-t-test    ||
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------

#-------------------------------------------- Bounce Rate ------------------------------------------------------

# Iteration One
# Hypothesis : There is no significant Mean diff between Control group Mean & Test group means
# H_0 : Control Mean < Test Mean V/s H_1 : Control Mean > Test Mean 

t.test(GAP_Pages_CP_BR[,2],GAP_Pages_CP_BR[,4],paired=T,alternative="greater")
# t = 0.0037, df = 14, p-value = 0.4985


# Iteration Two : Decrease in Bounce rate by 11%
# Hypothesis : There is no significant Mean diff between Control group Mean & Test group means
# H_0 : Control Mean < Test Mean V/s H_1 : Control Mean > Test Mean 
GAP_Pages_CP_BR$Bounce_Rate_TG_It2 <- (GAP_Pages_CP_BR$Bounce_Rate_TG)-((GAP_Pages_CP_BR$Bounce_Rate_TG/100)*11)
t.test(GAP_Pages_CP_BR[,2],GAP_Pages_CP_BR[,5],paired=T,alternative="greater")
# t = 1.7238, df = 14, p-value = 0.05337
# Conclusion : Since P < 0.05, we reject NH and conclude that Test Group Means are less than Control Group Means
# Test Group Pages are better than Control Group Pages

#-------------------------------------------- Average Time On Page ------------------------------------------------------

# Iteration One
# Hypothesis : There is no significant Mean diff between Control group Mean & Test group means
# H_0 : Control Mean > Test Mean V/s H_1 : Control Mean < Test Mean 

t.test(GAP_Pages_CP_ATOP[,2],GAP_Pages_CP_ATOP[,4],paired=T,alternative="less")
# t = -0.0902, df = 14, p-value = 0.4647


# Iteration Two : Increase in Average Time On Page by 11%
# Hypothesis : There is no significant Mean diff between Control group Mean & Test group means
# H_0 : Control Mean < Test Mean V/s H_1 : Control Mean > Test Mean 
GAP_Pages_CP_ATOP$ATOP_TG_It2 <- (GAP_Pages_CP_ATOP$ATOP_TG)+((GAP_Pages_CP_ATOP$ATOP_TG/100)*11)
t.test(GAP_Pages_CP_ATOP[,2],GAP_Pages_CP_ATOP[,5],paired=T,alternative="less")
# t = -1.7612, df = 14, p-value = 0.05001
# Conclusion : Since P < 0.05, we reject NH and conclude that Test Group Means are greater than Control Group Means
# Test Group Pages are better than Control Group Pages


#-------------------------------------------- Visits ------------------------------------------------------

# Iteration One
# Hypothesis : There is no significant Mean diff between Control group Mean & Test group means
# H_0 : Control Mean > Test Mean V/s H_1 : Control Mean < Test Mean 

t.test(GAP_Pages_CP_Vis[,2],GAP_Pages_CP_Vis[,4],paired=T,alternative="less")
# t = 0.0734, df = 14, p-value = 0.5288


# Iteration Two : Increase in Visits by 78%
# Hypothesis : There is no significant Mean diff between Control group Mean & Test group means
# H_0 : Control Mean < Test Mean V/s H_1 : Control Mean > Test Mean 
GAP_Pages_CP_Vis$Visits_TG_It2 <- (GAP_Pages_CP_Vis$Visits_TG)+((GAP_Pages_CP_Vis$Visits_TG/100)*78)
t.test(GAP_Pages_CP_Vis[,2],GAP_Pages_CP_Vis[,5],paired=T,alternative="less")
# t = -1.7141, df = 14, p-value = 0.05428
# Conclusion : Since P < 0.05, we reject NH and conclude that Test Group Means are greater than Control Group Means
# Test Group Pages are better than Control Group Pages


# ---------------------------------------------------------------------------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------------------------------------------------------------------------
