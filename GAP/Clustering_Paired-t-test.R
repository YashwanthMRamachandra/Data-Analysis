# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
#                                       ||  Group One Bounce Rate   ||
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------

Bounce_Rate <- read.table("C:/Yashwanth/Clustering/1.Input_data/Bounce_rate.csv", 
                          header = TRUE, sep = ",", quote = "\"", dec = "." ,fill=TRUE, comment.char="", as.is=TRUE)


# --------------------------------------------------------------------------------------------------------------
#                                       ||  Create Control and Test groups    ||
# --------------------------------------------------------------------------------------------------------------

Bounce_Rate_Control <- data.frame(subset(Bounce_Rate,Pages=="mens_tall_skinny_jeans" | Pages=="relaxed_fit_jeans" |
                                           Pages=="tall_men_jeans" | Pages=="mens_carpenter_jeans" |
                                           Pages=="low_rise_mens_jeans",names(Bounce_Rate)),row.names=NULL)

Bounce_Rate_Test <- data.frame(subset(Bounce_Rate,Pages=="baggy_fit_jeans" | Pages=="loose_jeans" |
                                           Pages=="mens_button_fly_jeans" | Pages=="mens_low_rise_jeans" |
                                           Pages=="button_fly_jeans_men",names(Bounce_Rate)),row.names=NULL)

Bounce_Rate_Control_Means <- cbind(Bounce_Rate_Control,rowMeans(Bounce_Rate_Control[,-1]))
Bounce_Rate_Control_Means <- Bounce_Rate_Control_Means[,c(1,ncol(Bounce_Rate_Control_Means))]
names(Bounce_Rate_Control_Means) <- c("Pages","Control_Means")

Bounce_Rate_Test_Means <- cbind(Bounce_Rate_Test,rowMeans(Bounce_Rate_Test[,-1]))
Bounce_Rate_Test_Means <- Bounce_Rate_Test_Means[,c(1,ncol(Bounce_Rate_Test_Means))]
names(Bounce_Rate_Test_Means) <- c("Pages","Test_Means_one")

Bounce_Rate_Paired <- cbind(Bounce_Rate_Control_Means,Bounce_Rate_Test_Means)
Bounce_Rate_Paired$Diff <- Bounce_Rate_Paired[,2]-Bounce_Rate_Paired[,4]
rm(list=ls(pattern="Test"));rm(list=ls(pattern="Control"))

# --------------------------------------------------------------------------------------------------------------
#                                       ||  Paired-t-test    ||
# --------------------------------------------------------------------------------------------------------------

# ---------------------------------------- Test One --------------------------------------------------

# Null Hypothesis : There is no significant diff b/n Test & Control means : Test & Control Means are same : M1=M2

t.test(Bounce_Rate_Paired[,4],Bounce_Rate_Paired[,2],paired=TRUE)

# t= +/- 0.2245, df=4, p=0.8334
# Conclusion : Since p > 0.05(LOS) , we fail to reject null hypothesis and conclude that there is no sufficient
#             data to prove Test & Control Means are same.

# ---------------------------------------- Test Two : Decrease Bounce rate by 20% -------------------------------------

# Hypothesis : There is no significant diff b/n Test & Control means : Test & Control Means are same : M1=M2

Bounce_Rate_Paired$Test_Means_Two <- (Bounce_Rate_Paired$Test_Means_one)-((Bounce_Rate_Paired$Test_Means_one/100)*20)
t.test(Bounce_Rate_Paired[,2],Bounce_Rate_Paired[,6],paired=TRUE)

# t= +/- 1.1187, df=4, p=0.3259
# Conclusion : Since p > 0.05(LOS) , we fail to reject null hypothesis and conclude that there is no sufficient
#             data to prove Test & Control Means are same.


# ---------------------------------------- Test Three : Decrease Bounce rate by 40% -------------------------------------

# Hypothesis : There is no significant diff b/n Test & Control means :  Test & Control Means are same : M1=M2

Bounce_Rate_Paired$Test_Means_Three <- (Bounce_Rate_Paired$Test_Means_one)-((Bounce_Rate_Paired$Test_Means_one/100)*40)

t.test(Bounce_Rate_Paired[,2],Bounce_Rate_Paired[,7],paired=TRUE)

# t= +/- 2.7401, df=4, p=0.0519
# Conclusion-1: Since p < 0.05(LOS) , we reject null hypothesis and conclude that there is a significant difference
# Test & Control Means.
# Conclusion-2: Hence, if Bounce rate reduced by 40%(minimum) then these pairs are evident enough to consider 
#               as pairs.

# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
#                                       ||  Group One Average Time On Page    ||
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------

Avg_TOP <- read.table("C:/Yashwanth/Clustering/1.Input_data/AvgTimeOnPage.csv", 
                          header = TRUE, sep = ",", quote = "\"", dec = "." ,fill=TRUE, comment.char="", as.is=TRUE)


# --------------------------------------------------------------------------------------------------------------
#                                       ||  Create Control and Test groups    ||
# --------------------------------------------------------------------------------------------------------------

Avg_TOP_Control <- data.frame(subset(Avg_TOP,Pages=="mens_tall_skinny_jeans" | Pages=="relaxed_fit_jeans" |
                                       Pages=="tall_men_jeans" | Pages=="mens_carpenter_jeans" |
                                       Pages=="low_rise_mens_jeans",names(Avg_TOP)),row.names=NULL)

Avg_TOP_Test <- data.frame(subset(Avg_TOP,Pages=="baggy_fit_jeans" | Pages=="loose_jeans" |
                                    Pages=="mens_button_fly_jeans" | Pages=="mens_low_rise_jeans" |
                                    Pages=="button_fly_jeans_men",names(Avg_TOP)),row.names=NULL)

Avg_TOP_Control_Means <- cbind(Avg_TOP_Control,rowMeans(Avg_TOP_Control[,-1]))
Avg_TOP_Control_Means <- Avg_TOP_Control_Means[,c(1,ncol(Avg_TOP_Control_Means))]
names(Avg_TOP_Control_Means) <- c("Pages","Control_Means")

Avg_TOP_Test_Means <- cbind(Avg_TOP_Test,rowMeans(Avg_TOP_Test[,-1]))
Avg_TOP_Test_Means <- Avg_TOP_Test_Means[,c(1,ncol(Avg_TOP_Test_Means))]
names(Avg_TOP_Test_Means) <- c("Pages","Test_Means_one" )

Avg_TOP_Paired <- cbind(Avg_TOP_Control_Means,Avg_TOP_Test_Means)
Avg_TOP_Paired$Diff <- Avg_TOP_Paired[,2]-Avg_TOP_Paired[,4]
rm(list=ls(pattern="Test"));rm(list=ls(pattern="Control"))

# --------------------------------------------------------------------------------------------------------------
#                                       ||  Paired-t-test    ||
# --------------------------------------------------------------------------------------------------------------

# ---------------------------------------- Test One --------------------------------------------------

# Hypothesis : Test & Control Means are not same(not equal to 0)

t.test(Avg_TOP_Paired[,4],Avg_TOP_Paired[,2],paired=TRUE)

# t= +/- 0.2245, df=4, p=0.8334
# Conclusion : Since p > 0.05(LOS) , we fail to reject null hypothesis and conclude that there is no sufficient
#             data to prove Test & Control Means are not same.

# ---------------------------------------- Test Two : Increase Bounce rate by 80% -------------------------------------

# Hypothesis : Test & Control Means are not same(not equal to 0)

Avg_TOP_Paired$Test_Means_Two <- (Avg_TOP_Paired$Test_Means_one)+((Avg_TOP_Paired$Test_Means_one/100)*80)

t.test(Avg_TOP_Paired[,6],Avg_TOP_Paired[,2],paired=TRUE)

# t= +/- 1.6642, df=4, p=0.1714
# Conclusion : Since p > 0.05(LOS) , we fail to reject null hypothesis and conclude that there is no sufficient
#             data to prove Test & Control Means are not same.

# ---------------------------------------- Test Three : Increase Bounce rate by 180% -------------------------------------

# Hypothesis : Test & Control Means are not same(not equal to 0)

Avg_TOP_Paired$Test_Means_Three <- (Avg_TOP_Paired$Test_Means_one)+((Avg_TOP_Paired$Test_Means_one/100)*180)

t.test(Avg_TOP_Paired[,7],Avg_TOP_Paired[,2],paired=TRUE)

# t= +/- 2.7049, df=4, p=0.05382
# Conclusion-1: Since p < 0.05(LOS) , we reject null hypothesis and conclude that Test & Control Means are not same.
# Conclusion-2: Hence, if Bounce rate reduced by 40%(minimum) then these pairs are evident enough to consider 
#               as pairs.

# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
#                                       ||  Group One Visits   ||
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------

Visits <- read.table("C:/Yashwanth/Clustering/1.Input_data/Visits.csv", 
                     header = TRUE, sep = ",", quote = "\"", dec = "." ,fill=TRUE, comment.char="", as.is=TRUE)


# --------------------------------------------------------------------------------------------------------------
#                                       ||  Create Control and Test groups    ||
# --------------------------------------------------------------------------------------------------------------

Visits_Control <- data.frame(subset(Visits,Pages=="mens_tall_skinny_jeans" | Pages=="relaxed_fit_jeans" |
                                      Pages=="tall_men_jeans" | Pages=="mens_carpenter_jeans" |
                                      Pages=="low_rise_mens_jeans",names(Visits)),row.names=NULL)

Visits_Test <- data.frame(subset(Visits,Pages=="baggy_fit_jeans" | Pages=="loose_jeans" |
                                   Pages=="mens_button_fly_jeans" | Pages=="mens_low_rise_jeans" |
                                   Pages=="button_fly_jeans_men",names(Visits)),row.names=NULL)

Visits_Control_Means <- cbind(Visits_Control,rowMeans(Visits_Control[,-1]))
Visits_Control_Means <- Visits_Control_Means[,c(1,ncol(Visits_Control_Means))]
names(Visits_Control_Means) <- c("Pages","Control_Means")

Visits_Test_Means <- cbind(Visits_Test,rowMeans(Visits_Test[,-1]))
Visits_Test_Means <- Visits_Test_Means[,c(1,ncol(Visits_Test_Means))]
names(Visits_Test_Means) <- c("Pages","Test_Means_one" )

Visits_Paired <- cbind(Visits_Control_Means,Visits_Test_Means)
Visits_Paired$Diff <- Visits_Paired[,2]-Visits_Paired[,4]
rm(list=ls(pattern="Test"));rm(list=ls(pattern="Control"))

# --------------------------------------------------------------------------------------------------------------
#                                       ||  Paired-t-test    ||
# --------------------------------------------------------------------------------------------------------------

# ---------------------------------------- Test One --------------------------------------------------

# Hypothesis : Test & Control Means are not same(not equal to 0)

t.test(Visits_Paired[,2],Visits_Paired[,4],paired=TRUE)

# t= +/- 0.0416, df=4, p=0.9688
# Conclusion : Since p > 0.05(LOS) , we fail to reject null hypothesis and conclude that there is no sufficient
#             data to prove Test & Control Means are not same.

# ---------------------------------------- Test Two : Increase Visits by 60% -------------------------------------

# Hypothesis : Test & Control Means are not same(not equal to 0)

Visits_Paired$Test_Means_Two <- (Visits_Paired$Test_Means_one)+((Visits_Paired$Test_Means_one/100)*60)
t.test(Visits_Paired[,2],Visits_Paired[,6],paired=TRUE)

# t= +/- 0.9702, df=4, p=0.0.3869
# Conclusion : Since p > 0.05(LOS) , we fail to reject null hypothesis and conclude that there is no sufficient
#             data to prove Test & Control Means are not same.


# ---------------------------------------- Test Two : Decrease Bounce rate by 110% -------------------------------------

# Hypothesis : Test & Control Means are not same(not equal to 0)

Visits_Paired$Test_Means_Three <- (Visits_Paired$Test_Means_one)-((Visits_Paired$Test_Means_one/100)*110)

t.test(Visits_Paired[,2],Visits_Paired[,7],paired=TRUE)

# t= +/- 2.6906, df=4, p=0.05463
# Conclusion-1: Since p > 0.05(LOS) , we reject null hypothesis and conclude that Test & Control Means are not same.
# Conclusion-2: Hence, if Bounce rate reduced by 40%(minimum) then these pairs are evident enough to consider 
#               as pairs.


# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
#                                       ||  Group One PPV   ||
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------

PPV <- read.table("C:/Yashwanth/Clustering/1.Input_data/ProductPageViews.csv", 
                  header = TRUE, sep = ",", quote = "\"", dec = "." ,fill=TRUE, comment.char="", as.is=TRUE)

# --------------------------------------------------------------------------------------------------------------
#                                       ||  Create Control and Test groups    ||
# --------------------------------------------------------------------------------------------------------------

PPV_Control <- data.frame(subset(PPV,Pages=="mens_tall_skinny_jeans" | Pages=="relaxed_fit_jeans" |
                                   Pages=="tall_men_jeans" | Pages=="mens_carpenter_jeans" |
                                   Pages=="low_rise_mens_jeans",names(PPV)),row.names=NULL)

PPV_Test <- data.frame(subset(PPV,Pages=="baggy_fit_jeans" | Pages=="loose_jeans" |
                                Pages=="mens_button_fly_jeans" | Pages=="mens_low_rise_jeans" |
                                Pages=="button_fly_jeans_men",names(PPV)),row.names=NULL)

PPV_Control_Means <- cbind(PPV_Control,rowMeans(PPV_Control[,-1]))
PPV_Control_Means <- PPV_Control_Means[,c(1,ncol(PPV_Control_Means))]
names(PPV_Control_Means) <- c("Pages","Control_Means")

PPV_Test_Means <- cbind(PPV_Test,rowMeans(PPV_Test[,-1]))
PPV_Test_Means <- PPV_Test_Means[,c(1,ncol(PPV_Test_Means))]
names(PPV_Test_Means) <- c("Pages","Test_Means_one" )

PPV_Paired <- cbind(PPV_Control_Means,PPV_Test_Means)
PPV_Paired$Diff <- PPV_Paired[,2]-PPV_Paired[,4]
rm(list=ls(pattern="Test"));rm(list=ls(pattern="Control"))

# --------------------------------------------------------------------------------------------------------------
#                                       ||  Paired-t-test    ||
# --------------------------------------------------------------------------------------------------------------

# ---------------------------------------- Test One --------------------------------------------------

# Hypothesis : Test & Control Means are not same(not equal to 0)

t.test(PPV_Paired[,4],PPV_Paired[,2],paired=TRUE)

# t= +/- 0.3856, df=4, p=0.7194
# Conclusion : Since p > 0.05(LOS) , we fail to reject null hypothesis and conclude that there is no sufficient
#             data to prove Test & Control Means are not same.

# ---------------------------------------- Test Two : Increase in Visits by 60% -------------------------------------

# Hypothesis : Test & Control Means are not same(not equal to 0)

PPV_Paired$Test_Means_Two <- (PPV_Paired$Test_Means_one)+((PPV_Paired$Test_Means_one/100)*60)
t.test(PPV_Paired[,2],PPV_Paired[,6],paired=TRUE)

# t= +/- 1.0987, df=4, p=0.3336
# Conclusion : Since p > 0.05(LOS) , we fail to reject null hypothesis and conclude that there is no sufficient
#             data to prove Test & Control Means are not same.


# ---------------------------------------- Test Two : Decrease Bounce rate by 95% -------------------------------------

# Hypothesis : Test & Control Means are not same(not equal to 0)

PPV_Paired$Test_Means_Three <- (PPV_Paired$Test_Means_one)-((PPV_Paired$Test_Means_one/100)*95)

t.test(PPV_Paired[,2],PPV_Paired[,7],paired=TRUE)

# t= +/- 2.6922, df=4, p=0.05454
# Conclusion-1: Since p < 0.05(LOS) , we reject null hypothesis and conclude that Test & Control Means are not same.
# Conclusion-2: Hence, if Bounce rate reduced by 40%(minimum) then these pairs are evident enough to consider 
#               as pairs.

rm(Bounce_Rate,Avg_TOP,Visits,PPV)


# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
#                                           ||  All Groups   ||
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------

# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
#                                             ||  Bounce Rate   ||
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------

# --------------------------------------------------------------------------------------------------------------
#                                       ||  Create Control and Test groups    ||
# --------------------------------------------------------------------------------------------------------------

Bounce_Rate_All_Control <- data.frame(subset(Bounce_Rate,Pages=="mens_tall_skinny_jeans" | Pages=="relaxed_fit_jeans" |
                                               Pages=="tall_men_jeans" | Pages=="mens_carpenter_jeans" |
                                               Pages=="low_rise_mens_jeans" | Pages=="mens_polo" |
                                               Pages=="deep_v_neck_t_shirts" | Pages=="mens_v_neck" |
                                               Pages=="denim_shirts_for_men" | Pages=="mens_flannel_shirt" |
                                               Pages=="mens_tank_tops" | Pages=="mens_printed_hoodies" |
                                               Pages=="mens_leather_jackets" | Pages=="mens_linen_pants" |
                                               Pages=="mens_sweat_pants" | Pages=="mens_lounge_pants" ,
                                             names(Bounce_Rate)),row.names=NULL)

Bounce_Rate_All_Test <- data.frame(subset(Bounce_Rate,Pages=="baggy_fit_jeans" | Pages=="loose_jeans" |
                                            Pages=="mens_button_fly_jeans" | Pages=="mens_low_rise_jeans" |
                                            Pages=="button_fly_jeans_men" | Pages=="mens_tees" |
                                            Pages=="mens_fitted_short_sleeve_shirts" | Pages=="mens_sweatshirt" |
                                            Pages=="v_neck_shirts_for_men" | Pages=="funny_t_shirts_for_men" |
                                            Pages=="mens_plaid_shirts" | Pages=="sport_jacket"|
                                            Pages=="mens_wool_coats" | Pages=="men_jean_leggings" |
                                            Pages=="mens_cargo_pants" | Pages=="mens_capri_pants",
                                          names(Bounce_Rate)),row.names=NULL)

Bounce_Rate_All_Control_Means <- cbind(Bounce_Rate_All_Control,rowMeans(Bounce_Rate_All_Control[,-1]))
Bounce_Rate_All_Control_Means <- Bounce_Rate_All_Control_Means[,c(1,ncol(Bounce_Rate_All_Control_Means))]
names(Bounce_Rate_All_Control_Means) <- c("Pages","Control_Means")

Bounce_Rate_All_Test_Means <- cbind(Bounce_Rate_All_Test,rowMeans(Bounce_Rate_All_Test[,-1]))
Bounce_Rate_All_Test_Means <- Bounce_Rate_All_Test_Means[,c(1,ncol(Bounce_Rate_All_Test_Means))]
names(Bounce_Rate_All_Test_Means) <- c("Pages","Test_Means_one")

Bounce_Rate_All_Paired <- cbind(Bounce_Rate_All_Control_Means,Bounce_Rate_All_Test_Means)
Bounce_Rate_All_Paired$Diff <- Bounce_Rate_All_Paired[,2]-Bounce_Rate_All_Paired[,4]
rm(list=ls(pattern="Test"));rm(list=ls(pattern="Control"));rm(Bounce_Rate)

# --------------------------------------------------------------------------------------------------------------
#                                       ||  Paired-t-test    ||
# --------------------------------------------------------------------------------------------------------------

# ---------------------------------------- Test One --------------------------------------------------

# Null Hypothesis : There is no significant diff b/n Test & Control means : Test & Control Means are same : M1=M2

t.test(Bounce_Rate_All_Paired[,4],Bounce_Rate_All_Paired[,2],paired=TRUE)

# t= +/- 0.2245, df=4, p=0.8334
# Conclusion : Since p > 0.05(LOS) , we fail to reject null hypothesis and conclude that there is no sufficient
#             data to prove Test & Control Means are same.

# ---------------------------------------- Test Two : Decrease Bounce rate by 20% -------------------------------------

# Hypothesis : There is no significant diff b/n Test & Control means : Test & Control Means are same : M1=M2

Bounce_Rate_All_Paired$Test_Means_Two <- (Bounce_Rate_All_Paired$Test_Means_one)-((Bounce_Rate_All_Paired$Test_Means_one/100)*20)
t.test(Bounce_Rate_All_Paired[,2],Bounce_Rate_All_Paired[,6],paired=TRUE)

# t= +/- 1.1187, df=4, p=0.3259
# Conclusion : Since p > 0.05(LOS) , we fail to reject null hypothesis and conclude that there is no sufficient
#             data to prove Test & Control Means are same.


# ---------------------------------------- Test Three : Decrease Bounce rate by 32.5% -------------------------------------

# Hypothesis : There is no significant diff b/n Test & Control means :  Test & Control Means are same : M1=M2

Bounce_Rate_All_Paired$Test_Means_Three <- (Bounce_Rate_All_Paired$Test_Means_one)-((Bounce_Rate_All_Paired$Test_Means_one/100)*32.5)

t.test(Bounce_Rate_All_Paired[,2],Bounce_Rate_All_Paired[,7],paired=TRUE)

# t= +/- 2.7401, df=4, p=0.0519
# Conclusion-1: Since p < 0.05(LOS) , we reject null hypothesis and conclude that there is a significant difference
# Test & Control Means.
# Conclusion-2: Hence, if Bounce rate reduced by 40%(minimum) then these pairs are evident enough to consider 
#               as pairs.



# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
#                                             ||  Average Time On Page   ||
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------

# --------------------------------------------------------------------------------------------------------------
#                                       ||  Create Control and Test groups    ||
# --------------------------------------------------------------------------------------------------------------

Avg_TOP_All_Control <- data.frame(subset(Avg_TOP,Pages=="mens_tall_skinny_jeans" | Pages=="relaxed_fit_jeans" |
                                           Pages=="tall_men_jeans" | Pages=="mens_carpenter_jeans" |
                                           Pages=="low_rise_mens_jeans" | Pages=="mens_polo" |
                                           Pages=="deep_v_neck_t_shirts" | Pages=="mens_v_neck" |
                                           Pages=="denim_shirts_for_men" | Pages=="mens_flannel_shirt" |
                                           Pages=="mens_tank_tops" | Pages=="mens_printed_hoodies" |
                                           Pages=="mens_leather_jackets" | Pages=="mens_linen_pants" |
                                           Pages=="mens_sweat_pants" | Pages=="mens_lounge_pants" ,
                                         names(Avg_TOP)),row.names=NULL)

Avg_TOP_All_Test <- data.frame(subset(Avg_TOP,Pages=="baggy_fit_jeans" | Pages=="loose_jeans" |
                                        Pages=="mens_button_fly_jeans" | Pages=="mens_low_rise_jeans" |
                                        Pages=="button_fly_jeans_men" | Pages=="mens_tees" |
                                        Pages=="mens_fitted_short_sleeve_shirts" | Pages=="mens_sweatshirt" |
                                        Pages=="v_neck_shirts_for_men" | Pages=="funny_t_shirts_for_men" |
                                        Pages=="mens_plaid_shirts" | Pages=="sport_jacket"|
                                        Pages=="mens_wool_coats" | Pages=="men_jean_leggings" |
                                        Pages=="mens_cargo_pants" | Pages=="mens_capri_pants",
                                      names(Avg_TOP)),row.names=NULL)

Avg_TOP_All_Control_Means <- cbind(Avg_TOP_All_Control,rowMeans(Avg_TOP_All_Control[,-1]))
Avg_TOP_All_Control_Means <- Avg_TOP_All_Control_Means[,c(1,ncol(Avg_TOP_All_Control_Means))]
names(Avg_TOP_All_Control_Means) <- c("Pages","Control_Means")

Avg_TOP_All_Test_Means <- cbind(Avg_TOP_All_Test,rowMeans(Avg_TOP_All_Test[,-1]))
Avg_TOP_All_Test_Means <- Avg_TOP_All_Test_Means[,c(1,ncol(Avg_TOP_All_Test_Means))]
names(Avg_TOP_All_Test_Means) <- c("Pages","Test_Means_one")

Avg_TOP_All_Paired <- cbind(Avg_TOP_All_Control_Means,Avg_TOP_All_Test_Means)
Avg_TOP_All_Paired$Diff <- Avg_TOP_All_Paired[,2]-Avg_TOP_All_Paired[,4]
rm(list=ls(pattern="Test"));rm(list=ls(pattern="Control"));rm(Avg_TOP)

# --------------------------------------------------------------------------------------------------------------
#                                       ||  Paired-t-test    ||
# --------------------------------------------------------------------------------------------------------------

# ---------------------------------------- Test One --------------------------------------------------

# Null Hypothesis : There is no significant diff b/n Test & Control means : Test & Control Means are same : M1=M2

t.test(Avg_TOP_All_Paired[,4],Avg_TOP_All_Paired[,2],paired=TRUE)

# t= +/- 0.6491, df=4, p=0.5261
# Conclusion : Since p > 0.05(LOS) , we fail to reject null hypothesis and conclude that there is no sufficient
#             data to prove Test & Control Means are same.

# ---------------------------------------- Test Two : Increase Avg TOP by 20% -------------------------------------

# Hypothesis : There is no significant diff b/n Test & Control means : Test & Control Means are same : M1=M2

Avg_TOP_All_Paired$Test_Means_Two <- (Avg_TOP_All_Paired$Test_Means_one)+((Avg_TOP_All_Paired$Test_Means_one/100)*20)
t.test(Avg_TOP_All_Paired[,2],Avg_TOP_All_Paired[,6],paired=TRUE)

# t= +/- 1.5134, df=4, p=0.151
# Conclusion : Since p > 0.05(LOS) , we fail to reject null hypothesis and conclude that there is no sufficient
#             data to prove Test & Control Means are same.


# ---------------------------------------- Test Three : Increase Avg TOP by 37% -------------------------------------

# Hypothesis : There is no significant diff b/n Test & Control means :  Test & Control Means are same : M1=M2

Avg_TOP_All_Paired$Test_Means_Three <- (Avg_TOP_All_Paired$Test_Means_one)+((Avg_TOP_All_Paired$Test_Means_one/100)*37)

t.test(Avg_TOP_All_Paired[,2],Avg_TOP_All_Paired[,7],paired=TRUE)

# t= +/- 2.1372, df=4, p=0.05249
# Conclusion-1: Since p < 0.05(LOS) , we reject null hypothesis and conclude that there is a significant difference
# Test & Control Means.
# Conclusion-2: Hence, if Bounce rate reduced by 38%(minimum) then these pairs are evident enough to consider 
#               as pairs.

# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
#                                               	||  Visits   ||
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------

# --------------------------------------------------------------------------------------------------------------
#                                       ||  Create Control and Test groups    ||
# --------------------------------------------------------------------------------------------------------------

Visits_All_Control <- data.frame(subset(Visits,Pages=="mens_tall_skinny_jeans" | Pages=="relaxed_fit_jeans" |
                                          Pages=="tall_men_jeans" | Pages=="mens_carpenter_jeans" |
                                          Pages=="low_rise_mens_jeans" | Pages=="mens_polo" |
                                          Pages=="deep_v_neck_t_shirts" | Pages=="mens_v_neck" |
                                          Pages=="denim_shirts_for_men" | Pages=="mens_flannel_shirt" |
                                          Pages=="mens_tank_tops" | Pages=="mens_printed_hoodies" |
                                          Pages=="mens_leather_jackets" | Pages=="mens_linen_pants" |
                                          Pages=="mens_sweat_pants" | Pages=="mens_lounge_pants" ,
                                        names(Visits)),row.names=NULL)

Visits_All_Test <- data.frame(subset(Visits,Pages=="baggy_fit_jeans" | Pages=="loose_jeans" |
                                       Pages=="mens_button_fly_jeans" | Pages=="mens_low_rise_jeans" |
                                       Pages=="button_fly_jeans_men" | Pages=="mens_tees" |
                                       Pages=="mens_fitted_short_sleeve_shirts" | Pages=="mens_sweatshirt" |
                                       Pages=="v_neck_shirts_for_men" | Pages=="funny_t_shirts_for_men" |
                                       Pages=="mens_plaid_shirts" | Pages=="sport_jacket"|
                                       Pages=="mens_wool_coats" | Pages=="men_jean_leggings" |
                                       Pages=="mens_cargo_pants" | Pages=="mens_capri_pants",
                                     names(Visits)),row.names=NULL)

Visits_All_Control_Means <- cbind(Visits_All_Control,rowMeans(Visits_All_Control[,-1]))
Visits_All_Control_Means <- Visits_All_Control_Means[,c(1,ncol(Visits_All_Control_Means))]
names(Visits_All_Control_Means) <- c("Pages","Control_Means")

Visits_All_Test_Means <- cbind(Visits_All_Test,rowMeans(Visits_All_Test[,-1]))
Visits_All_Test_Means <- Visits_All_Test_Means[,c(1,ncol(Visits_All_Test_Means))]
names(Visits_All_Test_Means) <- c("Pages","Test_Means_one")

Visits_All_Paired <- cbind(Visits_All_Control_Means,Visits_All_Test_Means)
Visits_All_Paired$Diff <- Visits_All_Paired[,2]-Visits_All_Paired[,4]
rm(list=ls(pattern="Test"));rm(list=ls(pattern="Control"));rm(Visits)

# --------------------------------------------------------------------------------------------------------------
#                                       ||  Paired-t-test    ||
# --------------------------------------------------------------------------------------------------------------

# ---------------------------------------- Test One --------------------------------------------------

# Null Hypothesis : There is no significant diff b/n Test & Control means : Test & Control Means are same : M1=M2

t.test(Visits_All_Paired[,4],Visits_All_Paired[,2],paired=TRUE)

# t= +/- 0.3095, df=4, p=0.7612
# Conclusion : Since p > 0.05(LOS) , we fail to reject null hypothesis and conclude that there is no sufficient
#             data to prove Test & Control Means are same.

# ---------------------------------------- Test Two : Increase Visits by 20% -------------------------------------

# Hypothesis : There is no significant diff b/n Test & Control means : Test & Control Means are same : M1=M2

Visits_All_Paired$Test_Means_Two <- (Visits_All_Paired$Test_Means_one)+((Visits_All_Paired$Test_Means_one/100)*20)
t.test(Visits_All_Paired[,2],Visits_All_Paired[,6],paired=TRUE)

# t= +/- 0.1928, df=4, p=0.8497
# Conclusion : Since p > 0.05(LOS) , we fail to reject null hypothesis and conclude that there is no sufficient
#             data to prove Test & Control Means are same.


# ---------------------------------------- Test Three : Increase Visits by 180% -------------------------------------

# Hypothesis : There is no significant diff b/n Test & Control means :  Test & Control Means are same : M1=M2

Visits_All_Paired$Test_Means_Three <- (Visits_All_Paired$Test_Means_one)+((Visits_All_Paired$Test_Means_one/100)*180)

t.test(Visits_All_Paired[,2],Visits_All_Paired[,7],paired=TRUE)

# t= +/- 2.1398, df=4, p=0.04921
# Conclusion-1: Since p < 0.05(LOS) , we reject null hypothesis and conclude that there is a significant difference
# Test & Control Means.
# Conclusion-2: Hence, if Bounce rate reduced by 40%(minimum) then these pairs are evident enough to consider 
#               as pairs.


# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
#                                               	||  Product Page Views   ||
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------

# --------------------------------------------------------------------------------------------------------------
#                                       ||  Create Control and Test groups    ||
# --------------------------------------------------------------------------------------------------------------

PPV_All__Control <- data.frame(subset(PPV,Pages=="mens_tall_skinny_jeans" | Pages=="relaxed_fit_jeans" |
                                        Pages=="tall_men_jeans" | Pages=="mens_carpenter_jeans" |
                                        Pages=="low_rise_mens_jeans" | Pages=="mens_polo" |
                                        Pages=="deep_v_neck_t_shirts" | Pages=="mens_v_neck" |
                                        Pages=="denim_shirts_for_men" | Pages=="mens_flannel_shirt" |
                                        Pages=="mens_tank_tops" | Pages=="mens_printed_hoodies" |
                                        Pages=="mens_leather_jackets" | Pages=="mens_linen_pants" |
                                        Pages=="mens_sweat_pants" | Pages=="mens_lounge_pants" ,
                                      names(PPV)),row.names=NULL)

PPV_All__Test <- data.frame(subset(PPV,Pages=="baggy_fit_jeans" | Pages=="loose_jeans" |
                                     Pages=="mens_button_fly_jeans" | Pages=="mens_low_rise_jeans" |
                                     Pages=="button_fly_jeans_men" | Pages=="mens_tees" |
                                     Pages=="mens_fitted_short_sleeve_shirts" | Pages=="mens_sweatshirt" |
                                     Pages=="v_neck_shirts_for_men" | Pages=="funny_t_shirts_for_men" |
                                     Pages=="mens_plaid_shirts" | Pages=="sport_jacket"|
                                     Pages=="mens_wool_coats" | Pages=="men_jean_leggings" |
                                     Pages=="mens_cargo_pants" | Pages=="mens_capri_pants",
                                   names(PPV)),row.names=NULL)

PPV_All__Control_Means <- cbind(PPV_All__Control,rowMeans(PPV_All__Control[,-1]))
PPV_All__Control_Means <- PPV_All__Control_Means[,c(1,ncol(PPV_All__Control_Means))]
names(PPV_All__Control_Means) <- c("Pages","Control_Means")

PPV_All__Test_Means <- cbind(PPV_All__Test,rowMeans(PPV_All__Test[,-1]))
PPV_All__Test_Means <- PPV_All__Test_Means[,c(1,ncol(PPV_All__Test_Means))]
names(PPV_All__Test_Means) <- c("Pages","Test_Means_one")

PPV_All__Paired <- cbind(PPV_All__Control_Means,PPV_All__Test_Means)
PPV_All__Paired$Diff <- PPV_All__Paired[,2]-PPV_All__Paired[,4]
rm(list=ls(pattern="Test"));rm(list=ls(pattern="Control"));rm(PPV)

# --------------------------------------------------------------------------------------------------------------
#                                       ||  Paired-t-test    ||
# --------------------------------------------------------------------------------------------------------------

# ---------------------------------------- Test One --------------------------------------------------

# Null Hypothesis : There is no significant diff b/n Test & Control means : Test & Control Means are same : M1=M2

t.test(PPV_All__Paired[,4],PPV_All__Paired[,2],paired=TRUE)

# t= +/- 0.4594, df=4, p=0.6525
# Conclusion : Since p > 0.05(LOS) , we fail to reject null hypothesis and conclude that there is no sufficient
#             data to prove Test & Control Means are same.

# ---------------------------------------- Test Two : Increase PPV by 20% -------------------------------------

# Hypothesis : There is no significant diff b/n Test & Control means : Test & Control Means are same : M1=M2

PPV_All__Paired$Test_Means_Two <- (PPV_All__Paired$Test_Means_one)+((PPV_All__Paired$Test_Means_one/100)*20)
t.test(PPV_All__Paired[,2],PPV_All__Paired[,6],paired=TRUE)

# t= +/- 0.0129, df=4, p=0.9898
# Conclusion : Since p > 0.05(LOS) , we fail to reject null hypothesis and conclude that there is no sufficient
#             data to prove Test & Control Means are same.


# ---------------------------------------- Test Three : Increase PPV by 235% -------------------------------------

# Hypothesis : There is no significant diff b/n Test & Control means :  Test & Control Means are same : M1=M2

PPV_All__Paired$Test_Means_Three <- (PPV_All__Paired$Test_Means_one)+((PPV_All__Paired$Test_Means_one/100)*235)

t.test(PPV_All__Paired[,2],PPV_All__Paired[,7],paired=TRUE)

# t= +/- 2.7401, df=4, p=0.0519
# Conclusion-1: Since p < 0.05(LOS) , we reject null hypothesis and conclude that there is a significant difference
# Test & Control Means.
# Conclusion-2: Hence, if Bounce rate reduced by 40%(minimum) then these pairs are evident enough to consider 
#               as pairs.

# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
