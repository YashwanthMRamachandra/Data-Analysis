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
#                                       ||  Exploratory Data Analysis    ||
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------

#---------------------------------------------- Histogram -------------------------------------------------------

par(mfrow=c(1,2),bg="grey52",cex=1.3,mar=c(3,3,1,1),ask=T)
for(i in 1:nrow(PAGES_CP_BR_CG)){
  new_char<- paste0(gsub("_"," ",as.character(PAGES_CP_BR_CG[,1]))," distribution")
  new_char<-sapply(new_char,function(x){gsub(pattern = "^.",replacement = toupper(substr(x = x,1,1)),x = x)})  
  hist(as.numeric(PAGES_CP_BR_CG[i,-1]),prob=T,xlab=PAGES_CP_BR_CG[i,1],main=new_char[i],breaks=5,col="lightskyblue")
  curve(dnorm(x,mean=mean(as.numeric(PAGES_CP_BR_CG[i,-1])),sd=sd(as.numeric(PAGES_CP_BR_CG[i,-1]))),add=TRUE)  
  for(j in 1:nrow(PAGES_CP_BR_TG)){
  new_char<- paste0(gsub("_"," ",as.character(PAGES_CP_BR_TG[,1]))," distribution")
  new_char<-sapply(new_char,function(x){gsub(pattern = "^.",replacement = toupper(substr(x = x,1,1)),x = x)})  
  hist(as.numeric(PAGES_CP_BR_TG[j,-1]),prob=T,xlab=PAGES_CP_BR_TG[j,1],main=new_char[j],breaks=5,col="lightskyblue")
  curve(dnorm(x,mean=mean(as.numeric(PAGES_CP_BR_TG[j,-1])),sd=sd(as.numeric(PAGES_CP_BR_TG[j,-1]))),add=TRUE)      
  }
}

dev.off()

par(bg="grey52",cex=1.3,mar=c(3,3,1,1))
#new_char<- paste0(gsub("_"," ",as.character(PAGES_CP_BR_CG[,1]))," distribution")
#new_char<-sapply(new_char,function(x){gsub(pattern = "^.",replacement = toupper(substr(x = x,1,1)),x = x)})  
hist(as.numeric(PAGES_CP_BR_CG[1,-1]),prob=T,xlab=PAGES_CP_BR_CG[1,1],main=new_char[1],breaks=5,col="lightskyblue")
curve(dnorm(x,mean=mean(as.numeric(PAGES_CP_BR_CG[1,-1])),sd=sd(as.numeric(PAGES_CP_BR_CG[1,-1]))),add=TRUE)

dev.off()
#---------------------------------------------- Trend Graph ----------------------------------------------------


plot(as.numeric(PAGES_CP_BR_CG[1,-1]),type="l")

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

GAP_Pages_CP_BR <- data.frame(GAP_Pages_CP_CG[,1:2],GAP_Pages_CP_TG[,1:2],row.names=NULL)
names(GAP_Pages_CP_BR) <- c("Control","Bounce_Rate_CG","Test","Bounce_Rate_TG")

GAP_Pages_CP_ATOP <- data.frame(GAP_Pages_CP_CG[,c(1,3)],GAP_Pages_CP_TG[,c(1,3)],row.names=NULL)
names(GAP_Pages_CP_ATOP) <- c("Control","ATOP_CG","Test","ATOP_TG")

GAP_Pages_CP_Vis <- data.frame(GAP_Pages_CP_CG[,c(1,4)],GAP_Pages_CP_TG[,c(1,4)],row.names=NULL)
names(GAP_Pages_CP_Vis) <- c("Control","Visits_CG","Test","Visits_TG")


# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
#                                       ||    Barplots    ||
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------


ggplot(data=GAP_Pages_CP_BR, aes(x=Control, y=Bounce_Rate_CG)) + 
  geom_bar(stat="identity", fill="grey", colour="red")+
  geom_bar(data=GAP_Pages_CP_BR, aes(x=Test, y=Bounce_Rate_TG),
           stat="identity", fill="grey", colour="blue")

