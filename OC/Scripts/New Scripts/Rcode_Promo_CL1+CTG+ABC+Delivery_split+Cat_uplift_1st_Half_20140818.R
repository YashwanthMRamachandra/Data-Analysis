#--------------------------------------------------------------------
#Further Promo uplift analyses-Today/tomorrow, 12/13 August 
OCC: R code for Analysis 24 |Uplift analyses CTG
# According to gerrit : CL1 need to be included with the CTG cut 
#CL1: HoReCa, Trader, SCO
#CTG: Asian, Vietnamese, Fast Food, Canteens etc 
#--------------------------------------------------------------------

# Initialize environment
ls()
rm(list=ls())
gc()
.libPaths(c("C:/Apps/R/x64/library",.libPaths()))

library("plyr")
library("RSQLite")
library("sqldf")

library(stats);
library(graphics);
library(grDevices);
library(utils);
library(splines);
library(methods);
library(bdsmatrix);   #install.packages("bdsmatrix", dep = TRUE)
library(Formula);

#Data.frame <- read.table("new.txt", header = TRUE, sep = "\t", quote = "\"",dec = ".",fill=TRUE,comment.char="",as.is=TRUE)

#------------Metro C&C - Czech Republic source data read/ Data will be provided by KP-------------

#setwd("C:/Prabu/Projects/Metro/data/Uplift Data - detail CTG, ABC, DEL CZ")
setwd("/data/Analytics/occ_deu/data/Uplift Data - detail CTG, ABC, DEL CZ")
fileConn<-file("Uplift Data - detail CTG, ABC, DEL CZ.txt")
#test.df <- read.table(fileConn, header = TRUE, sep = "|", quote = "\"", dec = "," ,fill=TRUE, comment.char="", as.is=TRUE)
df <- read.table("C:/Yashwanth/OC & C/Input_file/Uplift Data - detail CTG, ABC, DEL CZ.txt", 
                 header = TRUE, sep = "|", quote = "", dec = "," ,fill=TRUE, comment.char="", as.is=TRUE)
#writeLines(text, fileConn)
close(fileConn)
rm(fileConn)
gc()

#Confirm control number
sum(df$PROM_sales)

#------------------------------------------------------------

df <- final.df
df$CY_ABC[df$CY_ABC==""] <- "Others"
df$CY_ABC[df$CY_ABC==" "] <- "Others"

#-----------------------------------------
#Resolve data conversion issues with below 4 columns////Need to examine later -confirm with KP///////
#-----------------------------------------

head(df$PROM_LI_BAS)
head(as.numeric(gsub(",",".", df$PROM_LI_BAS)))
sum(is.na(as.numeric(gsub(",",".", df$PROM_LI_BAS))))

df$PROM_LI_BAS[is.na(as.numeric(gsub(",",".", df$PROM_LI_BAS)))]
View(df[is.na(as.numeric(gsub(",",".", df$PROM_LI_BAS))),])

sum(is.na(as.numeric(gsub(",",".", df$PROM_LI_BAS))))
sum(is.na(as.numeric(gsub(",",".", df$PROM_LI_SAP))))

if (class(df$PROM_LI_BAS) == "character")
{
  df$PROM_LI_BAS <- as.numeric(gsub(",",".", df$PROM_LI_BAS))
}

if (class(df$PROM_LI_SAP) == "character")
{
  df$PROM_LI_SAP <- as.numeric(gsub(",",".", df$PROM_LI_SAP))
}

sum(is.na(as.numeric(gsub(",",".", df$PREV_LI_BAS))))
sum(is.na(as.numeric(gsub(",",".", df$PREV_LI_SAP))))

if (class(df$PREV_LI_BAS) == "character")
{
  df$PREV_LI_BAS <- as.numeric(gsub(",",".", df$PREV_LI_BAS))
}

if (class(df$PREV_LI_SAP) == "character")
{
  df$PREV_LI_SAP <- as.numeric(gsub(",",".", df$PREV_LI_SAP))
}
if (class(df$AFTER_avg_stores) == "character")
{
  df$AFTER_avg_stores <- as.numeric(gsub(",",".", df$AFTER_avg_stores))
}
if (class(df$AFTER_buy_custs) == "character")
{
  df$AFTER_buy_custs <- as.numeric(gsub(",",".", df$AFTER_buy_custs))
}
if (class(df$AFTER_visits) == "character")
{
  df$AFTER_visits <- as.numeric(gsub(",",".", df$AFTER_visits))
}
if (class(df$AFTER_pieces) == "character")
{
  df$AFTER_pieces <- as.numeric(gsub(",",".", df$AFTER_pieces))
}
if (class(df$AFTER_sales) == "character")
{
  df$AFTER_sales <- as.numeric(gsub(",",".", df$AFTER_sales))
}
if (class(df$AFTER_profit) == "character")
{
  df$AFTER_profit <- as.numeric(gsub(",",".", df$AFTER_profit))
}
if (class(df$AFTER_BP) == "character")
{
  df$AFTER_BP <- as.numeric(gsub(",",".", df$AFTER_BP))
}
if (class(df$after_duration) == "character")
{
  df$after_duration <- as.numeric(gsub(",",".", df$after_duration))
}
if (class(df$AFTER_LI_BAS) == "character")
{
  df$AFTER_LI_BAS <- as.numeric(gsub(",",".", df$AFTER_LI_BAS))
}
if (class(df$AFTER_LI_SAP) == "character")
{
  df$AFTER_LI_SAP <- as.numeric(gsub(",",".", df$AFTER_LI_SAP))
}

# ---------------------------------------------------------------------------
# Data aggregation for Customer Cluster Uplift
# ---------------------------------------------------------------------------
df<- sqldf("SELECT   start_week  ,
           prom_desc  ,
           PROM_START_DATE  ,
           PROM_END_DATE	,
           PROM_ID	,
           ADVERT_ID	,
           fnf_cd	,
           division	,
           catman_buy_domain_desc	,
           main_category	,
           category	,
           sub_category	,
           PRIVATE_LABEL	,
           art_no	,
           article	,
           CL1,CTG,CY_ABC,Delivery_Flag,
           SUM(	PREV_avg_stores	) as PREV_avg_stores,
           SUM(  PREV_buy_custs ) as PREV_buy_custs,
           SUM(	PREV_visits	) as PREV_visits,
           SUM(	PREV_pieces	) as PREV_pieces,
           SUM(	PREV_sales	) as PREV_sales,
           SUM(	PREV_profit	) as PREV_profit,
           SUM(	PREV_BP	) as PREV_BP,
           SUM(	PROM_buy_custs	) as PROM_buy_custs,
           SUM(	PROM_visits	) as PROM_visits,
           SUM(	PROM_pieces	) as PROM_pieces,
           SUM(	PROM_sales	) as PROM_sales,
           SUM(	PROM_profit	) as PROM_profit,
           SUM(	AFTER_buy_custs	) as AFTER_buy_custs,
           SUM(	AFTER_visits	) as AFTER_visits,
           SUM(	AFTER_pieces	) as AFTER_pieces,
           SUM(	AFTER_sales	) as AFTER_sales,
           SUM(	AFTER_profit	) as AFTER_profit,
           SUM(	AFTER_BP	) as AFTER_BP,
           AVG(  PREV_DAYS ) as prev_days,
           AVG(  prev_duration	) as prev_duration,
           AVG(	prom_duration	) as prom_duration,
           AVG(	after_duration	) as after_duration,
           AVG(	last_promotion_before	) as last_promotion_before,
           AVG(	no_of_promotions	) as no_of_promotions,
           SUM(	PROM_LI_BAS	) as PROM_LI_BAS,
           SUM(	PROM_LI_SAP	) as PROM_LI_SAP,
           SUM(	PREV_LI_BAS	) as PREV_LI_BAS,
           SUM(	PREV_LI_SAP	) as PREV_LI_SAP,
           SUM(	AFTER_LI_BAS	) as AFTER_LI_BAS,
           SUM(	AFTER_LI_SAP	) as AFTER_LI_SAP
           FROM	df
           GROUP BY 	start_week	,
           prom_desc	,
           PROM_START_DATE	,
           PROM_END_DATE	,
           PROM_ID	,
           ADVERT_ID	,
           fnf_cd	,
           division	,
           catman_buy_domain_desc	,
           main_category	,
           category	,
           sub_category	,
           PRIVATE_LABEL	,
           art_no	,
           article,
           CL1,
           CTG,
           CY_ABC,
           Delivery_Flag")

nrow(df)


#------------Metro C&C - Master data read----------------------------
#setwd("C:/Prabu/Projects/Metro/data/csv")
setwd("/data/Analytics/occ_deu/data")
cat_mast_df <- read.csv("C:/Yashwanth/OC & C/Input_file/Category_Master_20140806.csv", header=TRUE)
main_leaflet_df <- read.csv("C:/Yashwanth/OC & C/Input_file/Main_Leaflet_Master_20140807.csv", header=TRUE)

#---------------------------------------------------------------
names(df)

nrow(df)
View(df)
names(df)

#info_list <- sqldf("SELECT 
#            CASE
#  						WHEN pdf.prev_days = 0 THEN 'no prev-period available'
#							WHEN (pdf.prev_days <> 0 AND (pdf.last_promotion_before >= 0 AND pdf.last_promotion_before <= 14)) THEN 'overlap with prev promo'
#							WHEN (pdf.prev_days <> 0 AND pdf.last_promotion_before < 1) THEN 'two promotions parallel'
#							ELSE 'ok'
#						END as info
#						FROM df as pdf ")

# Logic below implemented based on feedback from Gerrit on 07-Aug-2014
# WHEN (pdf.prev_days <> 0 AND (pdf.last_promotion_before >= 1 AND pdf.last_promotion_before <= 14)) THEN 'overlap with prev promo'
# Also, I?m asking myself what happens during the data aggregation when [last_promotion_before] is empty before aggregation (no promotion found). 
# Will the column remain empty as we can?t build the average over multiple empty columns? Or will it show 0? If the last thing is the case, 
# we might have to change the code further from
# WHEN l.prev_days <> 0 AND x.last_promotion_before < 1 THEN 'two promotions parallel'
# To
# WHEN l.prev_days <> 0 AND x.last_promotion_before < 0 THEN 'two promotions parallel
# 

info_list <- sqldf("SELECT 
                   CASE
                   WHEN pdf.prev_days = 0 THEN 'no prev-period available'
                   WHEN (pdf.prev_days <> 0 AND (pdf.last_promotion_before >= 1 AND pdf.last_promotion_before <= 14)) THEN 'overlap with prev promo'
                   WHEN (pdf.prev_days <> 0 AND pdf.last_promotion_before < 0) THEN 'two promotions parallel'
                   ELSE 'ok'
                   END as info
                   FROM df as pdf ")

nrow(info_list)
names(info_list)
summary(factor(info_list$info))

length(names(df))
df$info=NULL
df <- cbind(df, info_list)
length(names(df))
names(df)
summary(factor(df$info))
rm(info_list)
gc()

unique(df$catman_buy_domain_desc)
occ_filter_list <- sqldf("SELECT CASE
                         WHEN (lower(pdf.info) = 'ok' AND upper(trim(catman_buy_domain_desc)) <> 'BF17 CIGARETTES') THEN 'ok'
                         ELSE 'dont use'
                         END as occ_filter
                         FROM df as pdf ")

nrow(occ_filter_list)
names(occ_filter_list)
summary(factor(occ_filter_list$occ_filter))

length(names(df))
df$occ_filter=NULL
df <- cbind(df, occ_filter_list)
length(names(df))
names(df)
unique(df$occ_filter)
summary(factor(df$occ_filter))
rm(occ_filter_list)
gc()

#df$occ_filter = NULL

unique(df$catman_buy_domain_desc)
unique(cat_mast_df$catman_buy_domain_desc)

cat_ident_list <-	sqldf("SELECT
                        cat_mast_df.Cat_Ident, cat_mast_df.OCC_cat_group
                        FROM df as prod_df, cat_mast_df 
                        WHERE upper(trim(cat_mast_df.catman_buy_domain_desc)) = upper(trim(prod_df.catman_buy_domain_desc))")

nrow(df)
nrow(cat_ident_list)
names(cat_ident_list)
summary(factor(cat_ident_list$Cat_Ident))
summary(factor(cat_ident_list$OCC_cat_group))

length(names(df))
df$cat_ident_list=NULL
df <- cbind(df, cat_ident_list)
length(names(df))
names(df)

unique(df$Cat_Ident)
summary(factor(df$Cat_Ident))
unique(df$OCC_cat_group)
summary(factor(df$OCC_cat_group))

rm(cat_ident_list)
gc()

summary(factor(df[is.na(df$OCC_cat_group),]$catman_buy_domain_desc))

names(main_leaflet_df)
unique(main_leaflet_df$prom_desc)
summary(factor(main_leaflet_df$Main_Leaflet))

main_leaflet_list <- sqldf("SELECT
                           main_leaflet_df.Main_Leaflet
                           FROM df as prod_df 
                           LEFT JOIN main_leaflet_df 
                           ON upper(trim(main_leaflet_df.prom_desc)) = upper(trim(prod_df.prom_desc))")

#a <- data.frame(unique(df$prom_desc))
#b <- data.frame(unique(main_leaflet_df$prom_desc))
#names(a)[1] <- "prom_desc"
#names(b)[1] <- "prom_desc"

#sqldf("select distinct prom_desc from a 
#      where prom_desc not in (select distinct prom_desc from b)")

summary(factor(main_leaflet_list$Main_Leaflet))
length(names(df))
gc()

df <- cbind(df, main_leaflet_list)
length(names(df))
names(df)
summary(factor(df$Main_Leaflet))
rm(main_leaflet_list)

write.csv(df, "C:/Yashwanth/OC & C/Output_file/MetroCC_Czech_CatUplift_Req24.csv")

gc()
# ---------------Memory management-------------- #
# setwd("C:/Prabu/Projects/Metro/data/csv")
# setwd("/data/Analytics/occ_deu/data")
# save.image("Metro_CC_Czech_20140806.RData")
# save(df, file="df.RData")
# save(main_leaflet_list, file="main_leaflet_list.RData")
# save(cat_mast_df, file="cat_mast_df.RData")
# save(main_leaflet_df, file="main_leaflet_df.RData")
# load("Metro_CC_Czech_20140806(Second_release).RData")
# load("df.RData")
# load("main_leaflet_list.RData")
# load("cat_mast_df.RData")
# load("main_leaflet_df.RData")
# ----------------------------------------------- #

names(df)

#-----------------------------------------------------------------------------
# Calculate Metrics
#-----------------------------------------------------------------------------
gc()
ls()

getwd()
setwd("C:/Prabu/Projects/Metro/data/csv")
load("cat_mast_df.RData")
load("main_leaflet_df.RData")

names(df)
df$category=NULL
df$sub_category=NULL
df$no_of_promotions=NULL
df$PROM_START_DATE=NULL
df$PROM_END_DATE=NULL
df$PREV_BP=NULL

# ---------------------------------------------------------------------------
# Customer Cluster by Product Category Uplift report
# ---------------------------------------------------------------------------

#CTG("Fast Food","Vietnamese", "Canteen"....list to be updated
#   CL1(HoReCa,Trader,SCO)

df11 <- sqldf("select df.CL1,df.CTG,df.CY_ABC,df.Delivery_Flag,df.catman_buy_domain_desc, 
             df.Cat_Ident, df.OCC_cat_group,
             sum(df.PROM_sales) as PROM_sales,
             sum(df.PROM_profit) as PROM_profit,
             sum(df.PROM_LI_BAS) as PROM_LI_BAS,
             sum(df.PROM_LI_SAP) as PROM_LI_SAP,           
             sum(df.PROM_buy_custs) as PROM_buy_custs,
             sum(df.prom_duration) as prom_duration,
             sum(df.PREV_sales) as PREV_sales,
             sum(df.PREV_profit) as PREV_profit,
             sum(df.PREV_LI_BAS) as PREV_LI_BAS,
             sum(df.PREV_LI_SAP) as PREV_LI_SAP,           
             sum(df.PREV_buy_custs) as PREV_buy_custs,
             sum(df.PREV_duration) as PREV_duration,
             sum(df.AFTER_sales) as AFTER_sales,
             sum(df.AFTER_profit) as AFTER_profit,
             sum(df.AFTER_LI_BAS) as AFTER_LI_BAS,
             sum(df.AFTER_LI_SAP) as AFTER_LI_SAP,
             sum(df.AFTER_buy_custs) as AFTER_buy_custs,
             sum(df.after_duration) as after_duration
             from df where 
             (df.start_week >= 201405 AND df.start_week <= 201426)
             AND df.Main_Leaflet = 'Yes'  
             AND df.occ_filter = 'ok'
             GROUP BY df.CL1,df.CTG,df.CY_ABC,df.Delivery_Flag,df.catman_buy_domain_desc, 
             df.Cat_Ident, df.OCC_cat_group")

write.csv(df11,"C:/Yashwanth/OC & C/Reports/Promo_CL1+CTG+ABC+Delivery_split+Cat_1st_Half_uplift_20140818/Stage11.csv")

df22 <- sqldf("select df.CL1,df.CTG,df.CY_ABC,df.Delivery_Flag,
             sum(df.PROM_sales) as PROM_sales,
             sum(df.PROM_profit) as PROM_profit,
             sum(df.PROM_LI_BAS) as PROM_LI_BAS,
             sum(df.PROM_LI_SAP) as PROM_LI_SAP,           
             sum(df.PROM_buy_custs) as PROM_buy_custs,
             sum(df.prom_duration) as prom_duration,
             sum(df.PREV_sales) as PREV_sales,
             sum(df.PREV_profit) as PREV_profit,
             sum(df.PREV_LI_BAS) as PREV_LI_BAS,
             sum(df.PREV_LI_SAP) as PREV_LI_SAP,           
             sum(df.PREV_buy_custs) as PREV_buy_custs,
             sum(df.PREV_duration) as PREV_duration,
             sum(df.AFTER_sales) as AFTER_sales,
             sum(df.AFTER_profit) as AFTER_profit,
             sum(df.AFTER_LI_BAS) as AFTER_LI_BAS,
             sum(df.AFTER_LI_SAP) as AFTER_LI_SAP,
             sum(df.AFTER_buy_custs) as AFTER_buy_custs,
             sum(df.after_duration) as after_duration
             from df11 as df
             GROUP BY df.CL1,df.CTG,df.CY_ABC,df.Delivery_Flag")

write.csv(df22,"C:/Yashwanth/OC & C/Reports/Promo_CL1+CTG+ABC+Delivery_split+Cat_1st_Half_uplift_20140818/Stage22.csv")

df33 <- sqldf("select df.CL1,df.CTG,df.CY_ABC,
             sum(df.PROM_sales) as PROM_sales,
             sum(df.PROM_profit) as PROM_profit,
             sum(df.PROM_LI_BAS) as PROM_LI_BAS,
             sum(df.PROM_LI_SAP) as PROM_LI_SAP,           
             sum(df.PROM_buy_custs) as PROM_buy_custs,
             sum(df.prom_duration) as prom_duration,
             sum(df.PREV_sales) as PREV_sales,
             sum(df.PREV_profit) as PREV_profit,
             sum(df.PREV_LI_BAS) as PREV_LI_BAS,
             sum(df.PREV_LI_SAP) as PREV_LI_SAP,           
             sum(df.PREV_buy_custs) as PREV_buy_custs,
             sum(df.PREV_duration) as PREV_duration,
             sum(df.AFTER_sales) as AFTER_sales,
             sum(df.AFTER_profit) as AFTER_profit,
             sum(df.AFTER_LI_BAS) as AFTER_LI_BAS,
             sum(df.AFTER_LI_SAP) as AFTER_LI_SAP,
             sum(df.AFTER_buy_custs) as AFTER_buy_custs,
             sum(df.after_duration) as after_duration
             from df22 as df
             GROUP BY df.CL1,df.CTG,df.CY_ABC")

write.csv(df33,"C:/Yashwanth/OC & C/Reports/Promo_CL1+CTG+ABC+Delivery_split+Cat_1st_Half_uplift_20140818/Stage33.csv")

df44 <- sqldf("select df.CL1,df.CTG,
             sum(df.PROM_sales) as PROM_sales,
             sum(df.PROM_profit) as PROM_profit,
             sum(df.PROM_LI_BAS) as PROM_LI_BAS,
             sum(df.PROM_LI_SAP) as PROM_LI_SAP,           
             sum(df.PROM_buy_custs) as PROM_buy_custs,
             sum(df.prom_duration) as prom_duration,
             sum(df.PREV_sales) as PREV_sales,
             sum(df.PREV_profit) as PREV_profit,
             sum(df.PREV_LI_BAS) as PREV_LI_BAS,
             sum(df.PREV_LI_SAP) as PREV_LI_SAP,           
             sum(df.PREV_buy_custs) as PREV_buy_custs,
             sum(df.PREV_duration) as PREV_duration,
             sum(df.AFTER_sales) as AFTER_sales,
             sum(df.AFTER_profit) as AFTER_profit,
             sum(df.AFTER_LI_BAS) as AFTER_LI_BAS,
             sum(df.AFTER_LI_SAP) as AFTER_LI_SAP,
             sum(df.AFTER_buy_custs) as AFTER_buy_custs,
             sum(df.after_duration) as after_duration
             from df33 as df
             GROUP BY df.CL1,df.CTG")    

write.csv(df44,"C:/Yashwanth/OC & C/Reports/Promo_CL1+CTG+ABC+Delivery_split+Cat_1st_Half_uplift_20140818/Stage44.csv")

df55 <- sqldf("select df.CL1,
             sum(df.PROM_sales) as PROM_sales,
             sum(df.PROM_profit) as PROM_profit,
             sum(df.PROM_LI_BAS) as PROM_LI_BAS,
             sum(df.PROM_LI_SAP) as PROM_LI_SAP,           
             sum(df.PROM_buy_custs) as PROM_buy_custs,
             avg(df.prom_duration) as prom_duration,
             sum(df.PREV_sales) as PREV_sales,
             sum(df.PREV_profit) as PREV_profit,
             sum(df.PREV_LI_BAS) as PREV_LI_BAS,
             sum(df.PREV_LI_SAP) as PREV_LI_SAP,           
             sum(df.PREV_buy_custs) as PREV_buy_custs,
             avg(df.PREV_duration) as PREV_duration,
             sum(df.AFTER_sales) as AFTER_sales,
             sum(df.AFTER_profit) as AFTER_profit,
             sum(df.AFTER_LI_BAS) as AFTER_LI_BAS,
             sum(df.AFTER_LI_SAP) as AFTER_LI_SAP,
             sum(df.AFTER_buy_custs) as AFTER_buy_custs,
             avg(df.after_duration) as after_duration
             from df44 as df
             GROUP BY df.CL1")    

write.csv(df55,"C:/Yashwanth/OC & C/Reports/Promo_CL1+CTG+ABC+Delivery_split+Cat_1st_Half_uplift_20140818/Stage55.csv")

# ---------------------------------------------------------------------------

#----------------------------------------------------------------------------