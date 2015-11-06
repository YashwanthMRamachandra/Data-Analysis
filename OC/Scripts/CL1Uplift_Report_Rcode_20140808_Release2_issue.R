#--------------------------------------------------------------------
# OCC: R code for Analysis 24 | Customer Cluster (CL1) Promotion Lift calculation
# CL1: HoReCa, Trader, SCO
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

#------------Metro C&C - Czech Republic source data read-------------

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

#-----------------------------------------
#Resolve data conversion issues with below 4 columns
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

# ---------------------------------------------------------------------------
# Get unique combinations of Customer Clusters (CL1 = HoReCa, Trader, SCO) + 
# Promo + Start week + Article from the base data set for the time period to be analyzed
# Time period
#ccl1_prom_art_master <- sqldf("SELECT distinct CL1, start_week  ,
#                              prom_desc, art_no
#                          FROM df
#                          WHERE (start_week >= 201327 AND start_week <= 201426)")

#head(ccl1_prom_art_master)
#nrow(ccl1_prom_art_master)

# ---------------------------------------------------------------------------
# Data aggregation for Customer Cluster Uplift
# ---------------------------------------------------------------------------
df_temp_1<- sqldf("SELECT   start_week  ,
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
                                 article	,
                                 CL1,
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
                                 FROM	df_temp_1	
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
                                 CL1")

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
#							WHEN pdf.prev_days = 0 THEN 'no prev-period available'
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

#
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
# ---------------------------------------------------------------------------
# Update unique combinations of Customer Clusters (CL1 = HoReCa, Trader, SCO) + 
# Promo + Start week + Article from the base data set for the time period to be analyzed
# Time period
# BY adding the Flags info, 

ccl1_prom_art_master <- sqldf("SELECT a.CL1, a.start_week,
            							a.prom_desc, a.art_no,
					          			b.info, b.occ_filter,
                          b.Cat_Ident, b.OCC_cat_group,
                          b.Main_Leaflet
                      FROM ccl1_prom_art_master as a, df as b
                              WHERE a.start_week  = b.start_week
                              AND a.prom_desc = b.prom_desc
                              AND a.art_no = b.art_no")

head(ccl1_prom_art_master)
nrow(ccl1_prom_art_master)

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

unique(ccl1_prom_art_master$CL1)

"HORECA","TRADER", "SCO"

v_cl1 <- data.frame(cl1="SCO")

cl1_df <- sqldf("SELECT df.catman_buy_domain_desc, df.Cat_Ident, df.OCC_cat_group,
      sum(df.PROM_sales) as PROM_sales, 
      sum(df.prom_duration) as prom_duration,
                sum(df.PREV_sales) as PREV_sales, 
                sum(df.prev_duration) as prev_duration,
                sum(df.PROM_profit) as PROM_profit, 
                (sum(df.PROM_profit)/sum(df.PROM_sales))*100 as PROM_GP_per,
                sum(df.PROM_LI_BAS) as PROM_LI_BAS,
                sum(df.PROM_LI_SAP) as PROM_LI_SAP,
                sum(df.PREV_profit) as PREV_profit,
                sum(df.PREV_LI_BAS) as PREV_LI_BAS,
                sum(df.PREV_LI_SAP) as PREV_LI_SAP,
                sum(df.PROM_profit + df.PROM_LI_BAS + df.PROM_LI_SAP) as PROM_TI,
                sum(df.PREV_profit + df.PREV_LI_BAS + df.PREV_LI_SAP) as PREV_TI,
                sum(df.PROM_buy_custs) as PROM_buy_custs, 
                sum(df.PREV_buy_custs) as PREV_buy_custs
                FROM df, v_cl1
                WHERE upper(df.CL1) = v_cl1.cl1
                AND (df.start_week >= 201327 AND df.start_week <= 201426)
                AND upper(df.CL1) = v_cl1.cl1
                AND df.Main_Leaflet = 'Yes'  
                AND df.occ_filter = 'ok'
                GROUP BY df.catman_buy_domain_desc, df.Cat_Ident, df.OCC_cat_group")

# View(cl1_df)

SCO <- paste("MetroCC_Czech_CL1Uplift_",v_cl1$cl1[1],".csv", sep="")
write.csv(cl1_df, "C:/Yashwanth/OC & C/Output_file/SCO.csv")

# ---------------------------------------------------------------------------

#---------------------------------------------------------------------
# Data validation
summary(factor(df$catman_buy_domain_desc))
unique(df$catman_buy_domain_desc)

View(head(df[df$Cat_Ident=="BF01",], nrows=100))
head(as.numeric(df$PROM_sales))

head(as.numeric(gsub(",","", df$PROM_sales)))

as.numeric(as.character(df[1,]$PROM_sales))
df[2,]$PROM_sales
df[1,]$PROM_sales + df[2,]$PROM_sales


sum(as.numeric(df[df$Cat_Ident=="BF01",]$PROM_sales))
sum(df[df$Cat_Ident=="BF01",]$PROM_sales)

class(df$PROM_sales)
sum(is.na(df$PROM_sales))
sum(df$PROM_sales=="")

head(as.numeric(df$PROM_sales))

sqldf("SELECT sum(PROM_Sales) 
FROM df
WHERE Cat_Ident='BF01'")

sqldf("SELECT sum(as.numeric(PROM_Sales)) 
FROM df
WHERE Cat_Ident='BF01'")

nrow(df)

df$Cat_Ident[is.na(df$Cat_Ident)] <- 0

sum(df$Cat_Ident=='BF01')

sum(is.na(df$Cat_Ident))

sqldf("SELECT sum(PROM_Sales) 
FROM df
WHERE Cat_Ident='BF01'
	AND (start_week >= 201327 AND start_week <= 201426)")

sqldf("SELECT distinct catman_buy_domain_desc, Cat_Ident 
	FROM df
	WHERE (start_week >= 201327 AND start_week <= 201426)")


sqldf("SELECT distinct catman_buy_domain_desc, Cat_Ident 
	FROM df
	WHERE (start_week >= 201327 AND start_week <= 201426)")

test <- sqldf("SELECT distinct prom_desc, start_week, PROM_START_DATE
	FROM df
	WHERE (start_week >= 201327 AND start_week <= 201426)")
#---------------------------------------------------------------------

head(df)
names(df)
df$info=NULL
df$occ_filter=NULL
df$Cat_Ident=NULL
df$OCC_cat_group=NULL
df$Main_Leaflet=NULL
gc()
system.memory()
