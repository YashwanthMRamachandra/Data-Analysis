########################################################################################################################################################################################################################################################################################################################################################################################################################################################################################
########################################################################################################################################################################################################################################################################################################################################################################################################################################################################################

# */ Imoprt the data */ 
# Import new data file  ##

system.time(  
  read.table("C:/Yashwanth/OC & C/new.txt", header = TRUE, sep = "\t", 
           quote = "\"",dec = ".",fill=TRUE,comment.char="",as.is=TRUE)
)
system.time(  
OCC.Input <- read.table("C:/Yashwanth/OC & C/new.txt",header = TRUE, sep = "|",
                                   quote = "\"",dec = ".",fill=TRUE,comment.char="",as.is=TRUE)
)

# Convert character vector to numeric vector  ##

Sample$PREV_pieces <- as.numeric(gsub(",","",Sample$PREV_pieces))

########################################################################################################################################################################################################################################################################################################################################################################################################################################################################################
########################################################################################################################################################################################################################################################################################################################################################################################################################################################################################

# Import uplift data file  ##

system.time(  
  OCC.Uplift_Data <- read.table("C:/Yashwanth/OC & C/Uplift Data - detail CTG, ABC, DEL CZ.txt",header = TRUE, sep = "|",
                          quote = "\"",dec = ".",fill=TRUE,comment.char="",as.is=TRUE)[-c(5,63),]
)

OCC.Uplift_Data <- data.frame(OCC.Uplift_Data,row.names=NULL)
OCC.Uplift_Data_Sample <- OCC.Uplift_Data[1:10000,]
OCC.Uplift_Filter_Sample <- data.frame(subset(OCC.Uplift_Data_Sample, start_week>=201327 & 
                                                start_week<=201426 |!catman_buy_domain_desc =="BF17 CIGARETTES",
                            names(OCC.Uplift_Data_Sample)),row.names=NULL)



OCC.Uplift_Filter <- data.frame(subset(OCC.Uplift_Data,start_week>=201327 & start_week<=201426,
                                       names(OCC.Uplift_Data)),row.names=NULL)

OCC.Uplift_Filter <- data.frame(subset(OCC.Uplift_Filter,!catman_buy_domain_desc =="BF17 CIGARETTES",
                                       names(OCC.Uplift_Filter)),row.names=NULL)


########################################################################################################################################################################################################################################################################################################################################################################################################################################################################################
########################################################################################################################################################################################################################################################################################################################################################################################################################################################################################
########################################################################################################################################################################################################################################################################################################################################################################################################################################################################################
########################################################################################################################################################################################################################################################################################################################################################################################################################################################################################
########################################################################################################################################################################################################################################################################################################################################################################################################################################################################################
########################################################################################################################################################################################################################################################################################################################################################################################################################################################################################


#--------------------------------------------------------------------
# OCC: R code for Analysis 24 | Category wise Promotion Lift calculation
# Pseudo code for validating logic
#--------------------------------------------------------------------

# Initialize environment
ls()
rm(list=ls())
gc()
.libPaths(c("C:/Apps/R/x64/library",.libPaths()))

#------------Metro C&C - Czech Republic source data read-------------

setwd("C:/Projects/Metro/data/Uplift Data - detail CTG, ABC, DEL CZ")
df <- read.table("C:/Yashwanth/OC & C/Input_file/Uplift Data - detail CTG, ABC, DEL CZ.txt", 
                 header=TRUE,sep="|",quote = "",dec = ".",fill=TRUE,comment.char="",as.is=TRUE)
df_temp <- df

#------------Metro C&C - Master data read for mappings----------------

setwd("C:/Projects/Metro/data/csv")
cat_mast_df <- read.csv("C:/Yashwanth/OC & C/Input_file/Category_Master.csv", header=TRUE)
main_leaflet_df <- read.csv("C:/Yashwanth/OC & C/Input_file/Main_Leaflet_Master_Ugam.csv", header=TRUE)

#---------------------------------------------------------------
# First level aggregation of the base data
#---------------------------------------------------------------
df <- sqldf("SELECT   start_week  ,
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
          AVG(	prev_duration	) as prev_duration,
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
          article	")


#---------------------------------------------------------------
# Set all additional columns / filter conditions
#---------------------------------------------------------------

info_list <- sqldf("SELECT 
                 CASE
                 WHEN pdf.prev_days = 0 THEN 'no prev-period available'
                 WHEN (pdf.prev_days <> 0 AND (pdf.last_promotion_before >= 0 AND pdf.last_promotion_before <= 14)) THEN 'overlap with prev promo'
                 WHEN (pdf.prev_days <> 0 AND pdf.last_promotion_before < 1) THEN 'two promotions parallel'
                 ELSE 'ok'
                 END as info
                 FROM df as pdf ")

df <- cbind(df, info_list)

occ_filter_list <- sqldf("SELECT CASE
                       WHEN (lower(pdf.info) = 'ok' AND upper(trim(catman_buy_domain_desc)) <> 'BF17 CIGARETTES') THEN 'ok'
                       ELSE 'dont use'
                       END as occ_filter
                       FROM df as pdf ")

df <- cbind(df, occ_filter_list)

unique(df$catman_buy_domain_desc)

cat_ident_list <-	sqldf("SELECT
                      cat_mast_df.Ca_Ident, cat_mast_df.OCC_cat_group
                      FROM df as prod_df, cat_mast_df 
                      WHERE upper(trim(cat_mast_df.catman_buy_domain_desc)) = upper(trim(prod_df.catman_buy_domain_desc))")

df <- cbind(df, cat_ident_list)
df$Ca_Ident <- substr(df$catman_buy_domain_desc,"0","4")
  
main_leaflet_list <- sqldf("SELECT
                         main_leaflet_df.Main_Leaflet
                         FROM df as prod_df 
                         LEFT JOIN main_leaflet_df 
                         ON upper(trim(main_leaflet_df.prom_desc)) = upper(trim(prod_df.prom_desc))")

df <- cbind(df, main_leaflet_list)

write.csv(df, "MetroCC_Czech_CatUplift_Req24.csv")

gc()

#-----------------------------------------------------------------------------
# Calculate Metrics
#-----------------------------------------------------------------------------


# -----------------------------------------------------------------------
# Part 1 of Category Uplift report
# -----------------------------------------------------------------------

cat_uplift_df <- sqldf("SELECT catman_buy_domain_desc, Ca_Ident, OCC_cat_group,
                     sum(PROM_sales) as PROM_sales, sum(PROM_profit) as PROM_profit, sum(PROM_sales)/sum(PROM_profit) as PROM_GP_per,
                     sum(PROM_LI_BAS) as PROM_LI_BAS, sum(PROM_LI_SAP) as PROM_LI_SAP
                     FROM df
                     WHERE (start_week >= 201327 AND start_week <= 201426)
                     GROUP BY catman_buy_domain_desc, Ca_Ident, OCC_cat_group")

cat_uplift_df$TI_per <- ((cat_uplift_df$PROM_profit + cat_uplift_df$PROM_LI_BAS + cat_uplift_df$PROM_LI_SAP)/ cat_uplift_df$PROM_sales)*100

write.csv(cat_uplift_df, "MetroCC_Czech_CatUplift_1.csv")

cat_uplift_df1_2 <- sql("SELECT cat_uplift_df.catman_buy_domain_desc,
                        cat_uplift_df.Cat_Ident,
                        cat_uplift_df.OCC_cat_group,
                        sum(df.PROM_sales) as MAIN_LEAFLET_SALES 
                        FROM df, cat_uplift_df
                        WHERE 
                        (df.start_week >= 201327 AND df.start_week <= 201426)
                        AND df.Main_Leaflet = 'Yes'	
                        AND df.catman_buy_domain_desc = cat_uplift_df.catman_buy_domain_desc
                        AND df.Cat_Ident = cat_uplift_df.Cat_Ident
                        AND df.OCC_cat_group = cat_uplift_df.OCC_cat_group
                        GROUP BY
                        cat_uplift_df.catman_buy_domain_desc,
                        cat_uplift_df.Cat_Ident,
                        cat_uplift_df.OCC_cat_group")

write.csv(cat_uplift_df1_2, "MetroCC_Czech_CatUplift_1_2.csv")

cat_uplift_df1_3 <- sql("SELECT cat_uplift_df.catman_buy_domain_desc,
                        cat_uplift_df.Cat_Ident,
                        cat_uplift_df.OCC_cat_group,
                        sum(df.PROM_sales) as OCC_FILTER_MAIN_LEAFLET_SALES 
                        FROM df, cat_uplift_df
                        WHERE 
                        (df.start_week >= 201327 AND df.start_week <= 201426)
                        AND df.Main_Leaflet = 'Yes'	
                        AND df.occ_filter = 'ok'
                        AND df.catman_buy_domain_desc = cat_uplift_df.catman_buy_domain_desc
                        AND df.Cat_Ident = cat_uplift_df.Cat_Ident
                        AND df.OCC_cat_group = cat_uplift_df.OCC_cat_group
                        GROUP BY
                        cat_uplift_df.catman_buy_domain_desc,
                        cat_uplift_df.Cat_Ident,
                        cat_uplift_df.OCC_cat_group")

write.csv(cat_uplift_df1_3, "MetroCC_Czech_CatUplift_1_3.csv")

# -----------------------------------------------------------------------
# Part 2 of Category Uplift report
# -----------------------------------------------------------------------

cat_uplift_df2 <- 
  sql("SELECT 
      catman_buy_domain_desc, Cat_Ident, OCC_cat_group,
      sum(PROM_sales) as PROM_sales, 
      sum(prom_duration) as prom_duration,
      sum(PREV_sales) as PREV_sales, 
      sum(prev_duration) as prev_duration,
      sum(PROM_profit + PROM_LI_BAS + PROM_LI_SAP) as PROM_TI,
      sum(PREV_profit + PREV_LI_BAS + PREV_LI_SAP) as PREV_TI,
      sum(PROM_buy_custs) as PROM_buy_custs, sum(PREV_buy_custs) as PREV_buy_custs
      FROM df
      WHERE (start_week >= 201327 AND start_week <= 201426)
      AND lower(occ_filter) = 'ok' 
      AND lower(Main_Leaflet) = 'yes'
      GROUP BY
      catman_buy_domain_desc, Cat_Ident, OCC_cat_group")

write.csv(cat_uplift_df2, "MetroCC_Czech_CatUplift_2.csv")

#---------------------------------------------------------------------
# Assemble all the above csv outputsections in excel and calculate the rest of the metrics
#---------------------------------------------------------------------











########################################################################################################################################################################################################################################################################################################################################################################################################################################################################################
########################################################################################################################################################################################################################################################################################################################################################################################################################################################################################









########################################################################################################################################################################################################################################################################################################################################################################################################################################################################################
########################################################################################################################################################################################################################################################################################################################################################################################################################################################################################
########################################################################################################################################################################################################################################################################################################################################################################################################################################################################################
########################################################################################################################################################################################################################################################################################################################################################################################################################################################################################
########################################################################################################################################################################################################################################################################################################################################################################################################################################################################################
########################################################################################################################################################################################################################################################################################################################################################################################################################################################################################
