########################################################################################################################################################################################################################################################################################################################################################################################################################################################################################
########################################################################################################################################################################################################################################################################################################################################################################################################################################################################################

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------
#                                       Make my Analysis
#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

#--------------------------------------------------------------------------------------------------
#                                 Modify data types of all variables
#--------------------------------------------------------------------------------------------------

df_temp_2 <- df_temp
df_temp_2$prom_type <- substr(df_temp_2$prom_type,start="7",stop="200")
df_temp_2$CTG <- substr(df_temp_2$CTG,start="10",stop="200")
df_temp_2$main_category <- substr(df_temp_2$main_category,start="7",stop="200")
df_temp_2$category <- substr(df_temp_2$category,start="7",stop="200")
df_temp_2$sub_category <- substr(df_temp_2$sub_category,start="7",stop="200")
df_temp_2$PRIVATE_LABEL <- substr(df_temp_2$PRIVATE_LABEL,start="7",stop="200")
df_temp_2$article <- substr(df_temp_2$article,start="13",stop="200")

df_temp_2$PROM_START_DATE <- as.Date(df_temp_2$PROM_START_DATE)
df_temp_2$PROM_END_DATE <- as.Date(df_temp_2$PROM_END_DATE)
df_temp_2$PREV_avg_stores[df_temp_2$PREV_avg_stores=="?Null?"] <- "0"
df_temp_2$PREV_avg_stores <- as.numeric(df_temp_2$PREV_avg_stores)
df_temp_2$PREV_pieces <- as.numeric(gsub(",","",df_temp_2$PREV_pieces))
df_temp_2$PREV_sales <- as.numeric(gsub(",","",df_temp_2$PREV_sales))
df_temp_2$PREV_profit <- as.numeric(gsub(",","",df_temp_2$PREV_profit))
df_temp_2$PREV_BP <- as.numeric(gsub(",","",df_temp_2$PREV_BP))
df_temp_2$last_promotion_before[df_temp_2$last_promotion_before=="?Null?"] <- "0"
df_temp_2$last_promotion_before <- as.numeric(gsub(",","",df_temp_2$last_promotion_before))

df_temp_2$PROM_avg_stores[df_temp_2$PROM_avg_stores=="?Null?"] <- "0"
df_temp_2$PROM_avg_stores <- as.numeric(gsub(",","",df_temp_2$PROM_avg_stores))
df_temp_2$PROM_pieces <- as.numeric(gsub(",","",df_temp_2$PROM_pieces))
df_temp_2$PROM_sales <- as.numeric(gsub(",","",df_temp_2$PROM_sales))
df_temp_2$PROM_profit <- as.numeric(gsub(",","",df_temp_2$PROM_profit))
df_temp_2$AFTER_avg_stores[df_temp_2$AFTER_avg_stores=="?Null?"] <- "0"
df_temp_2$AFTER_avg_stores <- as.numeric(gsub(",","",df_temp_2$AFTER_avg_stores))

df_temp_2$AFTER_pieces <- as.numeric(gsub(",","",df_temp_2$AFTER_pieces))
df_temp_2$AFTER_sales <- as.numeric(gsub(",","",df_temp_2$AFTER_sales))
df_temp_2$AFTER_profit <- as.numeric(gsub(",","",df_temp_2$AFTER_profit))
df_temp_2$AFTER_BP <- as.numeric(gsub(",","",df_temp_2$AFTER_BP))
df_temp_2$insert_timestamp <- as.Date(df_temp_2$insert_timestamp)

df_temp_2$PROM_LI[df_temp_2$PROM_LI=="?Null?"] <- "0"
df_temp_2$PROM_LI <- as.numeric(gsub(",","",df_temp_2$PROM_LI))

df_temp_2$PREV_LI[df_temp_2$PREV_LI=="?Null?"] <- "0"
df_temp_2$PREV_LI <- as.numeric(gsub(",","",df_temp_2$PREV_LI))

df_temp_2$PROM_LI_BAS[df_temp_2$PROM_LI_BAS=="?Null?"] <- "0"
df_temp_2$PROM_LI_BAS <- as.numeric(gsub(",","",df_temp_2$PROM_LI_BAS))

df_temp_2$PROM_LI_SAP[df_temp_2$PROM_LI_SAP=="?Null?"] <- "0"
df_temp_2$PROM_LI_SAP <- as.numeric(gsub(",","",df_temp_2$PROM_LI_SAP))

df_temp_2$PREV_LI_BAS[df_temp_2$PREV_LI_BAS=="?Null?"] <- "0"
df_temp_2$PREV_LI_BAS <- as.numeric(gsub(",","",df_temp_2$PROM_LI_BAS))

df_temp_2$PREV_LI_SAP[df_temp_2$PREV_LI_SAP=="?Null?"] <- "0"
df_temp_2$PREV_LI_SAP <- as.numeric(gsub(",","",df_temp_2$PROM_LI_SAP))

df_temp_2$AFTER_LI_BAS[df_temp_2$AFTER_LI_BAS=="?Null?"] <- "0"
df_temp_2$AFTER_LI_BAS <- as.numeric(gsub(",","",df_temp_2$AFTER_LI_BAS))

df_temp_2$AFTER_LI_SAP[df_temp_2$AFTER_LI_SAP=="?Null?"] <- "0"
df_temp_2$AFTER_LI_SAP <- as.numeric(gsub(",","",df_temp_2$AFTER_LI_SAP))

########################################################################################################################################################################################################################################################################################################################################################################################################################################################################################
########################################################################################################################################################################################################################################################################################################################################################################################################################################################################################
