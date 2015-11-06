#--------------------------------------------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------------------------------------------
#---------------------------------- Import data---------------------------------------------------------

system.time(  
  OCC.Input_Freq <- read.table("C:/Yashwanth/OC & C/Input_file/Uplift Data - detail CTG Bain Adjust Frequency cluster CZ v2.txt",
                header = TRUE, sep = "|", quote = "",dec = ".",fill=TRUE,comment.char="",as.is=TRUE)
)


#--------------------------------------------------------------------------------------------------------------------------------------

df <- data.frame(subset(OCC.Input_Freq,start_week>=201327 & start_week<=201426,names(OCC.Input_Freq)),
            row.names=NULL)
OCC.df_sql <- sqldf("select * from OCC.Input_Freq")


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

head(df$PROM_LI)
head(as.numeric(gsub(",",".", df$PROM_LI)))
sum(is.na(as.numeric(gsub(",",".", df$PROM_LI))))

df$PROM_LI[is.na(as.numeric(gsub(",",".", df$PROM_LI)))]
View(df[is.na(as.numeric(gsub(",",".", df$PROM_LI))),])

sum(is.na(as.numeric(gsub(",",".", df$PROM_LI))))
sum(is.na(as.numeric(gsub(",",".", df$PREV_LI))))

if (class(df$PROM_LI) == "character")
{
  df$PROM_LI <- as.numeric(gsub(",",".", df$PROM_LI))
}

if (class(df$PREV_LI) == "character")
{
  df$PREV_LI <- as.numeric(gsub(",",".", df$PREV_LI))
}

#------------------------------------------------------------








#------------------------------------------------------------------------
#------------------------------------------------------------------------














############--------------------------- End --------------------------------- ########################