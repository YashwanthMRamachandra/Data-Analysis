####################################################################################################################################
####################################################################################################################################
# ---------------------------------------- Canada EDA ------------------------------------------------------------------------------
####################################################################################################################################
####################################################################################################################################

####################################################################################################################################
# -------------------------------------  Calculate MS Metrics  --------------------------------------------------------------
####################################################################################################################################

Canada_Data$Margin_Dollar <- NULL
for(i in 1:nrow(Canada_Data)){
Canada_Data$Margin_Dollar[i] <- sum(c(Canada_Data$CADSALES[i]-Canada_Data$CADCOST[i],Canada_Data$CADMARGINCORRECTION[i]),
                                 na.rm = TRUE)
}

Canada_Data$Margin_Dollar_Comp <- NULL
for(i in 1:nrow(Canada_Data)){
  Canada_Data$Margin_Dollar_Comp[i] <- sum(c(Canada_Data$CADSALES[i]-Canada_Data$CADCOST[i],Canada_Data$CADMARGINCORRECTIONCOMP[i]),
                                      na.rm = TRUE)
}

Canada_Data$Margin_Per <- Canada_Data$Margin_Dollar/Canada_Data$CADSALES
Canada_Data$Avg_Selling_Price <- Canada_Data$CADSALES/Canada_Data$SALESUNITS
Canada_Data$Avg_Selling_Price[which(!is.finite(Canada_Data$Avg_Selling_Price))] <- 0

####################################################################################################################################
# -------------------------------------  Formatting Data  --------------------------------------------------------------
####################################################################################################################################
                                     
# Sort data by Date                                     
Canada_Data <- Canada_Data[order(Canada_Data$FSC_WK_END_DT),]

# Rename Column names
library(plyr)
Canada_Data <- rename(Canada_Data,c("fsc_qtr_nbr"="Quarter",
                                    "FSC_WK_END_DT"="Week",
                                    "Loc_NBR"="Location_NMR",
                                    "DBA_NME"="Store",
                                    "MER_DVS_NBR"="Merch_Division_NMR",
                                    "MER_DVS_DES_TXT"="Merch_Division_TEXT",
                                    "ITM_TYP_CD"="Item_Type_Code",
                                    "ITM_TYP_DES_TXT"="Item_Type_Desc",
                                    "ITM_LCT_STK_CD"="Item_Type_Desc_Code",
                                    "ITM_LCT_STK_DES_TXT"="Stock_Status",
                                    "CADSALES"="CAD_Sales_Dollar",
                                    "CADSALESCOMP"="CAD_Sales_Dollar_Comp",
                                    "COMPWEEKLYSALESUNITS"="Comp_Weekly_Sales_Units",
                                    "SALESUNITS"="Sales_Units",
                                    "CADCOST"="Cost",
                                    "CADINVENTORYONHAND"="Inventory_On_Hand_Dollar",
                                    "INVENTORYONHANDUNITS"="Inventory_On_Hand_Units",
                                    "CADUNITPRICE"="List_Price",
                                    "COUNTSALESUNITSINVOICECOUN"="Invoice_Count",
                                    "CADMARGINCORRECTIONCOMP"="Margin_Correction_Comp",
                                    "CADMARGINCORRECTION"="Margin_Correction",
                                    "CADCOSTCOMP"="Comp_Cost"))

# Change Data Types
Canada_Data$Quarter <- as.factor(Canada_Data$Quarter)

#---------------------------------------------------------------------------------------------------------------
#                                       Sampling : Stratification
#---------------------------------------------------------------------------------------------------------------

library(sampling)
Canada_Data_Stratum <- data.frame(table(Canada_Data$Store))
Canada_Data_Stratum$Per <- (Canada_Data_Stratum$Freq/sum(Canada_Data_Stratum$Freq))*100
names(Canada_Data_Stratum)[1] <- "Store"

# Consider 70% of the data as sample
Canada_Data_Stratum$Strata_Size <- ceiling((Canada_Data_Stratum$Freq*(ceiling((dim(Canada_Data)[1]/100)*70)/sum(Canada_Data_Stratum$Freq))))

# Stratification
set.seed(1000)
Canada_Data_Strata <- strata(Canada_Data,c("Store"),size = Canada_Data_Stratum$Strata_Size,
                             method = "srswor")
Canada_Data_Sample <- getdata(Canada_Data,Canada_Data_Strata)
Canada_Data_Sample <- Canada_Data_Sample[order(Canada_Data_Sample$Week),]
rm(Canada_Data_Strata)

# --------------------------------------------------------------------------------------------------------------
#                                    Distribution of Variables : Histogram
# --------------------------------------------------------------------------------------------------------------

library(ggplot2) # Reference : http://docs.ggplot2.org/0.9.2.1/
library(data.table)
# data.table 1.9.6  For help type ?data.table or https://github.com/Rdatatable/data.table/wiki
# The fastest way to learn (by data.table authors): https://www.datacamp.com/courses/data-analysis-the-data-table-way

# Distribution by Store

plot_ggHist <- function(x,na.rm = TRUE){
attach(Canada_Data_Sample)
plots <- NULL
cols <- names(Canada_Data_Sample[sapply(Canada_Data_Sample,is.numeric)])
for(i in 1:seq_along(cols)){
plots <- ggplot(x,aes(x=cols[i])) +
          facet_wrap(~ Stock_Status) +
          geom_histogram(aes(y=..density..),col = "blue2") +
          stat_function(fun = dnorm,args = list(mean=mean(cols[i]),sd=sd(cols[i])),colour = "red") +
          labs(title = "col[i] Distribution by Stock Status")
  }
detach(Canada_Data_Sample)
}



####################################################################################################################################
####################################################################################################################################
# -------------- END ----------------------- Canada EDA ------------------------------ END -----------------------------------------
####################################################################################################################################
####################################################################################################################################
                                     