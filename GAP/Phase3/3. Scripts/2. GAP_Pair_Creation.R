# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
#                                       || standardization of data  ||
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------

rm(list=ls())
GAP_Input <- read.table("C:/Yashwanth/GAP/15. Phase3/1. Input_data/Data_Input_v2.csv", 
                        header = TRUE, sep = ",", quote = "\"", dec = "." ,fill=TRUE, comment.char="", as.is=TRUE)
str(GAP_Input)
GAP_Input$Level_1 <- as.factor(GAP_Input$Level_1)
GAP_Input$Level_2 <- as.factor(GAP_Input$Level_2)
GAP_Input$Category_Id <- as.factor(GAP_Input$Category_Id)
GAP_Input$Average_Bounce_rate <- round(GAP_Input$Average_Bounce_rate,2)
GAP_Input$Avg_Time_spent_on_page <- round(GAP_Input$Avg_Time_spent_on_page,2)

library(clusterSim)
GAP_Input_Std <- data.Normalization(GAP_Input[,6:9],type="n1")
rownames(GAP_Input_Norm) <- GAP_Input[,1]

# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
#                                       || Distance Matrix  ||
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------

GAP_Input_Dist <- data.frame(as.matrix(dist(GAP_Input_Norm,upper=TRUE)))
colnames(GAP_Input_Dist) <- GAP_Input[,1]
rownames(GAP_Input_Dist) <- GAP_Input[,1]
library(openxlsx)
write.xlsx(GAP_Input_Dist,"C:/Yashwanth/GAP/15. Phase3/4. Output/GAP_Input_Dist_v1.xlsx",sheet = 1,
           sheetName = "Distance matrix",row.names = TRUE)

# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
#                                       || Correlation matrix  ||
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------

GAP_Input_Cor <- data.frame(cor(GAP_Input[,sapply(GAP_Input, is.numeric)]))
# http://www.r-bloggers.com/quickly-export-multiple-r-objects-to-an-excel-workbook/
save.xlsx <- function (file, ...)
{
  require(xlsx, quietly = TRUE)
  objects <- list(...)
  fargs <- as.list(match.call(expand.dots = TRUE))
  objnames <- as.character(fargs)[-c(1, 2)]
  nobjects <- length(objects)
  for (i in 1:nobjects) {
    if (i == 1)
      write.xlsx(objects[[i]], file, sheetName = objnames[i])
    else write.xlsx(objects[[i]], file, sheetName = objnames[i],
                    append = TRUE)
  }
  print(paste("Workbook", file, "has", nobjects, "worksheets."))
}
save.xlsx("C:/Yashwanth/GAP/15. Phase3/4. Output/GAP_Input_Dist_v1.xlsx",GAP_Input_Dist,GAP_Input_Cor)

# --------------------------------------------------------------------------------------------------------------
# 