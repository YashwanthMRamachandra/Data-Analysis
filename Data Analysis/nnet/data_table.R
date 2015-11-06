# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
#                                           Data Table Package
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------

rm(list=ls())
data(cars);cars

class(cars)
Cars_DT <- data.table(cars)
class(Cars_DT)

head(cars)
head(Cars_DT)

tables()
sapply(cars,class)
sapply(Cars_DT,class)

Cars_DT[4,]
Cars_DT[speed == 11,]

setkey(Cars_DT,speed)
Cars_DT
tables()

# Not clear
Cars_DT[speed == 11,]
Cars_DT[11,mult="first"]
Cars_DT[11,mult="last"]
Cars_DT[11]

# SII data
SII <- read.table("C:/Yashwanth/Data Analysis/Logistic Regression/SII/Output_all.csv",
                  header = TRUE, sep = ",", quote = "\"", dec = "." ,fill=TRUE, comment.char="", as.is=TRUE)

SII_DT <- data.table(SII);rm(SII)

SII_DT$L_unitsales <- as.factor(SII_DT$L_unitsales)
setkey(SII_DT,L_unitsales)
SII_DT["1",]
SII_DT["1",mult="first"]
SII_DT["1",mult="last"]



#---------------------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------------
