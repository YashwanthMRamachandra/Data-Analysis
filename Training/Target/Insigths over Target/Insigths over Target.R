#############################################################################################################################################################################################################################################
#############################################################################################################################################################################################################################################

# Import the data  ##

read.csv("D:/Day_1/Target/Insigths over Target/Assortment against competitors by price(Retailers as Rows).csv", header = TRUE, sep = ",", quote = "\"",
         dec = ".",row.names=1)

Target_Assortment <- read.table("D:/Day_1/Target/Insigths over Target/Assortment against competitors by price(Retailers as Rows).csv", header = TRUE, sep = ",", quote = "\"",
           dec = ".",row.names=1)
names(Target_Assortment)

################################################################################################################# 
#################################################################################################################

# Normality Check  ##
help(qqplot)

help(Kmeans)
Kmeans(Target_Assortment,2,"euclidean") 
library(ca)
