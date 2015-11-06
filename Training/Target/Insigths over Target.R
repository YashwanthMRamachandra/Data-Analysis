#############################################################################################################################################################################################################################################
#############################################################################################################################################################################################################################################

# Import the data  ##

read.csv("D:/Day_1/Target/Insigths over Target/Assortment by price/Assortment(by price) against competitors by price(Retailers as Columns).csv", header = TRUE, sep = ",", quote = "\"",
         dec = ".",row.names=1)

Target_Assortment <- read.table("D:/Day_1/Target/Insigths over Target/Assortment by price/Assortment(by price) against competitors by price(Retailers as Columns).csv", header = TRUE, sep = ",", quote = "\"",
           dec = ".",row.names=1)
names(Target_Assortment)

################################################################################################################# 
#################################################################################################################

# Above is the contigency table : follow the correspondance analysis  ##
install.packages("nnet",dependencies=TRUE)
