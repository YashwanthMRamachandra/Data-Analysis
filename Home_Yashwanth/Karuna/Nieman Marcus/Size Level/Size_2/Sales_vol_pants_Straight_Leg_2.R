###############################################################################################################################################################################################
###############################################################################################################################################################################################

########################################## Importing data  ############################################################

read.csv("H:/Karuna/Nieman Marcus/Sizes/Size_2/Sales_vol_pants_Straight_Leg_2.csv", header = TRUE, sep = ",", 
		quote = "\"",dec = ".")

Sales_vol_pants_Stght_Leg_2 <- read.table("H:/Karuna/Nieman Marcus/Sizes/Size_2/Sales_vol_pants_Straight_Leg_2.csv", 
		header = TRUE, sep = ",", quote = "\"",dec = ".")
names(Sales_vol_pants_Stght_Leg_2)



Sales_vol_pants_Stght_Leg_2_LOG <- log(Sales_vol_pants_Stght_Leg_2)
names(Sales_vol_pants_Stght_Leg_2_LOG)

lm(log(Sales_vol_pants_Stght_Leg_2))

				#####################################
				#####################################

############################################# Log Linear Model ##########################################################

library(MASS)
Sales_LOG_LM <- lm(Pants_.Stght_Leg_2_Sales_volume ~ ., data = Sales_vol_pants_Stght_Leg_2_LOG)
summary(Sales_LOG_LM)
help(lm)

Sales_LOG_LM <- lm(Pants_Stght_Leg_2_Sales_volume ~ ., data = Sales_vol_pants_Stght_Leg_2)
