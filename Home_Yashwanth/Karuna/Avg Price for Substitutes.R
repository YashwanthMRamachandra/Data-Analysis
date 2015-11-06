###############################################################################################################################################################################################
###############################################################################################################################################################################################

# Importing data  ##

read.csv("H:/Karuna/Target_substitue_file_v1.csv", header = TRUE, sep = ",", 
		quote = "\"",dec = ".",row.names=1)

TARGET_SUBSTITUTES <- read.table("H:/Karuna/Target_substitue_file_v1.csv", 
		header = TRUE, sep = ",", quote = "\"",dec = ".",row.names=1)

names(TARGET_SUBSTITUTES)
head(TARGET_SUBSTITUTES)

				#####################################
				#####################################
 
read.csv("H:/Karuna/PE_Baby_Cribs_Data_ks07112013.csv", header = TRUE, sep = ",", 
		quote = "\"",dec = ".")

PE_BABY_CRIBS <- read.table("H:/Karuna/PE_Baby_Cribs_Data_ks07112013.csv", 
		header = TRUE, sep = ",", quote = "\"",dec = ".",fill=TRUE)


###############################################################################################################################################################################################
###############################################################################################################################################################################################


which(TARGET_SUBSTITUTES[,1]==TARGET_SUBSTITUTES[,1])

which(TARGET_SUBSTITUTES[,1]=="DaVinci Alpha Mini Rocking Crib")
which(TARGET_SUBSTITUTES[,1]==levels(TARGET_SUBSTITUTES[,1]))

subset(TARGET_SUBSTITUTES[,1],subset=TARGET_SUBSTITUTES[,1])

levels(TARGET_SUBSTITUTES[,1])


help(table)
match(TARGET_SUBSTITUTES)


which((1:12)%%2 == 0) # which are even?
which(1:10 > 3, arr.ind = TRUE)