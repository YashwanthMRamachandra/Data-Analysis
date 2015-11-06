

#--- Load input files
Social_Signals <- read.csv(file="/home/mono/Documents/Assortment/Top_25/Data/Inputs/Social_Signals.csv")
load(file="/home/mono/Documents/Assortment/Top_25/Data/ProcessedData/1_All_Review_Processed.RData")

#--- Prepare data
for(i in c(1:ncol(Social_Signals))){
  Social_Signals[,i] <- as.numeric(Social_Signals[,i])
}

#--- Track retailer columns
Retailers <- colnames(Social_Signals)

#--- Calculate ratio
# System command
#system('echo "# Calculating ratio..."; echo "65"')
#---------
Total_Reviews <- nrow(All_Review)
Total_Social_Signals <- sum(Social_Signals,na.rm = T)

Review_Social_Signal_Ratio <- Total_Reviews / Total_Social_Signals

#--- Calculate social signals
# System command
#system('echo "# Calculating social signals..."; echo "70"')
#---------
Social_Signals$Total_SS <- rowSums(Social_Signals[,c(1:length(Retailers))], na.rm=T)

#--- Calculate Net Social Signals
Social_Signals$Net_Social_Signal <- Social_Signals$Total_SS / Review_Social_Signal_Ratio

write.csv(Search_Score_Data, file = "~/Polos_Bestseller.csv")
#--- Save data
# System command
#system('echo "# Saving data..."; echo "75"')
#---------
save(Social_Signals,file="/home/mono/Documents/Assortment/Top_25/Data/ProcessedData/4_Social_Signal_Data.RData")

rm(list=ls())
