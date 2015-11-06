load("/home/mono/Documents/Assortment/Top_25/Data/ProcessedData/1_All_Review_Processed.RData")
max(table(All_Review$Retailer))
x<-table(All_Review$Retailer)
laply(x,function(x){x/max(table(All_Review$Retailer))})

