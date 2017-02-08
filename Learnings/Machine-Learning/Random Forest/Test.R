Test <- DMC_PBI[sapply(DMC_PBI,is.factor)]


for(i in levels(Test$job)){
  print(i)
}



Freq_table <- NA
for(j in names(Test)){
  for(i in list(table(Test$j)))
  Freq_table[i] <- list(table(Test$i))
}

lapply(table(Test),table)

list(levels(Test$job),levels(Test$marital),levels(Test$education),levels(Test$default),levels(Test$housing),
     levels(Test$loan),levels(Test$contact),levels(Test$month),levels(Test$day_of_week),levels(Test$poutcome),
     levels(Test$y))
