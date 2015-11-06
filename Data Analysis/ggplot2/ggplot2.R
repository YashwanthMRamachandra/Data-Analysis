rm(list=setdiff(ls(),"Assortment"))

Assortment <- read.table("C:/Yashwanth/Data Analysis/ggplot2/Assortment.csv",
                         header = TRUE, sep = ",", quote = "\"", dec = "." ,fill=TRUE, comment.char="", as.is=TRUE)

Barplot_Data <- data.frame(table(Assortment$site_name))
names(Barplot_Data) <- c("site_name","Frequency")
Barplot_Data <- Barplot_Data[with(Barplot_Data,order(Frequency)),]

Barplot_Data$site_name <- factor(Barplot_Data$site_name,
                                 levels=c("Eastbay","JustballGloves","Baseball Savings","Homerunmonkey",
                                          "Baseball Rampage","Baseball Express","Dicks Sporting Goods"))

par(mar=c(5,4,5,2))
ggplot(data=Barplot_Data, aes(y=Frequency, x=site_name, colour=site_name)) +
  geom_bar(position="dodge", stat="identity") + 
  geom_text(aes(label = Frequency)) +
  theme(axis.text.y=element_blank(),axis.ticks=element_blank(),axis.title.y=element_blank())+
  geom_point()

# Reference : https://trinkerrstuff.wordpress.com/2012/10/15/how-do-i-re-arrange-ordering-a-plot/
#           http://stackoverflow.com/questions/14313285/ggplot2-theme-with-no-axes-or-grid

# ggplot(order_by(car, ~ mpg, dat2), aes(x=car, y=mpg)) + 
#   geom_bar(stat="identity") + 
#   coord_flip() + ggtitle("Order Pretty Easy")
# 
# library(devtools)
# install_github("plotflow", "trinker")
# library(plotflow)
