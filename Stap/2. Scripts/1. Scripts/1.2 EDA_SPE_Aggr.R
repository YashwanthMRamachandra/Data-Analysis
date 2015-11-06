# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
#                                    Staples Price Elasticity : EDA
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------

rm(list=ls())
load("C:/Yashwanth/Staples PE/2. Scripts/0. R_data/2. Staples_PE_Aggr.RData")
rm(list=setdiff(ls(),"Staples_Aggr_Monthly_Dec"))

# --------------------------------------------------------------------------------------------------------------
#                                    Distribution of Variables : Histogram
# --------------------------------------------------------------------------------------------------------------
library(ggplot2) # Reference : http://docs.ggplot2.org/0.9.2.1/
library(data.table)
Staples_Aggr_Monthly_Dec <- data.table(Staples_Aggr_Monthly_Dec)
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
} # http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/

# Conversion Distribution
attach(Staples_Aggr_Monthly_Dec)
H1 <- ggplot(Staples_Aggr_Monthly_Dec,aes(x=Conversion)) +
  facet_wrap(~ Loyalty_Cat) +
  geom_histogram(aes(y=..density..),col = "blue2") +
  stat_function(fun = dnorm,args = list(mean=mean(Conversion),sd=sd(Conversion)),colour = "red") +
  labs(title = "Conversion Distribution")

# Price Competitiveness Distribution  
H2 <- ggplot(Staples_Aggr_Monthly_Dec,aes(x=Price_Comp)) +
  facet_wrap(~ Loyalty_Cat) +
  geom_histogram(aes(y=..density..), col = "blue2") +
  #stat_density() # http://docs.ggplot2.org/0.9.2.1/stat_density.html
  stat_function(fun = dnorm,args = list(mean=mean(Price_Comp),sd=sd(Price_Comp)),colour = "red") +
  labs(title = "Price Competitiveness Distribution",x="Price Competitiveness")
detach(Staples_Aggr_Monthly_Dec)

multiplot(H1,H2,cols = 2);rm(H1,H2)

# Overall Visits Distribution
attach(Staples_Aggr_Monthly_Dec)  
ggplot(Staples_Aggr_Monthly_Dec,aes(x=OA_Visits)) +
  facet_wrap(~ Loyalty_Cat) +
  geom_histogram(aes(y=..density..), col = "blue2") +
  #stat_density() # http://docs.ggplot2.org/0.9.2.1/stat_density.html
  stat_function(fun = dnorm,args = list(mean=mean(OA_Visits),sd=sd(OA_Visits)),colour = "red") +
  labs(title = "Overall Visits Distribution",x="Overall Visits")
detach(Staples_Aggr_Monthly_Dec)

# Overall Visits < 3000 Distribution
attach(Staples_Aggr_Monthly_Dec)  
ggplot(Staples_Aggr_Monthly_Dec[OA_Visits<=3000],aes(x=OA_Visits)) +
  facet_wrap(~ Loyalty_Cat) +
  geom_histogram(aes(y=..density..), col = "blue2") +
  #stat_density() # http://docs.ggplot2.org/0.9.2.1/stat_density.html
  stat_function(fun = dnorm,args = list(mean=mean(OA_Visits),sd=sd(OA_Visits)),colour = "red") +
  labs(title = "Overall Visits < 3000 Distribution",x="Overall Visits")
detach(Staples_Aggr_Monthly_Dec)


# Correlation by filteres Products
attach(Staples_Aggr_Monthly_Dec_Filtrd)  
ggplot(Staples_Aggr_Monthly_Dec,aes(x=OA_Visits)) +
  facet_wrap(~ Loyalty_Cat) +
  geom_histogram(aes(y=..density..), col = "blue2") +
  #stat_density() # http://docs.ggplot2.org/0.9.2.1/stat_density.html
  stat_function(fun = dnorm,args = list(mean=mean(OA_Visits),sd=sd(OA_Visits)),colour = "red") +
  labs(title = "Overall Visits",x="Overall Visits")
detach(Staples_Aggr_Monthly_Dec_Filtrd)


# Correlation by filteres Products : OA_Visits <= 3000
attach(Staples_Aggr_Monthly_Dec_Filtrd)  
ggplot(Staples_Aggr_Monthly_Dec[OA_Visits<=3000],aes(x=OA_Visits)) +
  facet_wrap(~ Loyalty_Cat) +
  geom_histogram(aes(y=..density..), col = "blue2") +
  #stat_density() # http://docs.ggplot2.org/0.9.2.1/stat_density.html
  stat_function(fun = dnorm,args = list(mean=mean(OA_Visits),sd=sd(OA_Visits)),colour = "red") +
  labs(title = "Overall Visits <= 3000 Distribution",x="Overall Visits")
detach(Staples_Aggr_Monthly_Dec_Filtrd)

# --------------------------------------------------------------------------------------------------------------
#                                        Correlation : Scatterplot
# --------------------------------------------------------------------------------------------------------------

# cor(Staples_Aggr_Monthly_Dec[Loyalty_Cat=="HIGH",c("Conversion" ,"Price_Comp"),with=FALSE])
# cor(Staples_Aggr_Monthly_Dec[Loyalty_Cat=="MID",c("Conversion" ,"Price_Comp"),with=FALSE])
# cor(Staples_Aggr_Monthly_Dec[Loyalty_Cat=="LOW",c("Conversion" ,"Price_Comp"),with=FALSE])

Staples_Aggr_Monthly_Dec[,(lm(Staples_Final_Price ~ Amz_Final_Price)$coefficients),by=list(Loyalty_Cat)]

ggplot(Staples_Aggr_Monthly_Dec,aes(y=Staples_Final_Price,x=Amz_Final_Price)) +
  facet_wrap(~ Loyalty_Cat) +
  geom_point(aes(colour=Loyalty)) + scale_colour_gradient(high = "yellowgreen")+
  labs(title = "Relationship between Staples & Amazon", x = "Amazon Final Price") +
  theme_bw(base_size = 12, base_family = "") +
  geom_abline(aes(intercept = 0.431108,slope = -0.06312445))

# --------------------------------------------------------------------------------------------------------------
#                                       Identifying Outliers : Boxplot
# --------------------------------------------------------------------------------------------------------------

# Conversion Distribution
BP1 <- ggplot(Staples_Aggr_Monthly_Dec,aes(factor(Loyalty_Cat),Conversion)) +
  #facet_wrap(~ Loyalty_Cat) +
  geom_boxplot(outlier.colour = "green",outlier.size = 3) +
  labs(title = "Conversion Distribution",x="Demand Loyalty", y="Conversion") +
  theme_bw(base_size = 12, base_family = "")

# Price Competitiveness Distribution
BP2 <- ggplot(Staples_Aggr_Monthly_Dec,aes(factor(Loyalty_Cat),Price_Comp)) +
  #facet_wrap(~ Loyalty_Cat) +
  geom_boxplot(outlier.colour = "green",outlier.size = 3) +
  labs(title = "Price Competitiveness Distribution",x="Demand Loyalty",y="Price Competitiveness") +
  theme_bw(base_size = 12, base_family = "")

multiplot(BP1,BP2,col = 2);rm(BP1,BP2)

# Overall Visits Distribution
ggplot(Staples_Aggr_Monthly_Dec,aes(factor(Loyalty_Cat),OA_Visits)) +
  #facet_wrap(~ Loyalty_Cat) +
  geom_boxplot(outlier.colour = "green",outlier.size = 3) +
  labs(title = "Overall Visits Distribution",x="Demand Loyalty",y="Overall Visits") +
  theme_bw(base_size = 12, base_family = "")

# Overall Visits < 3000 Distribution
ggplot(Staples_Aggr_Monthly_Dec[OA_Visits<=3000],aes(factor(Loyalty_Cat),OA_Visits)) +
  #facet_wrap(~ Loyalty_Cat) +
  geom_boxplot(outlier.colour = "green",outlier.size = 3) +
  labs(title = "Overall Visits Distribution",x="Demand Loyalty",y="Overall Visits") +
  theme_bw(base_size = 12, base_family = "")

# Filtered Products : Overall Visits Distribution
ggplot(Staples_Aggr_Monthly_Dec_Filtrd,aes(factor(Loyalty_Cat),OA_Visits)) +
  #facet_wrap(~ Loyalty_Cat) +
  geom_boxplot(outlier.colour = "green",outlier.size = 3) +
  labs(title = "Overall Visits Distribution",x="Demand Loyalty",y="Overall Visits") +
  theme_bw(base_size = 12, base_family = "")

# Filtered Products : Overall Visits < 3000 Distribution
ggplot(Staples_Aggr_Monthly_Dec_Filtrd[OA_Visits<=2500],aes(factor(Loyalty_Cat),OA_Visits)) +
  #facet_wrap(~ Loyalty_Cat) +
  geom_boxplot(outlier.colour = "green",outlier.size = 3) +
  labs(title = "Overall Visits Distribution",x="Demand Loyalty",y="Overall Visits") +
  theme_bw(base_size = 12, base_family = "")

#---------------------------------------------------------------------------------------------------------------
#                                        Replacing OR Removing Outliers
#---------------------------------------------------------------------------------------------------------------
#           Note :  it is NOT acceptable to drop an observation just because it is an outlier # Check Ref URL
#---------------------------------------------------------------------------------------------------------------



# --------------------------------------------------------------------------------------------------------------
#                                                  Correlation 
# --------------------------------------------------------------------------------------------------------------

cor(Staples_Aggr_Monthly_Dec[Loyalty_Cat=="HIGH",c("Conversion" ,"Price_Comp"),with=FALSE])
cor(Staples_Aggr_Monthly_Dec[Loyalty_Cat=="MID",c("Conversion" ,"Price_Comp"),with=FALSE])
cor(Staples_Aggr_Monthly_Dec[Loyalty_Cat=="LOW",c("Conversion" ,"Price_Comp"),with=FALSE])

cor(Staples_Aggr_Monthly_Dec[Loyalty_Cat=="HIGH" & OA_Visits<=3000,c("Conversion" ,"Price_Comp"),with=FALSE])
cor(Staples_Aggr_Monthly_Dec[Loyalty_Cat=="MID" & OA_Visits<=3000,c("Conversion" ,"Price_Comp"),with=FALSE])
cor(Staples_Aggr_Monthly_Dec[Loyalty_Cat=="LOW" & OA_Visits<=3000,c("Conversion" ,"Price_Comp"),with=FALSE])

# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
# -------------- END -------------------- Staples Price Elasticity : EDA ---------------- END ------------------
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------

# grid <- with(Staples_Aggr_Monthly_Dec,seq(min(Conversion),max(Conversion),length = 100))
# normaldens <- ddply(Staples_Aggr_Monthly_Dec,"Conversion",function(df){
#   data.frame(
#     predicted = grid,
#     density  = dnorm(grid,mean(df$Conversion),sd(df$Conversion))
#   )
# })
