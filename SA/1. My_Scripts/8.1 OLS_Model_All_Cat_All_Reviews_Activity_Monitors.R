# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
#                                   Best Seller Model ;  Logistic Ression
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------------
#                                         Input Data and data types
#---------------------------------------------------------------------------------------------------------------

rm(list=ls())
load("C:/Yashwanth/Sports Authority/3. Output/Review_Aggr_v2.RData")
rm(list=setdiff(ls(),c("Review_Aggr_Data","Review_Aggr_Model_Data")))
library(data.table)
Review_Aggr_Model_Data <- data.table(Review_Aggr_Model_Data)
Review_Aggr_Act_Mon_Model_Data <- Review_Aggr_Model_Data[Category=="Activity Monitors"]
Review_Aggr_Act_Mon_Model_Data$Product <- as.factor(Review_Aggr_Act_Mon_Model_Data$Product)
Review_Aggr_Act_Mon_Model_Data <- with(Review_Aggr_Act_Mon_Model_Data,Review_Aggr_Act_Mon_Model_Data[order(Sales_Units,decreasing = TRUE),])
Review_Aggr_Act_Mon_Model_Data$Sales <- c(rep(1,10),rep(0,length(11:nrow(Review_Aggr_Act_Mon_Model_Data))))
Review_Aggr_Act_Mon_Model_Data <- with(Review_Aggr_Act_Mon_Model_Data,Review_Aggr_Act_Mon_Model_Data[order(Product_ID,decreasing = TRUE),])

# --------------------------------------------------------------------------------------------------------------
#                                    Distribution of Variables : Histogram
# --------------------------------------------------------------------------------------------------------------

library(ggplot2) # Reference : http://docs.ggplot2.org/0.9.2.1/
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

# Reviews_Q1 Distribution
attach(Review_Aggr_Act_Mon_Model_Data)
H1 <- ggplot(Review_Aggr_Act_Mon_Model_Data,aes(x=Reviews_Q1)) +
  facet_wrap(~ Sales)+
  geom_histogram(aes(y=..density..),col = "blue2") +
  stat_function(fun = dnorm,args = list(mean=mean(Reviews_Q1),sd=sd(Reviews_Q1)),colour = "red") +
  labs(title = "Reviews_Q1 Distribution")

# Reviews_Q2 Distribution
H2 <- ggplot(Review_Aggr_Act_Mon_Model_Data,aes(x=Reviews_Q2)) +
  facet_wrap(~ Sales)+
  geom_histogram(aes(y=..density..),col = "blue2") +
  stat_function(fun = dnorm,args = list(mean=mean(Reviews_Q2),sd=sd(Reviews_Q2)),colour = "red") +
  labs(title = "Reviews_Q2 Distribution")

# Reviews_Q3 Distribution
H3 <- ggplot(Review_Aggr_Act_Mon_Model_Data,aes(x=Reviews_Q3)) +
  facet_wrap(~ Sales)+
  geom_histogram(aes(y=..density..),col = "blue2") +
  stat_function(fun = dnorm,args = list(mean=mean(Reviews_Q3),sd=sd(Reviews_Q3)),colour = "red") +
  labs(title = "Reviews_Q3 Distribution")

# Reviews_Q4 Distribution
H4 <- ggplot(Review_Aggr_Act_Mon_Model_Data,aes(x=Reviews_Q4)) +
  facet_wrap(~ Sales)+
  geom_histogram(aes(y=..density..),col = "blue2") +
  stat_function(fun = dnorm,args = list(mean=mean(Reviews_Q4),sd=sd(Reviews_Q4)),colour = "red") +
  labs(title = "Reviews_Q4 Distribution")
detach(Review_Aggr_Act_Mon_Model_Data)

dev.off()
multiplot(H1,H2,cols = 2)
multiplot(H3,H4,cols = 2)
rm(H1,H2,H3,H4)

# --------------------------------------------------------------------------------------------------------------
#                                        Correlation : Scatterplot
# --------------------------------------------------------------------------------------------------------------

# Reviews Q1
lm(Sales_Units ~ Reviews_Q1,data = Review_Aggr_Act_Mon_Model_Data)$coefficients

ggplot(Review_Aggr_Act_Mon_Model_Data,aes(y=Sales_Units,x=Reviews_Q1)) +
  geom_point(aes(colour=Sales_Units)) + scale_colour_gradient(high = "yellowgreen")+
  labs(title = "Relationship between Sales & Review_Q1", x = "Reviews Q1") +
  theme_bw(base_size = 12, base_family = "") +
  geom_abline(aes(intercept = lm(Sales_Units ~ Reviews_Q1,data = Review_Aggr_Act_Mon_Model_Data)$coefficients[[1]],
                  slope = lm(Sales_Units ~ Reviews_Q1,data = Review_Aggr_Act_Mon_Model_Data)$coefficients[[2]]))

# Reviews Q2
lm(Sales_Units ~ Reviews_Q2,data = Review_Aggr_Act_Mon_Model_Data)$coefficients

ggplot(Review_Aggr_Act_Mon_Model_Data,aes(y=Sales_Units,x=Reviews_Q2)) +
  geom_point(aes(colour=Sales_Units)) + scale_colour_gradient(high = "yellowgreen")+
  labs(title = "Relationship between Sales & Reviews_Q2", x = "Reviews Q2") +
  theme_bw(base_size = 12, base_family = "") +
  geom_abline(aes(intercept = lm(Sales_Units ~ Reviews_Q2,data = Review_Aggr_Act_Mon_Model_Data)$coefficients[[1]],
                  slope = lm(Sales_Units ~ Reviews_Q2,data = Review_Aggr_Act_Mon_Model_Data)$coefficients[[2]]))

# Reviews Q3
lm(Sales_Units ~ Reviews_Q3,data = Review_Aggr_Act_Mon_Model_Data)$coefficients

ggplot(Review_Aggr_Act_Mon_Model_Data,aes(y=Sales,x=Reviews_Q3)) +
  geom_point(aes(colour=Sales)) + scale_colour_gradient(high = "yellowgreen")+
  labs(title = "Relationship between Sales & Review_Q3", x = "Reviews Q3") +
  theme_bw(base_size = 12, base_family = "") +
  geom_abline(aes(intercept = lm(Sales_Units ~ Reviews_Q3,data = Review_Aggr_Act_Mon_Model_Data)$coefficients[[1]],
                  slope = lm(Sales_Units ~ Reviews_Q3,data = Review_Aggr_Act_Mon_Model_Data)$coefficients[[2]]))

# Reviews Q4
lm(Sales_Units ~ Reviews_Q4,data = Review_Aggr_Act_Mon_Model_Data)$coefficients

ggplot(Review_Aggr_Act_Mon_Model_Data,aes(y=Sales,x=Reviews_Q4)) +
  geom_point(aes(colour=Sales)) + scale_colour_gradient(high = "yellowgreen")+
  labs(title = "Relationship between Sales & Review_Q4", x = "Reviews Q4") +
  theme_bw(base_size = 12, base_family = "") +
  geom_abline(aes(intercept = lm(Sales_Units ~ Reviews_Q4,data = Review_Aggr_Act_Mon_Model_Data)$coefficients[[1]],
                  slope = lm(Sales_Units ~ Reviews_Q4,data = Review_Aggr_Act_Mon_Model_Data)$coefficients[[2]]))

# --------------------------------------------------------------------------------------------------------------
#                                       Identifying Outliers : Boxplot
# --------------------------------------------------------------------------------------------------------------

# Reviews Q1 Distribution
  ggplot(Review_Aggr_Act_Mon_Model_Data,aes(as.factor(Sales),Reviews_Q1)) +
    geom_boxplot(outlier.colour = "green",outlier.size = 3) +
    labs(title = "Reviews_Q1 Distribution",x="Sales", y="Reviews_Q1") +
    theme_bw(base_size = 12, base_family = "")

# Reviews Q2 Distribution
ggplot(Review_Aggr_Act_Mon_Model_Data,aes(as.factor(Sales),Reviews_Q2)) +
  geom_boxplot(outlier.colour = "green",outlier.size = 3) +
  labs(title = "Reviews_Q1 Distribution",x="Sales", y="Reviews_Q2") +
  theme_bw(base_size = 12, base_family = "")

# Reviews Q3 Distribution
ggplot(Review_Aggr_Act_Mon_Model_Data,aes(as.factor(Sales),Reviews_Q3)) +
  geom_boxplot(outlier.colour = "green",outlier.size = 3) +
  labs(title = "Reviews_Q1 Distribution",x="Sales", y="Reviews_Q3") +
  theme_bw(base_size = 12, base_family = "")

# Reviews Q4 Distribution
ggplot(Review_Aggr_Act_Mon_Model_Data,aes(as.factor(Sales),Reviews_Q4)) +
  geom_boxplot(outlier.colour = "green",outlier.size = 3) +
  labs(title = "Reviews_Q1 Distribution",x="Sales", y="Reviews_Q4") +
  theme_bw(base_size = 12, base_family = "")
