##############################################################################################################################################################
##############################################################################################################################################################
                                    # Foundations of Probability
##############################################################################################################################################################
##############################################################################################################################################################

# 1.1 Sample Spaces/ Derive a Sample Space
install.packages("prob",dependencies = TRUE)
library(prob)
library(combinat)
tosscoin(1)
tosscoin(3)

rolldie(1)
View(rolldie(3,nsides = 4))

View(cards())
# Urn : In Prob & Statistics, an urn problem is an idealized mental exercise in which some objects 
# of real interest (such as atoms, people, cars, etc.) are represented as colored balls in an urn or 
# other container.

# Sampling with Urns
urnsamples(1:3,2,TRUE,TRUE) # Ordered with replacement
urnsamples(1:3,2,FALSE,TRUE) # Ordered w/o replacement

# If the numbers 1, 2, and 3 represented “Fred”, “Mary”, and “Sue”, respectively,
# then this experiment would be equivalent to selecting two people of the three to serve as president
# and vice-president of a company, respectively, and the sample space lists all possible ways that
# this could be done.

urnsamples(1:3,2,FALSE,FALSE) # Unordered w/o replacement
urnsamples(1:3,2,TRUE,FALSE) # Unordered with replacement

# consider each outcome a separate way to distribute two identical golf balls into three boxes labeled 
# 1, 2, and 3.Regardless of the interpretation, urnsamples() lists every possible way that the experiment can conclude.
# Remember all the samples are distinguishable.

# Generate n-samples
nsamp(n=3,k=2,replace = TRUE, ordered = TRUE)
nsamp(n=3,k=2,replace = FALSE, ordered = TRUE)


# 1.2 Defining Probablility Space
# Equally likely model
rolldie(1)
probspace(rolldie(1),probs = rep(1/6,times = 6))
probspace(1:6)
tosscoin(2,makespace = TRUE)

probspace(tosscoin(1),probs = c(0.7,0.3))


# 1.3 Joint & Conditional Probabilities
S <- cards(makespace = TRUE) # makespace gives probs
A <- subset(S, suit == "Heart") # table(cards()$suit)
B <- subset(S, rank %in% 7:9)


# Probability
Prob(A) 
Prob(S, suit=="Heart")

Prob(B)
Prob(S)


# Conditional Probablity
# S={draw a card},A ={suit = "Heart"} and B={rank is 7, 8, or 9}
Prob(A, given = B)
Prob(intersect(A,B))
Prob(B)

Prob(union(A,B))
Prob(A) + Prob(B) - Prob(intersect(A,B))

# Or perhaps the Multiplication Rule:
Prob(intersect(A,B))
Prob(A) * Prob(B, given = A)

# We could give evidence that consecutive trials of flipping a coin are independent:
# Keep in mind, however, that the point is
# not that R (or any other software package, for that matter) will ever be an effective surrogate for
# critical thinking; rather, the point is that statistical tools like R serve to change the classroom
# landscape, hopefully for the better. Plus, it's free.

# Simulating Experiments
sim(S,ntrials = 5)
# We only simulated the experiment 5 times in the interest of space; a person could simulate
# as many trials as desired (within reason).

S <- tosscoin(2, makespace = TRUE)
sims <- sim(S, ntrials = 50000)
empirical(sims)
# We see that each outcome has an observed relative frequency fairly close to 0.25, as we would
# expect. Of course, this doesn't prove that the sampling is perfect, but it is good news nonetheless.



# Normal Distributions
set.seed(100)
Sample <- data.frame(X=rnorm(1000,0.8,3))
library(ggplot2)
ggplot(Sample,aes(x=X)) + 
  geom_histogram( aes(y=..density..),breaks=seq(-10,11,by=0.5),colour="black",fill="white") +
  stat_function(fun=dnorm, args=list(mean=mean(Sample$X), sd=sd(Sample$X)))+
  labs(title="Normal Distribution",y="Probability") 
summary(Sample)



# If N is sufficiently large
Sample <- rnorm(1000,0.8,3)
range(Sample)
Sample <- append(Sample,c(50,55,45,36,53))
range(Sample)
Sample <- data.frame(X=Sample)
ggplot(Sample,aes(x=X)) + 
  geom_histogram( aes(y=..density..),breaks=seq(-10,55,by=0.5),colour="black",fill="white") +
  stat_function(fun=dnorm, args=list(mean=mean(Sample$X), sd=sd(Sample$X)))+
  labs(title="Normal Distribution",y="Probability")


library(clusterSim)
help("data.Normalization")
range(Sample)
Sample_norm <- data.frame(X=data.Normalization(Sample$X,type = "n1"))
range(Sample_norm$X)
ggplot(Sample_norm,aes(x=X)) + 
  geom_histogram( aes(y=..density..),breaks=seq(-3,10,by=0.1),colour="black",fill="white") +
  stat_function(fun=dnorm, args=list(mean=mean(Sample_norm$X), sd=sd(Sample_norm$X)))+
  labs(title="Normal Distribution",y="Probability")


data("iris");iris
View(iris)
iris_demo <- iris
range(iris_demo$Sepal.Length)
iris_demo <- rbind(iris,c(45,41,34,42,"virginica"))
iris_demo <- as.data.frame(lapply(iris_demo[1:4],as.numeric))
range(iris_demo$Sepal.Length)


ggplot(iris_demo,aes(x=Sepal.Length)) + 
  geom_histogram( aes(y=..density..),breaks=seq(4,48,by=1),colour="black",fill="white") +
  stat_function(fun=dnorm, args=list(mean=mean(iris_demo$Sepal.Length), sd=sd(iris_demo$Sepal.Length)))+
  labs(title="Normal Distribution",y="Probability") 

iris_norm <- data.frame(Sepal.Length=data.Normalization(iris_demo$Sepal.Length,type = "n1"))
range(iris_norm$Sepal.Length)
ggplot(iris_norm,aes(x=Sepal.Length)) + 
  geom_histogram( aes(y=..density..),breaks=seq(-3,13,by=0.5),colour="black",fill="white") +
  stat_function(fun=dnorm, args=list(mean=mean(iris_norm$Sepal.Length), sd=sd(iris_norm$Sepal.Length)))+
  labs(title="Normal Distribution",y="Probability") 

range(iris$Sepal.Length)
shapiro.test(iris$Sepal.Length)
range(iris_demo$Sepal.Length)
shapiro.test(iris_demo$Sepal.Length)


# Binomial Distributions
# Suppose we have a binomial random variable X over 10 trials, where each trial has a
# success probability of 1/2. Then we can calculate the probability of observing x = 7 by
# calling dbinom:
dbinom(7, size=10, prob=0.5) # P(X=7)
pbinom(7, size=10, prob=0.5) # P(X<=7)

tosscoin(2, makespace = TRUE)

x1 <- 3:17;dbinom(x1, 20, 0.5)
df <- data.frame(x = x1, y = dbinom(x1, 20, 0.5)) # P(X=3),P(X=4),...,P(X=17)
ggplot(df, aes(x = x, y = y)) + geom_bar(stat = "identity", col = "pink", fill = "pink") + 
  scale_y_continuous(expand = c(0.01, 0)) + xlab("x") + ylab("Density") + 
  labs(title = "dbinom(x, 20, 0.5)") + theme_bw(16, "serif") + 
  theme(plot.title = element_text(size = rel(1.2), vjust = 1.5))


# Poisson Distribution
# The average number of homes sold by the Acme Realty 
# company is 2 homes per day. 
# What is the probability that exactly 3 homes will be 
# sold tomorrow?
dpois(3,2)
dpois(0:10,2)
ggplot(data.frame(x=c(0:10)), aes(x)) +
  geom_point(aes(y=dpois(x, 2)), colour="red")


# Geometric Distribution/Negative Binomial Distribution
# Suppose we flip a coin repeatedly and count the number 
# of heads (successes). If we continue flipping 
# the coin until it has landed 2 times on heads, 
# we are conducting a negative binomial experiment . 
# The negative binomial random variable is the number 
# of coin flips required to achieve 2 heads. 
# In this example, the random variable can take on any integer value between 2 and plus infinity. 
# The negative binomial probability distribution for this example is presented below.

dnbinom(10,20,prob = 0.5)
dnbinom(10:100,20,prob = 0.5)
ggplot(data.frame(x=c(10:100)), aes(x)) +
  geom_point(aes(y=dnbinom(x, 20, 0.5)), colour="red")


##############################################################################################################################################################
##############################################################################################################################################################
#           END                            Foundations of Probability                      END
##############################################################################################################################################################
##############################################################################################################################################################




##############################################################################################################################################################
##############################################################################################################################################################
                                            # Limit Theorems #
##############################################################################################################################################################
##############################################################################################################################################################

# Central limit thoerem
sdm.sim <- function(n,src.dist=NULL,param1=NULL,param2=NULL) {
  r <- 10000  # Number of replications/samples - DO NOT ADJUST
  # This produces a matrix of observations with  
  # n columns and r rows. Each row is one sample:
  my.samples <- switch(src.dist,
                       "E" = matrix(rexp(n*r,param1),r),
                       "N" = matrix(rnorm(n*r,param1,param2),r),
                       "U" = matrix(runif(n*r,param1,param2),r),
                       "P" = matrix(rpois(n*r,param1),r),
                       "C" = matrix(rcauchy(n*r,param1,param2),r),
                       "B" = matrix(rbinom(n*r,param1,param2),r),
                       "G" = matrix(rgamma(n*r,param1,param2),r),
                       "X" = matrix(rchisq(n*r,param1),r),
                       "T" = matrix(rt(n*r,param1),r))
  all.sample.sums <- apply(my.samples,1,sum)
  all.sample.means <- apply(my.samples,1,mean)   
  all.sample.vars <- apply(my.samples,1,var) 
  par(mfrow=c(2,2))
  hist(my.samples[1,],col="gray",main="Distribution of One Sample")
  hist(all.sample.sums,col="gray",main="Sampling Distributionnof
	the Sum")
  hist(all.sample.means,col="gray",main="Sampling Distributionnof the Mean")
  hist(all.sample.vars,col="gray",main="Sampling Distributionnof
	the Variance")
}
sdm.sim(50,src.dist="E",param1=1)

##############################################################################################################################################################
##############################################################################################################################################################
#           END                              Limit Theorems                          END
##############################################################################################################################################################
##############################################################################################################################################################




##############################################################################################################################################################
##############################################################################################################################################################
                                            # Basic Statistics #
##############################################################################################################################################################
##############################################################################################################################################################

library(openxlsx)
data(iris);iris

mean(iris$Sepal.Length)
median(iris$Sepal.Width)


library(modes)
v <- c(2,1,2,3,1,2,3,4,1,5,5,3,2,3)
modes(v);v
modes(iris$Sepal.Width)


summary(iris)
range(iris$Sepal.Length)
var(iris$Sepal.Length)
sd(iris$Sepal.Length)

cov(iris$Sepal.Length,iris$Sepal.Width)
cor(iris$Sepal.Length,iris$Sepal.Width)

data("mtcars");mtcars
cov(mtcars$mpg,mtcars$wt)
cor(mtcars$mpg,mtcars$wt)


# Advanced functions
library(psych)
describe(iris$Sepal.Length) # note down the formulae


# ggplot2
mydata100 <- read.xlsx("C:/Yashwanth/9. My Folders/Workshops/1. MIT/mydata100.xlsx")
mydata100$q3 <- as.numeric(mydata100$q3)
ggplot(mydata100,  aes(x = factor(""), fill = workshop) ) +
  geom_bar()

ggplot(mydata100,
       aes(x = factor(""), fill = workshop) ) +
  geom_bar() +
  coord_polar(theta = "y") +
  scale_x_discrete("")

ggplot(mydata100) +
  geom_bar( aes(workshop) )

ggplot(mydata100, aes(workshop) ) +
  geom_bar() + coord_flip()

ggplot(mydata100, aes(workshop, fill = workshop ) ) +
  geom_bar()

ggplot(mydata100, aes(gender, fill = workshop) ) +
  geom_bar(position = "stack")


##############################################################################################################################################################
##############################################################################################################################################################
#           END                               Basic Statistics                          END
##############################################################################################################################################################
##############################################################################################################################################################


##############################################################################################################################################################
##############################################################################################################################################################
                                            # Graphical Visualization #
##############################################################################################################################################################
##############################################################################################################################################################

# Google Viz
library(googleVis)
data("Fruits");Fruits
M <- gvisMotionChart(Fruits, idvar="Fruit", timevar="Year")
plot(M)


Bubble <- gvisBubbleChart(Fruits, idvar="Fruit", 
                          xvar="Sales", yvar="Expenses",
                          colorvar="Year", sizevar="Profit",
                          options=list(
                            hAxis='{minValue:75, maxValue:125}'))
plot(Bubble)


data("CityPopularity");CityPopularity
Gauge <-  gvisGauge(CityPopularity, 
                    options=list(min=0, max=800, greenFrom=500,
                                 greenTo=800, yellowFrom=300, yellowTo=500,
                                 redFrom=0, redTo=300, width=400, height=300))
plot(Gauge)

data("Andrew");head(Andrew)
AndrewMap <- gvisMap(Andrew, "LatLong" , "Tip", 
                     options=list(showTip=TRUE, 
                                  showLine=TRUE, 
                                  enableScrollWheel=TRUE,
                                  mapType='terrain', 
                                  useMapTypeControl=TRUE))
plot(AndrewMap)


G <- gvisGeoChart(Exports, "Country", "Profit",
                  options=list(width=200, height=100))
T <- gvisTable(Exports,
                  options=list(width=200, height=270))
M <- gvisMotionChart(Fruits, "Fruit", "Year",
                        options=list(width=400, height=370))
GT <- gvisMerge(G,T, horizontal=FALSE)
GTM <- gvisMerge(GT, M, horizontal=TRUE,
                     tableOptions="bgcolor=\"#CCCCCC\" cellspacing=10")
plot(GTM)


# rCharts
library(devtools)
library(Rcpp)
library(rCharts)

data("HairEyeColor");HairEyeColor
hair_eye_male <- subset(as.data.frame(HairEyeColor), Sex == "Male")
head(hair_eye_male)
n1 <- nPlot(Freq ~ Hair, group = "Eye", data = hair_eye_male, 
            type = 'multiBarChart')
n1


# Stock Analysis 
library(quantmod)
library(ggplot2)
library(xts)
library(dygraphs)
library(tseries)

getSymbols("AAPL")
chartSeries(AAPL, subset='last 12 months')
addBBands()

getSymbols(c("ORCL","IBM"))


# Get IBM and Linkedin stock data from Yahoo Finance
ibm_url <- "http://real-chart.finance.yahoo.com/table.csv?s=IBM&a=07&b=24&c=2010&d=07&e=24&f=2015&g=d&ignore=.csv"
lnkd_url <- "http://real-chart.finance.yahoo.com/table.csv?s=LNKD&a=07&b=24&c=2010&d=07&e=24&f=2015&g=d&ignore=.csv"

yahoo.read <- function(url){
  dat <- read.table(url,header=TRUE,sep=",")
  df <- dat[,c(1,5)]
  df$Date <- as.Date(as.character(df$Date))
  return(df)}

ibm  <- yahoo.read(ibm_url)
lnkd <- yahoo.read(lnkd_url)

# Plot with the htmlwidget dygraphs
# dygraph() needs xts time series objects
ibm_xts <- xts(ibm$Close,order.by=ibm$Date,frequency=365)
lnkd_xts <- xts(lnkd$Close,order.by=lnkd$Date,frequency=365)

stocks <- cbind(ibm_xts,lnkd_xts)

dygraph(stocks,ylab="Close", 
        main="IBM and Linkedin Closing Stock Prices") %>%
  dySeries("..1",label="IBM") %>%
  dySeries("..2",label="LNKD") %>%
  dyOptions(colors = c("blue","brown")) %>%
  dyRangeSelector()


# Live Stock Price analysis
aapl<-get.hist.quote(instrument = "aapl", quote = c("Cl", "Vol"))
goog <- get.hist.quote(instrument = "goog", quote = c("Cl", "Vol"))
msft <- get.hist.quote(instrument = "msft", quote = c("Cl", "Vol"))

plot(msft$Close,main = "Stock Price Comparison",
     ylim=c(0,800) ,col="red" ,type="l" ,lwd=0.5,
     pch=19 ,cex=0.6 ,xlab="Date" ,ylab="Stock Price (USD)")

lines(goog$Close,col="blue",lwd=0.5)
lines(aapl$Close,col="gray",lwd=0.5)

legend("top",horiz=TRUE,legend=c("Microsoft","Google","Apple"),
       col=c("red","blue","gray"),lty=1,bty="n")


##############################################################################################################################################################
##############################################################################################################################################################
#           END                               Graphical Visualization                          END
##############################################################################################################################################################
##############################################################################################################################################################




##############################################################################################################################################################
##############################################################################################################################################################
                                            # Hypothesis Testing #
##############################################################################################################################################################
##############################################################################################################################################################

# Testing of Hypothesis, Test Statistic, t-test

# Null Hypothesis      : Posttest score are NOT significantly better 
#                          than pretest scores
# Alternate Hypothesis : Posttest score are significantly better than 
#                        pretest scores
t.test(mydata100$pretest,mydata100$posttest, alternative = "less")

# Conclusion : Since p<0.05, we reject the Null Hyp & conclude that Posttest scores are
#              Significantly better than Pretest scores


# Chi-square

# Null Hypothesis      : Icecream flavour are independent of Gender factor
# Alternate Hypothesis : Icecream flavour are dependent of Gender factor
# Entering the data into vectors
men <- c(100, 120, 60)
women <- c(350, 200, 90)

# combining the row vectors in matrices, then converting the matrix into a data frame
ice.cream.survey <- as.data.frame(rbind(men, women))

# assigning column names to this data frame
names(ice.cream.survey) <- c('chocolate', 'vanilla', 'strawberry')
View(ice.cream.survey)

chisq.test(ice.cream.survey)
# Conclusion : Since p<0.05, we reject the Null Hypo & conclude that Ice-cream flavour are
#              dependent of Gender factor
# Interpretation : 1. It provides strong evidence to suggest that gender and 
# ice cream flavour  preference are dependent or have some association.
#                  2. It provides strong evidence to suggest that men and women tend to have difference 
#                     preferences for ice cream flavours.


##############################################################################################################################################################
##############################################################################################################################################################
#           END                               Hypothesis Testing                        END
##############################################################################################################################################################
##############################################################################################################################################################



##############################################################################################################################################################
##############################################################################################################################################################
                                            # Regression Analysis #
##############################################################################################################################################################
##############################################################################################################################################################


# Linear Models : Simple linear regression
# Sampling
set.seed(400)
mydata100_IND <- sample(nrow(mydata100),size=round(((nrow(mydata100)/100)*70)+1,0))
mydata100_Train <- mydata100[mydata100_IND,]
mydata100_Test <- mydata100[-mydata100_IND,]

# Null Hypothesis      : There is NO significant difference between pretest & posttest scores
# Alternate Hypothesis : There is a significant difference between pretest & posttest scores
cor(mydata100_Train$pretest,mydata100_Train$posttest)
lm_fit <- lm(posttest ~ pretest, data = mydata100_Train)
summary(lm_fit)
# Conclusion : Since p<0.05, we reject Null Hypo & conclude that there is a 
#          significant difference
#              between pretest scores & posttest scores


lm_fit$fitted.values
plot(lm_fit)
Predictions <- data.frame(Actuals = mydata100_Test$posttest,
                Prediction=floor(predict(lm_fit,mydata100_Test,type = "response")),
                Residuals = mydata100_Test$posttest-floor(predict(lm_fit,mydata100_Test,type = "response")))
ggplot(Predictions,aes(x=scale(Predictions$Residuals)[,1])) + 
  geom_histogram( aes(y=..density..),breaks=seq(-2,3.5,by=0.3),colour="black",fill="white") +
  stat_function(fun=dnorm, args=list(mean=mean(scale(Predictions$Residuals)[,1]), 
  sd=sd(scale(Predictions$Residuals)[,1])))+
  labs(title="Residual Distribution",x="Standardised Residuals",y="Probability") 


# Least square estimation
library(ggplot2)
ggplot(mydata100,aes(pretest,posttest)) +
      geom_point()
ggplot(mydata100,aes(pretest,posttest)) +
      geom_point(aes(colour = as.factor(workshop)))+
      geom_abline(intercept = 18.66470, slope = 0.84561)

plot(mydata100$pretest,mydata100$posttest)
abline(lm(posttest ~ pretest, data = mydata100), col = "red")


# Linear Models : Multiple linear regression
lm_fit_MLR <- lm(posttest ~ q1+q2+q3+q4+pretest, data = mydata100_Train)
summary(lm_fit_MLR)
lm_fit_MLR$fitted.values
plot(lm_fit_MLR)
Predictions <- data.frame(Actuals = mydata100_Test$posttest,
                          Prediction=floor(predict(lm_fit,mydata100_Test,type = "response")),
                          Residuals = mydata100_Test$posttest-floor(predict(lm_fit_MLR,mydata100_Test,type = "response")))


# ANOVA
# Null Hypothesis      : Group means of 4-different Workshops are NOT significantly different from each other
# Alternate Hypothesis : Group means of 4-different Workshops are significantly different from each other
anova(lm(posttest ~ gender+workshop, data = mydata100_Train))
# Conclusion : since p<0.05, we reject Null Hypo & conclude that means of 4-different workshops are
#              significantly different from each other.


# Mean Square Error
mean(lm(posttest ~ pretest, data = mydata100)$residuals^2)


# Mean absolute error
mean(abs(lm(posttest ~ pretest, data = mydata100)$residuals))


##############################################################################################################################################################
##############################################################################################################################################################
#           END                               Regression Analysis                          END
##############################################################################################################################################################
##############################################################################################################################################################



##############################################################################################################################################################
##############################################################################################################################################################
                                            # Density Estimation #
##############################################################################################################################################################
##############################################################################################################################################################

# K-means clustering
library(datasets)

str(attitude)

# Summarise data
summary(attitude)

# Subset the attitude data
dat <- attitude[,c(3,4)]

# Plot subset data
plot(dat, main = "% of favourable responses to Learning and Privilege", pch =20, cex =2)

# Perform K-Means with 2 clusters
set.seed(7)
km1 <- kmeans(dat, 2, nstart=100)

# Plot results
plot(dat, col =(km1$cluster +1) , main="K-Means result with 2 clusters", pch=20, cex=2)


# Check for the optimal number of clusters given the data
mydata <- dat
wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(mydata,
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares",
     main="Assessing the Optimal Number of Clusters with the Elbow Method",
     pch=20, cex=2)

# Perform K-Means with the optimal number of clusters identified from the Elbow method
set.seed(7)
km2 <- kmeans(dat, 6, nstart=100)

# Examine the result of the clustering algorithm
km2

# Plot results
plot(dat, col =(km2$cluster +1) , main="K-Means result with 6 clusters", pch=20, cex=2)


##############################################################################################################################################################
##############################################################################################################################################################
#           END                               Density Estimation                          END
##############################################################################################################################################################
##############################################################################################################################################################
