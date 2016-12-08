##############################################################################################################################################################
##############################################################################################################################################################
                                    # Foundations of Probability
##############################################################################################################################################################
##############################################################################################################################################################

data(mtcars);mtcars;

# 1.1 Sample Spaces/ Derive a Sample Space
library(prob)
library(combinat)
tosscoin(1)
tosscoin(3)

rolldie(1)
View(rolldie(3,nsides = 4))

cards()
# Urn : In Prob & Statistics, an urn problem is an idealized mental exercise in which some objects of real interest (such as atoms, people, cars, etc.) are represented as colored balls in an urn or other container.

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

nsamp(n=3,k=2,replace = TRUE, ordered = TRUE)
# Explain the grid

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
Prob(A) # 13/52
Prob(S, suit=="Heart")

Prob(B);Prob(S)

# Conditional Probablity
# S={draw a card},A ={suit = "Heart"} and B={rank is 7, 8, or 9}
Prob(A, given = B)

Prob(union(A,B))
Prob(A) + Prob(B) - Prob(intersect(A,B))

# Or perhaps the Multiplication Rule:
Prob(intersect(A,B))
Prob(A) * Prob(B, given = A)

# We could give evidence that consecutive trials of flipping a coin are independent:
#Keep in mind, however, that the point is
#not that R (or any other software package, for that matter) will ever be an effective surrogate for
#critical thinking; rather, the point is that statistical tools like R serve to change the classroom
#landscape, hopefully for the better. Plus, it's free.

# Simulating Experiments
sim(S,ntrials = 5)
sim(S,ntrials = 5)
# We only simulated the experiment 5 times in the interest of space; a person could simulate
# as many trials as desired (within reason).

S <- tosscoin(2, makespace = TRUE)
sims <- sim(S, ntrials = 50000)
empirical(sims)
#We see that each outcome has an observed relative frequency fairly close to 0.25, as we would
#expect. Of course, this doesn't prove that the sampling is perfect, but it is good news nonetheless.


# Bayes Theorem
# One who asks you what you think before a study, in order to tell you what you think afterwards
# http://www.slideshare.net/BayesLaplace1/bayesian-statistics-using-r-intro
library(Bolstad)
results <- binobp(8,20,1,1, plot = TRUE)

set.seed(1234)
y <- rnorm(29,-0.5,1)

# Find the posterior density with N(0,1) prior on mu
normnp(y,1)

# Find the posterior density with N(0.5, 3) prior on mu
normnp(y, 1, 0.5, 3)


# Normal Distributions
set.seed(100)
Sample <- data.frame(X=rnorm(1000,0.8,3))
library(ggplot2)
ggplot(Sample,aes(x=X)) + 
  geom_histogram( aes(y=..density..),breaks=seq(-10,11,by=0.5),colour="black",fill="white") +
  stat_function(fun=dnorm, args=list(mean=mean(Sample$X), sd=sd(Sample$X)))+
  labs(title="Normal Distribution",y="Probability") 
summary(Sample)


# Binomial Distributions
# Suppose we have a binomial random variable X over 10 trials, where each trial has a
# success probability of 1/2. Then we can calculate the probability of observing x = 7 by
# calling dbinom:
dbinom(7, size=20, prob=0.5) # P(X=7)
pbinom(7, size=20, prob=0.5) # P(X<=7)

tosscoin(2, makespace = TRUE)

x1 <- 3:17;dbinom(x1, 20, 0.5)
df <- data.frame(x = x1, y = dbinom(x1, 20, 0.5)) # P(X=3),P(X=4),...,P(X=17)
ggplot(df, aes(x = x, y = y)) + geom_bar(stat = "identity", col = "pink", fill = "pink") + 
  scale_y_continuous(expand = c(0.01, 0)) + xlab("x") + ylab("Density") + 
  labs(title = "dbinom(x, 20, 0.5)") + theme_bw(16, "serif") + 
  theme(plot.title = element_text(size = rel(1.2), vjust = 1.5))


# Poisson Distribution
dpois(3,2)
dpois(0:10,2)
ggplot(data.frame(x=c(0:10)), aes(x)) +
  geom_point(aes(y=dpois(x, 2)), colour="red")


# Geometric Distribution/Negative Binomial Distribution
dnbinom(10,20,prob = 0.5)
dnbinom(10:100,20,prob = 0.5)
ggplot(data.frame(x=c(10:100)), aes(x)) +
  geom_point(aes(y=dnbinom(x, 20, 0.5)), colour="red")

rgeom(10,0.5)
set.seed(15064)
ggplot(data.frame(x=c(10:100)), aes(x)) +
  geom_point(aes(y=rgeom(x, 0.5)), colour="red")


# Drop this # Discrete & Continuous Random Variables
# Drop this # Moments & Moment Generating Functions & their properties

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

# Law of Large Numbers
# http://study.com/academy/lesson/law-of-large-numbers-definition-examples-statistics.html
mean(rolldie(1, makespace = TRUE)$X1)
mean(rolldie(10, makespace = TRUE)$X1)

# Central limit thoerem
# https://www.r-bloggers.com/sampling-distributions-and-central-limit-theorem-in-r/
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
# https://www.r-bloggers.com/using-r-for-introductory-statistics-6-simulations/

# http://www.stat.ucla.edu/~nchristo/introeconometrics/introecon_central_limit_theorem.pdf

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
Sample <- read.xlsx("C:/Yashwanth/9. My Folders/Workshops/1. MIT/sample.xlsx")

mean(Sample$X1)
median(Sample$X1)
Mode <- function(num) {
  unique_num <- unique(num)
  unique_num [which.max(tabulate(match(num, unique_num )))]
}
Mode(Sample$X1)
summary(Sample)

range(Sample$X1)
var(Sample$X1)
sd(Sample$X1)

cov(Sample$X1,Sample$X2)
cor(Sample$X1,Sample$X2)


# Advanced functions
library(psych)
describe(Sample$X1) # note down the formulae


# ggplot2
# https://r4stats.wordpress.com/examples/mydata/
mydata100 <- read.xlsx("C:/Yashwanth/9. My Folders/Workshops/1. MIT/mydata100.xlsx")
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

ggplot(mydata100, aes(workshop, ..count.. ) ) +
  geom_point(stat = "bin", size = 3) + coord_flip() +
  facet_grid(gender ~ .)

ggplot(mydata100, aes(workshop, ..count..)) +
  geom_bar() +
  opts( title="Workshop Attendance" ) +
  scale_x_discrete("Statistics Package nWorkshops")

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

# https://cran.r-project.org/web/packages/googleVis/vignettes/googleVis.pdf
# https://cran.r-project.org/web/packages/googleVis/vignettes/googleVis_examples.html
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

data("Andrew");Andrew
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
# https://github.com/ramnathv/rCharts
library(devtools)
library(Rcpp)
library(rCharts)

data("HairEyeColor");HairEyeColor
hair_eye_male <- subset(as.data.frame(HairEyeColor), Sex == "Male")
n1 <- nPlot(Freq ~ Hair, group = "Eye", data = hair_eye_male, 
            type = 'multiBarChart')
n1

h1 <- Highcharts$new()
h1$chart(type = "spline")
h1$series(data = c(1, 3, 2, 4, 5, 4, 6, 2, 3, 5, NA), dashStyle = "longdash")
h1$series(data = c(NA, 4, 1, 3, 4, 2, 9, 1, 2, 3, 4), dashStyle = "shortdot")
h1$legend(symbolWidth = 80)
h1

##############################################################################################################################################################
##############################################################################################################################################################
#           END                               Graphical Visualization                          END
##############################################################################################################################################################
##############################################################################################################################################################




##############################################################################################################################################################
##############################################################################################################################################################
                                            # Regression Analysis #
##############################################################################################################################################################
##############################################################################################################################################################


# Linear Models
# Sampling
set.seed(400)
mydata100_IND <- sample(nrow(mydata100),size=round(((nrow(mydata100)/100)*70)+1,0))
mydata100_Train <- mydata100[mydata100_IND,]
mydata100_Test <- mydata100[-mydata100_IND,]

cor(mydata100_Train$pretest,mydata100_Train$posttest)
lm_fit <- lm(posttest ~ pretest, data = mydata100_Train)
summary(lm_fit)
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


# Mean Square Error
mean(lm(posttest ~ pretest, data = mydata100)$residuals^2)


# Mean absolute error
mean(abs(lm(posttest ~ pretest, data = mydata100)$residuals))


# Maximum likelihood
# https://www.r-bloggers.com/fitting-a-model-by-maximum-likelihood/
summary(lm(posttest ~ pretest, data = mydata100))
LL <- LL <- function(beta0, beta1, mu, sigma) {
  R = mydata100$posttest - mydata100$pretest * beta1 - beta0
  #
  R = suppressWarnings(dnorm(R, mu, sigma, log = TRUE))
  #
  -sum(R)
}

library(stats4)
fit <- mle(LL, start = list(beta0 = 20, beta1 = 3, mu = 0, sigma=1),
           fixed = list(mu = 0), nobs = length(y))
fit
summary(fit)


##############################################################################################################################################################
##############################################################################################################################################################
#           END                               Regression Analysis                          END
##############################################################################################################################################################
##############################################################################################################################################################




##############################################################################################################################################################
##############################################################################################################################################################
                                            # Hypothesis Testing #
##############################################################################################################################################################
##############################################################################################################################################################

# Type-1 & Type-2 error : http://support.minitab.com/en-us/minitab/17/topic-library/basic-statistics-and-graphs/hypothesis-tests/basics/type-i-and-type-ii-error/
# Testing of Hypothesis, Test Statistic, t-test
# Null Hypothesis      : Posttest score are NOT significantly better than pretest scores
# Alternate Hypothesis : Posttest score are significantly better than pretest scores
t.test(mydata100$pretest,mydata100$posttest, alternative = "less")

# Conclusion : Since p<0.05, we reject the Null Hyp & conclude that Posttest scores are
#              Significantly better than Pretest scores


# Paired t-test


  
# Chi-square
# https://www.r-bloggers.com/the-chi-squared-test-of-independence-an-example-in-both-r-and-sas/
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
# Interpretation : 1. It provides strong evidence to suggest that gender and ice cream flavour 
#                     preference are dependent or have some association.
#                  2. It provides strong evidence to suggest that men and women tend to have difference 
#                     preferences for ice cream flavours.

# F- distribution
# http://www.r-tutor.com/elementary-statistics/probability-distributions/f-distribution


##############################################################################################################################################################
##############################################################################################################################################################
#           END                               Hypothesis Testing                        END
##############################################################################################################################################################
##############################################################################################################################################################
  



##############################################################################################################################################################
##############################################################################################################################################################
                                            # Density Estimation #
##############################################################################################################################################################
##############################################################################################################################################################

# drop this: K-nearest neighbor



# K-means clustering
# https://rpubs.com/FelipeRego/K-Means-Clustering
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


# drop this: Confidence Interval for Normal distribution


##############################################################################################################################################################
##############################################################################################################################################################
#           END                               Density Estimation                          END
##############################################################################################################################################################
##############################################################################################################################################################
