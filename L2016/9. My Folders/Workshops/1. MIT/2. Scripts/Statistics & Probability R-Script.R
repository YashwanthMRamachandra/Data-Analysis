##############################################################################################################################################################
##############################################################################################################################################################
                                    # Foundations of Probability
##############################################################################################################################################################
##############################################################################################################################################################

# Sample Spaces/ Derive a Sample Space
install.packages("prob",dependencies = TRUE) # Install a package
library(prob) # load the package
library(combinat)
tosscoin(1) # Tossing a fair coin once
tosscoin(3) # Tossing a fair coin thrice

rolldie(1) # rolling a dice once
View(rolldie(3,nsides = 4)) # rolling a 4-sided dice thrice

View(cards()) # View all the outputs of cards

# Sampling with Urns
urnsamples(1:3,2,TRUE,TRUE) # Ordered sample with replacement
urnsamples(1:3,2,FALSE,TRUE) # Ordered sample w/o replacement

urnsamples(1:3,2,TRUE,FALSE) # Unordered sample with replacement
urnsamples(1:3,2,FALSE,FALSE) # Unordered sample w/o replacement


# Generate n-samples
nsamp(n=3,k=2,replace = TRUE, ordered = TRUE) # Generate n-samples with replacement
nsamp(n=3,k=2,replace = FALSE, ordered = TRUE)# Generate n-samples w/o replacement


# Defining Probablility Space
# Equally likely model
rolldie(1)
probspace(rolldie(1),probs = rep(1/6,times = 6)) # Probability of each outcome of rolling a dice once
probspace(1:6) # Probability of each outcome of rolling a dice once
tosscoin(2,makespace = TRUE) # Probability of each outcome of tossing a coin twice
  
probspace(tosscoin(1),probs = c(0.7,0.3)) # Fixed Probability of each outcome of tossing a coin once


# Joint & Conditional Probabilities
S <- cards(makespace = TRUE) # Define a sample space with all outcome of cards
A <- subset(S, suit == "Heart") # Define subset of cards where suit="Heart"
B <- subset(S, rank %in% 7:9) # Define subset of cards where rank = 7,8,9


# Probability
Prob(A) # Probability of event A(subset of cards where suit="Heart")
Prob(S, suit=="Heart") # Probability of event(subset of cards where suit="Heart")

Prob(B) # Probability of event B(subset of cards where rank = 7,8,9)
Prob(S) # Probability of a Sample Space


# Conditional Probablity
# S={draw a card},A ={suit = "Heart"} and B={rank is 7, 8, or 9}
Prob(A, given = B) # Probability of event A given B
Prob(intersect(A,B)) # Intersection of event A and B
Prob(B) # Probability of event B 

Prob(union(A,B)) # Union of event A and B
Prob(A) + Prob(B) - Prob(intersect(A,B)) # Property of probability events

# Multiplication Rule:
Prob(intersect(A,B)) # Intersection of event A and B
Prob(A) * Prob(B, given = A) # Probability of A multiplied by Conditional Probability of B given A


# Normal Distributions
set.seed(100) # Fix the random sampling number
Sample <- data.frame(X=rnorm(1000,0.8,3)) # Define a sample of normal distribution with n=1000, mean=0.8, var=3
library(ggplot2) # load "ggplot2" package to enable plotting

# http://docs.ggplot2.org/current/  -- Please refer the link for complete details
ggplot(Sample,aes(x=X)) + 
  geom_histogram( aes(y=..density..),breaks=seq(-10,11,by=0.5),colour="black",fill="white") + 
  stat_function(fun=dnorm, args=list(mean=mean(Sample$X), sd=sd(Sample$X)))+
  labs(title="Normal Distribution",y="Probability") 

summary(Sample) # Summary of the data/data.frame


# If N is sufficiently large
Sample <- rnorm(1000,0.8,3)
range(Sample) # Range(min-max) of Sample
Sample <- append(Sample,c(50,55,45,36,53)) # Adding outliers : appends 5-outliers to the vector
range(Sample)
Sample <- data.frame(X=Sample)

# http://docs.ggplot2.org/current/  -- Please refer the link for complete details
ggplot(Sample,aes(x=X)) + 
  geom_histogram( aes(y=..density..),breaks=seq(-10,55,by=0.5),colour="black",fill="white") +
  stat_function(fun=dnorm, args=list(mean=mean(Sample$X), sd=sd(Sample$X)))+
  labs(title="Normal Distribution",y="Probability")


library(clusterSim)
range(Sample)
Sample_norm <- data.frame(X=data.Normalization(Sample$X,type = "n1")) # Normalize the data
range(Sample_norm$X)

# http://docs.ggplot2.org/current/  -- Please refer the link for complete details
ggplot(Sample_norm,aes(x=X)) + 
  geom_histogram( aes(y=..density..),breaks=seq(-3,10,by=0.1),colour="black",fill="white") +
  stat_function(fun=dnorm, args=list(mean=mean(Sample_norm$X), sd=sd(Sample_norm$X)))+
  labs(title="Normal Distribution",y="Probability")


data("iris");iris # Load the data
iris_demo <- iris # Duplicate the data
range(iris_demo$Sepal.Length) # Range of a column or variable
iris_demo <- rbind(iris,c(45,41,34,42,"virginica")) # Add a row to the existing data
iris_demo <- as.data.frame(lapply(iris_demo[1:4],as.numeric)) # Convert variables into numeric
range(iris_demo$Sepal.Length)


# http://docs.ggplot2.org/current/  -- Please refer the link for complete details
ggplot(iris_demo,aes(x=Sepal.Length)) + 
  geom_histogram( aes(y=..density..),breaks=seq(4,48,by=1),colour="black",fill="white") +
  stat_function(fun=dnorm, args=list(mean=mean(iris_demo$Sepal.Length), sd=sd(iris_demo$Sepal.Length)))+
  labs(title="Normal Distribution",y="Probability") 

iris_norm <- data.frame(Sepal.Length=data.Normalization(iris_demo$Sepal.Length,type = "n1"))
range(iris_norm$Sepal.Length)

# http://docs.ggplot2.org/current/  -- Please refer the link for complete details
ggplot(iris_norm,aes(x=Sepal.Length)) + 
  geom_histogram( aes(y=..density..),breaks=seq(-3,13,by=0.5),colour="black",fill="white") +
  stat_function(fun=dnorm, args=list(mean=mean(iris_norm$Sepal.Length), sd=sd(iris_norm$Sepal.Length)))+
  labs(title="Normal Distribution",y="Probability") 

range(iris$Sepal.Length)
shapiro.test(iris$Sepal.Length) # Conduct Shapiro-Wilk's/Normality test
range(iris_demo$Sepal.Length)
shapiro.test(iris_demo$Sepal.Length)


# Binomial Distributions
# Suppose we have a binomial random variable X over 10 trials, where each trial has a
# success probability of 1/2. Then we can calculate the probability of observing x = 7 by
# calling dbinom:
dbinom(7, size=10, prob=0.5) # Binomial distribution with P(X=7)
pbinom(7, size=10, prob=0.5) # Binomial distribution with P(X<=7)


x1 <- 3:17 # Create a vector from 3 to 7 & assign it to object x1
dbinom(x1, 20, 0.5) # Binomial distribution with 20-trails & probability = 0.5
df <- data.frame(x = x1, y = dbinom(x1, 20, 0.5)) # Create data.frame with X & Y variables

# http://docs.ggplot2.org/current/  -- Please refer the link for complete details
ggplot(df, aes(x = x, y = y)) + 
  geom_bar(stat = "identity", col = "pink", fill = "pink") + 
  scale_y_continuous(expand = c(0.01, 0)) +
  xlab("x") + ylab("Density") + 
  labs(title = "dbinom(x, 20, 0.5)") + 
  theme_bw(16, "serif") + 
  theme(plot.title = element_text(size = rel(1.2), vjust = 1.5))


# Poisson Distribution
# The average number of homes sold by the Acme Realty 
# company is 2 homes per day. 
# What is the probability that exactly 3 homes will be 
# sold tomorrow?
dpois(3,2) # Poisson distribution with X=3 and mean=variance=2
dpois(0:10,2) # Poisson distribution with X= 0 to 10 and mean=variance=2

# http://docs.ggplot2.org/current/  -- Please refer the link for complete details
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

dnbinom(10,20,prob = 0.5) # Negative binomial with X=10, 20-trials & probability = 0.5
dnbinom(10:100,20,prob = 0.5) # Negative binomial can take values between X=10 to 100, 20-trials & probability = 0.5

# http://docs.ggplot2.org/current/  -- Please refer the link for complete details
ggplot(data.frame(x=c(10:100)), aes(x)) +
  geom_point(aes(y=dnbinom(x, 20, 0.5)), colour="red")


##############################################################################################################################################################
##############################################################################################################################################################
#           END                            Foundations of Probability                      END
##############################################################################################################################################################
##############################################################################################################################################################




##############################################################################################################################################################
##############################################################################################################################################################
                                            # Basic Statistics #
##############################################################################################################################################################
##############################################################################################################################################################

library(openxlsx) # load the package
data(iris);iris # load the data

mean(iris$Sepal.Length) # Mean/average of the Sepal length
median(iris$Sepal.Width) # Median of the Sepal length


library(modes)
v <- c(2,1,2,3,1,2,3,4,1,5,5,3,2,3) # Define a vector
modes(v);v # Mode of the vector
modes(iris$Sepal.Width) # Modes/Bi-modal of Sepal Width


summary(iris) # Summary of the data iris
range(iris$Sepal.Length) # Range of Sepal Length
var(iris$Sepal.Length) # Variance of Sepal Length
sd(iris$Sepal.Length) # Standard deviation of Sepal Length

cov(iris$Sepal.Length,iris$Sepal.Width) # Co-variance of 2-variables
cor(iris$Sepal.Length,iris$Sepal.Width) # Correlation of 2-variables

data("mtcars");mtcars
cov(mtcars$mpg,mtcars$wt)
cor(mtcars$mpg,mtcars$wt)


# Advanced functions
library(psych)
describe(iris$Sepal.Length) # Brief summary of a variable


# Visualization using ggplot2
# mydata100 <- read.xlsx("file.path/mydata100.xlsx") # Import data : Please replace "file.path" by the your actual path
mydata100 <- read.xlsx("C:/Yashwanth/9. My Folders/Workshops/1. MIT/mydata100.xlsx")
mydata100$q3 <- as.numeric(mydata100$q3) # Convert variable to numeric data-type

# http://docs.ggplot2.org/current/  -- Please refer the link for complete details
ggplot(mydata100,  aes(x = factor(""), fill = workshop) ) +
  geom_bar() # Adding bars

ggplot(mydata100,
       aes(x = factor(""), fill = workshop) ) +
  geom_bar() +
  coord_polar(theta = "y") +
  scale_x_discrete("")

ggplot(mydata100) +
  geom_bar(aes(workshop) )

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
                                            # Hypothesis Testing #
##############################################################################################################################################################
##############################################################################################################################################################

# Testing of Hypothesis, Test Statistic, t-test

# Null Hypothesis      : Posttest score are NOT significantly better 
#                          than pretest scores
# Alternate Hypothesis : Posttest score are significantly better than 
#                        pretest scores
t.test(mydata100$pretest,mydata100$posttest, alternative = "less")
# Select X & Y-variable with one-sided alternative hypothesis

# Conclusion : Since p<0.05, we reject the Null Hyp & conclude that Posttest scores are
#              Significantly better than Pretest scores



# Chi-square

# Null Hypothesis      : Icecream flavour are independent of Gender factor
# Alternate Hypothesis : Icecream flavour are dependent of Gender factor
# Entering the data into vectors
men <- c(100, 120, 60) # Random vector
women <- c(350, 200, 90)

# combining the row vectors in matrices, then converting the matrix into a data frame
ice.cream.survey <- as.data.frame(rbind(men, women))

# assigning column names to this data frame
names(ice.cream.survey) <- c('chocolate', 'vanilla', 'strawberry')
View(ice.cream.survey)

chisq.test(ice.cream.survey) # Conduct Chi-Square test
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
set.seed(400) # Fix the randomness
mydata100_IND <- sample(nrow(mydata100),size=round(((nrow(mydata100)/100)*70)+1,0)) # Create Indices for 70% of the data
mydata100_Train <- mydata100[mydata100_IND,] # Separate Train data from the Sample
mydata100_Test <- mydata100[-mydata100_IND,] # Separate Test data from the Sample


# Null Hypothesis      : There is NO significant difference between pretest & posttest scores
# Alternate Hypothesis : There is a significant difference between pretest & posttest scores
cor(mydata100_Train$pretest,mydata100_Train$posttest) # Check for Correlation
lm_fit <- lm(posttest ~ pretest, data = mydata100_Train) # Run Linear-model/Regression analysis
summary(lm_fit) # Model Summary

# Model Output
# Call:
#   lm(formula = posttest ~ pretest, data = mydata100_Train) -- Model Equation
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -10.2823  -3.9827  -0.0838   3.5479   8.9162 
# 
# Coefficients:
#                 Estimate              Std. Error  t-value     Pr(>|t|)    
# (Intercept)     17.9693(beta_not)     8.5350      2.105       0.0389 *       -- p-value
#   pretest       0.8664(beta_one)      0.1136      7.625       9.68e-11 ***   -- p-value
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 4.858(avg model-error) on 69 degrees of freedom
# Multiple R-squared:  0.4573(variation explained by the model),	Adjusted R-squared:  0.4494(Scaling difference in variables)
# F-statistic: 58.14(Test-statistic) on 1 and 69 DF,  p-value: 9.684e-11(Overall model significance)


# Conclusion : Since p<0.05, we reject Null Hypo & conclude that there is a 
#          significant differences between pretest scores & posttest scores


lm_fit$fitted.values # Extract the fitted values
plot(lm_fit) # Get the residual plots

# Identify the predicted values versus actuals
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

# http://docs.ggplot2.org/current/  -- Please refer the link for complete details
ggplot(mydata100,aes(pretest,posttest)) +
      geom_point()
ggplot(mydata100,aes(pretest,posttest)) +
      geom_point(aes(colour = as.factor(workshop)))+
      geom_abline(intercept = 18.66470, slope = 0.84561)


# Linear Models : Multiple linear regression
lm_fit_MLR <- lm(posttest ~ q1+q2+q3+q4+pretest, data = mydata100_Train) # Adding more than one variable to the equation
summary(lm_fit_MLR) # Summary of the Multiple linear regression
lm_fit_MLR$fitted.values
plot(lm_fit_MLR)
Predictions <- data.frame(Actuals = mydata100_Test$posttest,
                          Prediction=floor(predict(lm_fit,mydata100_Test,type = "response")),
                          Residuals = mydata100_Test$posttest-floor(predict(lm_fit_MLR,mydata100_Test,type = "response")))

##############################################################################################################################################################
##############################################################################################################################################################
#           END                               Regression Analysis                          END
##############################################################################################################################################################
##############################################################################################################################################################
