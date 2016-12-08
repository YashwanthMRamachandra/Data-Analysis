##############################################################################################################################################################
##############################################################################################################################################################
# Basic Statistics
##############################################################################################################################################################
##############################################################################################################################################################

# Load data
data("iris");iris

# Measure of Central Tendency
mean(iris$Sepal.Length)
median(iris$Sepal.Width)
Mode <- function(num) {
  unique_num <- unique(num)
  unique_num [which.max(tabulate(match(num, unique_num )))]
}
Mode(iris$Petal.Length)

summary(iris)
library(psych)
describe(iris)
describe(iris[1:4])


# Measure of Dispersion
range(iris$Sepal.Length)
sd(iris$Sepal.Length)
var(iris$Sepal.Width)
cov(iris$Sepal.Length,iris$Sepal.Width)
cor(iris$Sepal.Length,iris$Sepal.Width)

# Order statistics
# In OS, order assumed not to be significant but assumed significant if it is Time series.
# A similar important statistic in exploratory data analysis that is simply related to the order statistics 
# is the sample interquartile range.
IQR(iris$Sepal.Length)
quantile(iris$Sepal.Length)
#  if n = 2m+1 for some integer m, then the sample median is {\displaystyle X_{(m+1)}} X_{(m+1)} and so is 
# an order statistic. On the other hand, when n is even, n = 2m and there are two middle values, 
# {\displaystyle X_{(m)}} X_{(m)} and {\displaystyle X_{(m+1)}} X_{(m+1)}, and the sample median is 
# some function of the two (usually the average) and hence not an order statistic. Similar remarks 
# apply to all sample quantiles.


# Regression
# Linear Models
data("mtcars");mtcars;head(mtcars)
lm(mpg ~ cyl+disp+hp, data = mtcars)
summary(lm(mpg ~ cyl+disp+hp, data = mtcars))
plot(lm(mpg ~ cyl+disp+hp, data = mtcars))



# K-Nearest Neighbor
# The k-NN algorithm is among the simplest of all machine learning algorithms.
# A shortcoming of the k-NN algorithm is that it is sensitive to the local structure of the data.
# The algorithm is not to be confused with k-means, another popular machine learning technique.
data("iris3");iris3
train <- rbind(iris3[1:25,,1], iris3[1:25,,2], iris3[1:25,,3])
test <- rbind(iris3[26:50,,1], iris3[26:50,,2], iris3[26:50,,3])
cl <- factor(c(rep("s",25), rep("c",25), rep("v",25)))
library(class)
knn(train, test, cl, k = 3, prob=TRUE)
attributes(.Last.value)
