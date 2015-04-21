# Data-Analysis
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
#                                         Logistic Regression
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------

#---------------------------------------------------------------------------------------------------------------
#                                         Data and data types
#---------------------------------------------------------------------------------------------------------------

Results <- read.csv("http://www.ats.ucla.edu/stat/data/binary.csv")

data(Results);Results
str(Results)
Results$rank <- as.factor(Results$rank)
Results$admit <- as.factor(Results$admit)
  
#---------------------------------------------------------------------------------------------------------------
#                                       Descriptive Statistics
#---------------------------------------------------------------------------------------------------------------

summary(Results[,sapply(Results,is.numeric)])

#---------------------------------------------------------------------------------------------------------------
#                                       Distribution of Variables : Histogram
#---------------------------------------------------------------------------------------------------------------

par(mfrow=c(2,1),mar=c(3,2,2,1))
hist(Results$gre,prob=T,xlab="gre",main="gre distribution")
curve(dnorm(x,mean=mean(Results$gre),sd=sd(Results$gre)),add=TRUE)
hist(log(Results$gre),prob=T,xlab="log(gre)",main="log(gre) distribution")
curve(dnorm(x,mean=mean(log(Results$gre)),sd=sd(log(Results$gre))),add=TRUE)

par(mfrow=c(2,1),mar=c(3,2,2,1))
hist(Results$gpa,prob=T,xlab="gpa",main="gpa distribution")
curve(dnorm(x,mean=mean(Results$gpa),sd=sd(Results$gpa)),add=TRUE)
hist(log(Results$gpa),prob=T,xlab="log(gpa)",main="log(gpa) distribution")
curve(dnorm(x,mean=mean(log(Results$gpa)),sd=sd(log(Results$gpa))),add=TRUE)

table(Results$admit);table(Results$rank)'table(Results$admit,Results$rank)'

#---------------------------------------------------------------------------------------------------------------
#                                        BootStrapping/Data Sampling
#---------------------------------------------------------------------------------------------------------------

# Set up the non-parametric bootstrap
Results$admit <- ifelse(Results$admit==1,0,1)
library(boot)
logit.bootstrap <- function(data, indices) {
  
  d <- data[indices, ]
  fit <- glm(admit ~ gre + gpa + rank, data = d, family = "binomial")
  
  return(coef(fit))
}

set.seed(12345) # seed for the RNG to ensure that you get exactly the same results as here

logit.boot <- boot(data=Results, statistic=logit.bootstrap, R=10000) # 10'000 samples


# Data Sampling  ##
set.seed(400)
Results_IND <- sample(nrow(Results),size=round(((nrow(Results)/100)*70)+1,0))

Results_TRAIN <- Results[Results_IND,]

Results_TEST <- Results[-Results_IND,]

#---------------------------------------------------------------------------------------------------------------
#                                         Model building
#---------------------------------------------------------------------------------------------------------------

My_Logit <- glm(admit ~ ., data=Results_TRAIN,family=binomial)
summary(My_Logit)
#Interpretation : For one unit change in gre, log odds of admit will increases by 0.0019
#                 For one unit change in gpa, log odds of admit will increases by 0.984439
#                 For rank2, log odds of admit will increases by -0.831731
#                 For rank3, log odds of admit will increases by -1.323668
#                 For rank4, log odds of admit will increases by -1.756235

#---------------------------------------------------------------------------------------------------------------
#                                         Odds ratio and CI
#---------------------------------------------------------------------------------------------------------------

exp(cbind(OR = coef(My_Logit), confint(My_Logit)))

#Interpretation : 
# For one unit increase in gre, odds of admition to graduate school will increases by 1.00196667/0.001%
# For one unit increase in gpa, odds of admition to gpa school will increases by 2.67630890/167%
# For rank2, odds of admition to rank2 school will increases by 0.43529531/43%
# For rank3, odds of admition to rank3 school will increases by 0.26615726/27%
# For rank4, odds of admition to rank4 school will increases by 0.17269375/17%

#---------------------------------------------------------------------------------------------------------------
#                                         Predictions
#---------------------------------------------------------------------------------------------------------------

predict(My_Logit,newdata=Results_TEST,type="response")
Final <- data.frame(Results_TEST,actual=Results_TEST$admit,
            prob=round(predict(My_Logit,newdata=Results_TEST,type="response"),2),row.names=NULL)
Final$OddsRatio <- round(Final$prob/(1-Final$prob),2)
Final$Odds <- round(ifelse(Final$OddsRatio<=1,Final$OddsRatio*100,(Final$OddsRatio*100)-100),2)
predict(My_Logit,newdata=Results_TEST,type="terms")

# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
# ----------------- END ----------------- Logistic Regression ----------------- END ----------------------------
# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
