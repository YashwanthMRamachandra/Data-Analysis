

rm(list=ls())
data("iris3");iris3
data("iris");iris

iris <- subset(iris,Species=="setosa" | Species=="versicolor",names(iris))

# Data Sampling
set.seed(100)
Iris_IND  <- sample(nrow(iris),size=round(((nrow(iris)/100)*70)+1,0))
Iris_TRAIN <- iris[Iris_IND,]
Iris_TEST <- iris[-Iris_IND,]


# ir <- iris[,1:(ncol(iris)-1)]
# targets <- class.ind( c(rep("s", 50), rep("c", 50), rep("v", 50)) )
# samp <- c(sample(1:50,25), sample(51:100,25), sample(101:150,25))

Iris_TRAIN$Species <- as.factor(as.character(Iris_TRAIN$Species))
ir1 <- nnet(Iris_TRAIN[,-5], class.ind(Iris_TRAIN[,5]), size = 2, rang = 0.1,
            decay = 5e-4, maxit = 200, entropy = TRUE)

summary(ir1)
Iris_TEST$Species <- ifelse(Iris_TEST$Species=="setosa",0,1)


Iris_TEST$Predict <- predict(ir1,Iris_TEST,type = "class")


