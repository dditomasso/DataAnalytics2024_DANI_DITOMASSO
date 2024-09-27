####### Data Analytics Fall 2024 Lab 02 ######

library(ggplot2)
library("e1071") 
library(stats)
library(class)
library(tidyverse)

### set working directory
setwd("~/masters docs\\fall 2024\\data_analytics\\lab2_2")
#iris <- iris[,-1]

## Call the NaiveBayes Classifier Package e1071, which auto calls the Class package ## 
classifier<-naiveBayes(iris[,1:5], iris[,6]) 
table(predict(classifier, iris[,-6]), iris[,6], dnn=list('predicted','actual')) 
classifier$apriori
classifier$tables$Petal.Length
plot(function(x) dnorm(x, 1.462, 0.1736640), 0, 8, col="red", main="Petal length distribution for the 3 different species") 
curve(dnorm(x, 4.260, 0.4699110), add=TRUE, col="blue") 
curve(dnorm(x, 5.552, 0.5518947 ), add=TRUE, col = "green")

##EXERCISE 1 --  naïve bayes analysis for width 
abalone <- read.csv(url("https://archive.ics.uci.edu/ml/machine-learning-databases/abalone/abalone.data"), 
                    header = FALSE, sep = ",")
colnames(abalone) <- c("sex", "length", 'diameter', 'height', 'whole_weight', 'shucked_wieght', 
                       'viscera_wieght', 'shell_weight', 'rings' )
abalone$age.group <- cut(abalone$rings, br=c(0,8,11,35), labels = c("young", 'adult', 'old')) 

classifier<-naiveBayes(abalone[,2:8], abalone[,10]) 
table(predict(classifier, abalone[,2:8]), abalone[,10], dnn=list('predicted','actual')) 
classifier$apriori
classifier$tables$length
plot(function(x) dnorm(x, 0.4209915, 0.11137474), ylim = c(0,5), 0, 1, col="red", main="Abalone length for Young, Adult, Old") 
curve(dnorm(x, 0.5707182, 0.08740980), add=TRUE, col="blue") 
curve(dnorm(x, 0.5868542, 0.08100644), add=TRUE, col = "green")

##EXERCISE 1 --  naïve bayes analysis for height
classifier<-naiveBayes(abalone[,2:4], abalone[,10]) 
table(predict(classifier, abalone[,2:4]), abalone[,10], dnn=list('predicted','actual')) 
classifier$tables$height
plot(function(x) dnorm(x, 0.1065956, 0.04183039), ylim = c(0,15), -0.5, 1, col="red", main="Abalone height for Young, Adult, Old") 
curve(dnorm(x, 0.1516906, 0.02984784), add=TRUE, col="blue") 
curve(dnorm(x, 0.5868542, 0.08100644), add=TRUE, col = "green")


##EXERCISE 1 --  naïve bayes analysis for whole weight 
classifier<-naiveBayes(abalone[,5:8], abalone[,10]) 
table(predict(classifier, abalone[,5:8]), abalone[,10], dnn=list('predicted','actual')) 
classifier$tables$whole_weight
plot(function(x) dnorm(x, 0.4323742, 0.3060074), ylim = c(0,2), -0.5, 3, col="red", main="Abalone whole_weight for Young, Adult, Old") 
curve(dnorm(x, 0.9850878, 0.4264315), add=TRUE, col="blue") 
curve(dnorm(x, 1.1148922, 0.4563715), add=TRUE, col = "green")

##EXERCISE 2 --KNN for iris (Sepal.Width)

#Run Once 
iris.train <-iris[s_iris,-1]
iris.test <-iris[-s_iris,-1]

#Continue
s_iris <- sample(150,100)
sqrt(100)
k = 10

KNNpred <- knn(train = iris.train[1], test = iris.test[1], cl = iris.train$Species, k = 10)
contingency.table <- table(KNNpred, iris.test$Species)
contingency.table

contingency.matrix = as.matrix(contingency.table)
sum(diag(contingency.matrix))/length(iris.test$Species)
accuracy <- c()
ks <- c(35,45,55,65,75,85,95,105)
for (k in ks) {
  KNNpred <- knn(train = iris.train[1], test = iris.test[1], cl = iris.train$Species, k = 10)
  cm = as.matrix(table(Actual=KNNpred, Predicted = iris.test$Species, dnn=list('predicted','actual')))
  accuracy <- c(accuracy,sum(diag(cm))/length(iris.test$Species)) 
}
plot(ks,accuracy,type = "b", ylim = c(0.65,0.9), main="Iris Sepal.Length for Species -- KNN")

##EXERCISE 2 --KNN for iris (Sepal.Width)
s_iris <- sample(150,100)
sqrt(100)
k = 10

KNNpred <- knn(train = iris.train[2], test = iris.test[2], cl = iris.train$Species, k = 10)
contingency.table <- table(KNNpred, iris.test$Species)
contingency.table

contingency.matrix = as.matrix(contingency.table)
sum(diag(contingency.matrix))/length(iris.test$Species)
accuracy <- c()
ks <- c(35,45,55,65,75,85,95,105)
for (k in ks) {
  KNNpred <- knn(train = iris.train[2], test = iris.test[2], cl = iris.train$Species, k = 10)
  cm = as.matrix(table(Actual=KNNpred, Predicted = iris.test$Species, dnn=list('predicted','actual')))
  accuracy <- c(accuracy,sum(diag(cm))/length(iris.test$Species)) 
}
plot(ks,accuracy,type = "b", ylim = c(0.4,0.5), main="Iris Sepal.Width for Species -- KNN")

##Exercise 3 -- K-Means for Iris (Sepal.Length vs Sepal.Width)
iris.km <- kmeans(iris[,-5], centers = 3)
assigned.clusters <- as.factor(iris.km$cluster)
ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, colour = assigned.clusters)) +
  geom_point()

wss <- c()
ks <- c(1,2,3,4)
for (k in ks) {
  iris.km <- kmeans(iris[,-5], centers = k)
  wss <- c(wss,iris.km$tot.withinss)
}
plot(ks,wss,type = "b")

labeled.clusters <- as.character(assigned.clusters)
labeled.clusters[labeled.clusters==1] <- "setosa"
labeled.clusters[labeled.clusters==2] <- "versivolor"
labeled.clusters[labeled.clusters==3] <- "virginica"
table(labeled.clusters, iris[,5])

##Exercise 3 -- K-Means for Abalone (Whole.Weight vs Shucked.Weight)

abalone.km <- kmeans(abalone[,-c(1,10)], centers = 3)
assigned.clusters <- as.factor(abalone.km$cluster)
ggplot(abalone, aes(x = whole_weight, y = shucked_wieght, colour = assigned.clusters)) +
  geom_point()

wss <- c()
ks <- c(3,4,5,6)
for (k in ks) {
  abalone.km <- kmeans(iris[,-5], centers = k)
  wss <- c(wss,abalone.km$tot.withinss)
}
plot(ks,wss,type = "b", main="Abalone K-Means")

labeled.clusters <- as.character(assigned.clusters)
labeled.clusters[labeled.clusters==1] <- "young"
labeled.clusters[labeled.clusters==2] <- "adult"
labeled.clusters[labeled.clusters==3] <- "old"
table(labeled.clusters, abalone[,10])

