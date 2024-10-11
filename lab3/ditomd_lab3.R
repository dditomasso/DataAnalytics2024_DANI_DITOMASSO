##LAB 3 Dani DiTomasso ##

library(ggplot2)
library(tidyverse)
library(class)

setwd("~/masters docs/fall 2024/data_analytics/lab3")
epi2024results <- read.csv("~/masters docs/fall 2024/data_analytics/lab3/epi2024results_DA_F24_lab03.csv")
summary(epi2024results)
#attach(epi2024results)

#Sub Group
epi.results2024.AP <- epi2024results[which(epi2024results$region == "Asia-Pacific"),]
epi.results2024.SA <- epi2024results[which(epi2024results$region == "Southern Asia"),]

#1.1 - Histograms for BDH 
hist(epi.results2024.AP$BDH, seq(5, 65, 1), prob=TRUE, main="Histogram of BDH for Asia-Pacific Region") 
lines(density(epi.results2024.AP$BDH,na.rm=TRUE,bw=5.)) 

hist(epi.results2024.SA$BDH, seq(10, 70, 1), prob=TRUE, main="Histogram of BDH for Southern Asia Region") 
lines(density(epi.results2024.SA$BDH,na.rm=TRUE,bw=5.)) 

#1.2 - Q-Q Plot for BDH in Asia-Pacific
qqnorm(epi.results2024.AP$BDH)
qqline(epi.results2024.AP$BDH)

x <- seq(5, 65, 1)
qqplot(qnorm(ppoints(200)), x)
qqline(x)

qqplot(qnorm(ppoints(200)),epi.results2024.AP$BDH)
qqline(epi.results2024.AP$BDH)

qqplot(rnorm(250),epi.results2024.AP$BDH)
qqline(epi.results2024.AP$BDH)

d1 <- rnorm(250)
d2 <- rnorm(250)
qqplot(d1,d1)
qqline(d1)

#1.2 - Q-Q Plot for BDH in Southern Asia
qqnorm(epi.results2024.SA$BDH)
qqline(epi.results2024.SA$BDH)

x <- seq(10, 70, 1)
qqplot(qnorm(ppoints(200)), x)
qqline(x)

qqplot(qnorm(ppoints(200)),epi.results2024.SA$BDH)
qqline(epi.results2024.SA$BDH)

qqplot(rnorm(250),epi.results2024.SA$BDH)
qqline(epi.results2024.SA$BDH)

d1 <- rnorm(250)
d2 <- rnorm(250)
qqplot(d1,d1)
qqline(d1)

#2.1 - Linear Model of EPI Results 2024
lin.mod.epi2024 <- lm(EPI ~ WRS + WWG + WWC + WWT + WWR, data = epi2024results)
plot(EPI ~ WRS + WWG + WWC + WWT + WWR, data = epi2024results, main="Linear Model of EPI Results 2024")
abline(lin.mod.epi2024)

summary(lin.mod.epi2024)

ggplot(epi2024results, aes(x = WRS + WWG + WWC + WWT + WWR, y = EPI)) +
  geom_point() +
  stat_smooth(method = "lm")


## Most significant variable to EPI 
  # Based off of the p-values of each of the 5 variables (found in chart when running 
  # a summary of lin.mod.epi2024), all variables are over a p-value of 0.05 meaning none
  # of the p-values are statistically significant to EPI. However, the variable with the 
  # smallest p-value is WRS with a value of 0.363, meaning it is the most significant
  # out of the 5 variables)

ggplot(epi2024results, aes(x = WRS, y = WWG)) +
  geom_point() +          
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "WRS vs WWG with Fitted Line", 
       x = "WRS", 
       y = "WWG")

#2.1 - Linear Model of Asia-Pacific Region 
lin.mod.AP <- lm(EPI ~ WRS + WWG + WWC + WWT + WWR, data = epi.results2024.AP)
plot(EPI ~ WRS + WWG + WWC + WWT + WWR, data = epi.results2024.AP, main="Linear Model of Asia-Pacific Region")
abline(lin.mod.AP)

summary(lin.mod.AP)

ggplot(epi.results2024.AP, aes(x = WRS + WWG + WWC + WWT + WWR, y = EPI)) +
  geom_point() +
  stat_smooth(method = "lm")


## Most significant variable to Asia-Pacific Region
  # Based off of the p-values of each of the 5 variables (found in chart when running 
  # a summary of lin.mod.AP), all variables are over a p-value of 0.05 meaning none
  # of the p-values are statistically significant to EPI of the Asia-Pacific region. 
  # However, the variable with the smallest p-value is WWT with a value of 0.4906, 
  # meaning it is the most significant out of the 5 variables)

ggplot(epi.results2024.AP, aes(x = WWT, y = WWR)) +
  geom_point() +          
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "WWT vs WWR with Fitted Line", 
       x = "WWT", 
       y = "WWR")

## Which model is a better fit? 
  # The EPI model is a better fit, due to the fact that the p-values for the 5 
  # variables selected, although above the value of 0.05, are all closer to being 
  # statistically significant compared to the Asia-Pacific Region. 


#3.1 - kNN model for Asia-Pacific, Southern Asia, and Greater Middle East
epi.AP.SA.GME <- epi2024results[epi2024results$region %in% c("Asia-Pacific", "Southern Asia", "Greater Middle East"), c(1:5, 36, 37, 38, 39, 40)]
s_epi.AP.SA.GME <- sample(1:nrow(epi.AP.SA.GME), 24)  
print(s_epi.AP.SA.GME)

epi.AP.SA.GME.train <- epi.AP.SA.GME[s_epi.AP.SA.GME, ]
epi.AP.SA.GME.test <- epi.AP.SA.GME[-s_epi.AP.SA.GME, ]

#remove NA vlaues
epi.AP.SA.GME.train <- na.omit(epi.AP.SA.GME.train)
epi.AP.SA.GME.test <- na.omit(epi.AP.SA.GME.test)

k = 10


KNNpred <- knn(train = epi.AP.SA.GME.train[, -c(1:5)], test = epi.AP.SA.GME.test[, -c(1:5)],
               cl = epi.AP.SA.GME.train$region, k = k)



contingency.table <- table(KNNpred, epi.AP.SA.GME.test$region)
contingency.matrix = as.matrix(contingency.table)
accuracy <- sum(diag(contingency.matrix)) / sum(contingency.matrix)


accuracy <- c()
ks <- 1:min(nrow(epi.AP.SA.GME.train), nrow(epi.AP.SA.GME.test))
for (k in ks) {
  KNNpred <- knn(train = epi.AP.SA.GME.train[, -c(1:5)], test = epi.AP.SA.GME.test[, -c(1:5)], 
                 cl = epi.AP.SA.GME.train$region, k = k)
  
  cm = as.matrix(table(Actual = epi.AP.SA.GME.test$region, Predicted = KNNpred, dnn = list('actual', 'predicted')))
  accuracy <- c(accuracy, sum(diag(cm)) / sum(cm))
}

print(accuracy)
plot(ks, accuracy, type = "b", ylim = c(0.30, 0.70), 
     main = "KNN Accuracy vs. k Values for AP, SA, and GME Regions", 
     xlab = "k Values", ylab = "Accuracy")


#3.1 - kNN model for Global West, Eastern Europe, and Latin America & Caribbean
epi.GW.EE.LAC <- epi2024results[epi2024results$region %in% c("Global West", "Eastern Europe", "Latin America & Caribbean"), c(1:5, 36, 37, 38, 39, 40)]
s_epi.GW.EE.LAC <- sample(1:nrow(epi.GW.EE.LAC), 24)  
print(s_epi.GW.EE.LAC)

epi.GW.EE.LAC.train <- epi.GW.EE.LAC[s_epi.GW.EE.LAC, ]
epi.GW.EE.LAC.test <- epi.GW.EE.LAC[-s_epi.GW.EE.LAC, ]

#remove NA vlaues
epi.GW.EE.LAC.train <- na.omit(epi.GW.EE.LAC.train)
epi.GW.EE.LAC.test <- na.omit(epi.GW.EE.LAC.test)

k = 10

KNNpred <- knn(train = epi.GW.EE.LAC.train[, -c(1:5)], test = epi.GW.EE.LAC.test[, -c(1:5)],
               cl = epi.GW.EE.LAC.train$region, k = k)

contingency.table <- table(KNNpred, epi.GW.EE.LAC.test$region)
contingency.matrix = as.matrix(contingency.table)
accuracy <- sum(diag(contingency.matrix)) / sum(contingency.matrix)


accuracy <- c()
ks <- 1:min(nrow(epi.GW.EE.LAC.train), nrow(epi.GW.EE.LAC.test))
for (k in ks) {
  KNNpred <- knn(train = epi.GW.EE.LAC.train[, -c(1:5)], test = epi.GW.EE.LAC.test[, -c(1:5)], 
                 cl = epi.GW.EE.LAC.train$region, k = k)
  
  cm = as.matrix(table(Actual = epi.GW.EE.LAC.test$region, Predicted = KNNpred, dnn = list('actual', 'predicted')))
  accuracy <- c(accuracy, sum(diag(cm)) / sum(cm))
}

print(accuracy)
plot(ks, accuracy, type = "b", ylim = c(0.30, 0.80), 
     main = "KNN Accuracy vs. k Values for GW, EE, and LAC Regions", 
     xlab = "k Values", ylab = "Accuracy")

##3.2 Reflection 
#I believe that the second kNN model is better, due to the fact that
#there is less drastic fluctuation between points between k-values.


##4.1 - k-means clustering for Former Soviet States, Sub-Saharan Africa, and Asia-Pacific (group 1)
# and Global West, Eastern Europe, and Latin America & Caribbean (group 2)
epi.group1 <- epi2024results[epi2024results$region %in% c("Former Soviet States", "Sub-Saharan Africa", "Asia-Pacific"), c(1:5, 46, 47, 48, 49, 50)]
epi.group2 <- epi2024results[epi2024results$region %in% c("Global West", "Eastern Europe", "Latin America & Caribbean"), c(1:5, 46, 47, 48, 49, 50)]
epi.group1.km <- kmeans(epi.group1[,-c(1:5)], centers = 3)
epi.group2.km <- kmeans(epi.group2[,-c(1:5)], centers = 3)

assigned.clusters1 <- as.factor(epi.group1.km$cluster)
ggplot(epi.group1, aes(x = HPE, y = AIR, colour = assigned.clusters1)) +
  geom_point()

assigned.clusters2 <- as.factor(epi.group2.km$cluster)
ggplot(epi.group2, aes(x = HPE, y = AIR, colour = assigned.clusters2)) +
  geom_point()

#remove NA vlaues
epi.group1 <- na.omit(epi.group1)
epi.group2 <- na.omit(epi.group2)

#WWCS Value for Group 1
wss1 <- c()
ks <- c(2,3, 4, 5)
for (k in ks) {
  epi.group1.km <- kmeans(epi.group1[6:10], centers = k)
  wss1 <- c(wss1,epi.group1.km$tot.withinss)
}
plot(ks,wss1,type = "b", main="EPI Group1 2024 K-Means")

#WWCS Value for Group 2
wss2 <- c()
for (k in ks) {
  epi.group2.km <- kmeans(epi.group2[6:10], centers = k)
  wss2 <- c(wss2,epi.group2.km$tot.withinss)
}
plot(ks,wss2,type = "b", main="EPI Group2 2024 K-Means")

##In both regional groups, 3.0 is the best value for ks since it 
##is where the "elbow" of the graph is, and both groups have 3 
##different regions.
