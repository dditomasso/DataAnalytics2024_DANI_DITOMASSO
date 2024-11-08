##LAB 4 Dani DiTomasso ##

install.packages("caret")

library(ggfortify)
library(e1071)
library(class)
library(psych)
library(ggplot2)
library(cluster)
library(caret)

setwd("~/masters docs/fall 2024/data_analytics/lab4")
wine <- read.csv("~/masters docs/fall 2024/data_analytics/lab4/wine.data")
names(wine) <- c("Type","Alcohol","Malic acid","Ash","Alcalinity of ash","Magnesium","Total phenols","Flavanoids","Nonflavanoid Phenols","Proanthocyanins","Color Intensity","Hue","Od280/od315 of diluted wines","Proline")
head(wine)

summary(wine)

wine$Type <- as.factor(wine$Type)
wine <- wine[,-c(4,5,10)]
pairs.panels(wine[,-1],gap = 0,bg = c("red", "yellow", "blue")[wine$Type],pch=21)


## Compute the PCs and plot the dataset using the 1st and 2nd PC.
wine.X <- (wine[,-c(1)])
principal_components <- princomp(wine.X, cor = TRUE, score = TRUE)
summary(principal_components)

autoplot(principal_components, data = wine, colour = 'Type',
         loadings = TRUE, loadings.colour = 'blue',
         loadings.label = TRUE, loadings.label.size = 3, main = "1st and 2nd PCs")

principal_components$loadings


## Identify the variables that contribute the most to the 1st PC.
  ## IN WRITTEN REPORT ##


# NOTE: Code to determine appropriate K-value (elbow plot)
k_values <- 1:5
wss <- sapply(k_values, function(k) {
  kmeans(wine.X, centers = k, nstart = 25)$tot.withinss
})

wss_df <- data.frame(K = k_values, WSS = wss)

ggplot(wss_df, aes(x = K, y = WSS)) +
  geom_line() +
  geom_point() +
  labs(title = "Elbow Method for Optimal K",
       x = "Number of Clusters (K)",
       y = "Total Within Sum of Squares (WSS)") +
  theme_minimal()


## Train a classifier model to predict wine type using the 13 attributes
k=2
knn.pred <- knn(train = wine[,-1], test = wine[,-1], cl = wine$Type, k = k)
cm <- table(Predicted=knn.pred, Actual = wine$Type, dnn=list('predicted','actual'))
cm

ggplot(wine, aes(x = principal_components$scores[,1], y = principal_components$scores[,2], color = knn.pred)) +
  geom_point(size = 1.5) +
  labs(title = paste("KNN Predictions vs Actual Wine Type for 13 Attributes (k =", k, ")"),
       x = "1st Principal Component", y = "2nd Principal Component") +
  scale_color_manual(values = c("red", "orange", "blue")) +
  theme_minimal()


## Train a classifier model to predict wine type using the data projected into the first 3 PCs
k=2
knn.pred1 <- knn(train = principal_components$scores[,1:3], test = principal_components$scores[,1:3], cl = wine$Type, k = k)
cm1 <- table(Predicted=knn.pred1, Actual = wine$Type, dnn=list('predicted','actual'))
cm1

ggplot(principal_components$scores[,1:3], aes(x = principal_components$scores[,1], y = principal_components$scores[,2], color = knn.pred1)) +
  geom_point(size = 1.5) +
  labs(title = paste("KNN Predictions vs Actual Wine Type for first 3 PCs (k =", k, ")"),
       x = "1st Principal Component", y = "2nd Principal Component") +
  scale_color_manual(values = c("red", "orange", "blue")) +
  theme_minimal()


## Drop least contributing to 1st PC and rerun PCA 
  ## PART OF ANSWER IN WRITTEN REPORT ##
wine.X1 <- (wine[,-c(1, 3, 7, 8)])
principal_components_sig <- princomp(wine.X1, cor = TRUE, score = TRUE)
summary(principal_components_sig)

autoplot(principal_components_sig, data = wine[,-c(3,7,8)], colour = 'Type',
         loadings = TRUE, loadings.colour = 'blue',
         loadings.label = TRUE, loadings.label.size = 3, main="PCA with least sigificant variables dropped")

principal_components_sig$loadings


## Train a classifier model to predict wine type using the data projected into the first 3 PCs 
## after rerunning PCA.
k=2
knn.pred_sig <- knn(train = principal_components_sig$scores[,1:3], test = principal_components_sig$scores[,1:3], cl = wine$Type, k = k)
cm_sig <- table(Predicted=knn.pred_sig, Actual = wine$Type, dnn=list('predicted','actual'))
cm_sig

ggplot(principal_components_sig$scores[,1:3], aes(x = principal_components_sig$scores[,1], y = principal_components_sig$scores[,2], color = knn.pred_sig)) +
  geom_point(size = 1.5) +
  labs(title = paste("KNN Predictions vs Actual Wine Type for first 3 PCs \n with Least Sigificant Values Dropped (k =", k, ")"),
       x = "1st Principal Component", y = "2nd Principal Component") +
  scale_color_manual(values = c("red", "orange", "blue")) +
  theme_minimal()


## Compare the 3 classification models using contingency tables and prevision/recall/f1 
## metrics

# Model 1 (13 attributes)
cm_model1 <- confusionMatrix(cm)
cm_model1$byClass[, c("Precision", "Recall", "F1")]

# Model 2 (1st 3 PCs)
cm_model2 <- confusionMatrix(cm1)
cm_model2$byClass[, c("Precision", "Recall", "F1")]

# Model 3 (1st 3 PCs with least significant values dropped)
cm_model3 <- confusionMatrix(cm_sig)
cm_model3$byClass[, c("Precision", "Recall", "F1")]
