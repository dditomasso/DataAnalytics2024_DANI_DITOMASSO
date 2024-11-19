## LAB 5 DANI DITOMASSO ##

install.packages("e1071")

library(ggfortify)
library(e1071)
library(class)
library(psych)
library(ggplot2)
library(cluster)
library(caret)


setwd("~/masters docs/fall 2024/data_analytics/lab5")
wine <- read.csv("~/masters docs/fall 2024/data_analytics/lab5/wine.data")
names(wine) <- c("Type","Alcohol","Malic acid","Ash","Alcalinity of ash","Magnesium","Total phenols","Flavanoids","Nonflavanoid Phenols","Proanthocyanins","Color Intensity","Hue","Od280/od315 of diluted wines","Proline")
nyh <- read.csv("~/masters docs/fall 2024/data_analytics/lab5/NY-House-Dataset.csv")


#function to calculate Precision, Recall, and F1 to eliminate repeating code
calc_metrics <- function(cm) {
  n = sum(cm) 
  diag = diag(cm) 
  rowsums = apply(cm, 1, sum) 
  colsums = apply(cm, 2, sum) 
  precision = diag / colsums
  recall = diag / rowsums
  f1 = 2 * precision * recall / (precision + recall)
  return(data.frame(precision, recall, f1))
}

dataset <- wine
train.indexes <- sample(177,0.7*177)

train <- dataset[train.indexes,]
test <- dataset[-train.indexes,]


## train SVM model (wine) - linear kernel
svm.mod0 <- svm(Type ~ Alcohol + `Malic acid` + Ash + `Alcalinity of ash` + Magnesium + `Total phenols`, data = train, kernel = 'linear')
svm.mod0
train.pred <- predict(svm.mod0, train)
cm0 = as.matrix(table(Actual = train$Type, Predicted = train.pred))
metrics_svm0 <- calc_metrics(cm0)


## Train SVM model (wine) - polynomial kernel
svm.mod1 <- svm(Type ~ Alcohol + `Malic acid` + Ash + `Alcalinity of ash` + Magnesium + `Total phenols`, data = train, kernel = 'polynomial')
svm.mod1
train.pred <- predict(svm.mod1, train)
cm1 = as.matrix(table(Actual = train$Type, Predicted = train.pred))
metrics_svm1 <- calc_metrics(cm1)


## Tuned SVM (wine) - polynomial
tuned.svm <- tune.svm(Type ~ Alcohol + `Malic acid` + Ash + `Alcalinity of ash` + Magnesium + `Total phenols`, data = train, kernel = 'polynomial',gamma = seq(1/2^nrow(train),1, .01), cost = 2^seq(-6, 4, 2), 
                      tunecontrol = tune.control(cross=2))
tuned.svm
svm.mod2 <- svm(Type ~ Alcohol + `Malic acid` + Ash + `Alcalinity of ash` + Magnesium + `Total phenols`, data = train, kernel = 'polynomial', gamma = 0.69, cost = .25)
svm.mod2
train.pred <- predict(svm.mod2, train)
cm2 = as.matrix(table(Actual = train$Type, Predicted = train.pred))
metrics_svm2 <- calc_metrics(cm2)


# CM for KNN (wine data)
wine.X <- wine[,-c(1, 8, 9, 10, 11, 12, 13, 14)]
principal_components <- princomp(wine.X, cor = TRUE, score = TRUE)

# Elbow plot for determining the best K number
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

k=2
knn.pred <- knn(train = wine.X, test = wine.X, cl = wine$Type, k = k)
cm3 <- table(Predicted=knn.pred, Actual = wine$Type, dnn=list('predicted','actual'))
metrics_knn <- calc_metrics(cm3)

ggplot(wine, aes(x = principal_components$scores[,1], y = principal_components$scores[,2], color = knn.pred)) +
  geom_point(size = 1.5) +
  labs(title = paste("KNN Predictions vs Actual Wine Type for first 6 Attributes (k =", k, ")"),
       x = "1st Principal Component", y = "2nd Principal Component") +
  scale_color_manual(values = c("red", "orange", "blue")) +
  theme_minimal()

##Comparing Precision, F1, and Recall numbers for all CM 
all_metrics <- data.frame(
  Model = c("SVM 0 (Linear)", "SVM 1 (Polynomial)", 
            "SVM 2 (Tuned Polynomial)", "KNN (k=2)"),
  Precision = c(mean(metrics_svm0$precision), mean(metrics_svm1$precision), 
                mean(metrics_svm2$precision), mean(metrics_knn$precision)),
  Recall = c(mean(metrics_svm0$recall), mean(metrics_svm1$recall), 
             mean(metrics_svm2$recall), mean(metrics_knn$recall)),
  F1 = c(mean(metrics_svm0$f1), mean(metrics_svm1$f1), 
         mean(metrics_svm2$f1), mean(metrics_knn$f1))
)
print(all_metrics)

## SVM regression model to predict PRICE based on Square Footage (NY-House)
dataset_nyh <- nyh
dataset_nyh <- dataset_nyh[dataset_nyh$PRICE <= 150000000, ] #remove outliers
train.indexes <- sample(4799,0.7*4799)

train_nyh <- dataset_nyh[train.indexes,]
test_nyh <- dataset_nyh[-train.indexes,]


svm.mod_nyh <- svm(PRICE ~ PROPERTYSQFT, data = train_nyh, type = "eps-regression", kernel = 'linear')
svm.mod_nyh
train.pred_nyh <- predict(svm.mod_nyh, train_nyh)
cm = as.matrix(table(Actual = train_nyh$PRICE, Predicted = train.pred_nyh))
cm

ggplot(train_nyh, aes(x = PRICE, y = train.pred_nyh)) +
  geom_point(color = "blue", size = 1.5) + 
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") + 
  labs(title = "SVM Regression: Actual vs Predicted Price \n based on PROPERTYSQFT",
       x = "Actual Price",
       y = "Predicted Price") +
  theme_minimal()


#Linear Model for NY-House
lin.mod.nyh <- lm(PRICE ~ PROPERTYSQFT, data = dataset_nyh)
print(summary(lin.mod.nyh))
predicted_values <- predict(lin.mod.nyh, newdata = dataset_nyh)

ggplot(dataset_nyh, aes(x = PRICE, y = predicted_values)) +
  geom_point(color = "blue", size = 1.5) +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +  # 45-degree line
  labs(title = "Linear Model: Actual vs Predicted Price \n based on PROPERTYSQFT",
       x = "Actual Price",
       y = "Predicted Price") +
  theme_minimal()
