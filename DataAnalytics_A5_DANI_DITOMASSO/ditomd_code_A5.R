##Assignment 5 Dani DiTomasso ##

install.packages("e1071")

library(class)
library(ggplot2)
library(cluster)
library(tidyverse)
library(tidyr)
library(dplyr)
library(broom)
library(e1071)
library(caret)
library(ggfortify)
library(psych)


setwd("~/masters docs/fall 2024/data_analytics/DataAnalytics_A5_DANI_DITOMASSO")
nyc_data <- read.csv("~/masters docs/fall 2024/data_analytics/DataAnalytics_A5_DANI_DITOMASSO/NYC_Citywide_Annualized_Calendar_Sales_Update_20241112.csv")
summary(nyc_data)

queens_data <- nyc_data[which(nyc_data$BOROUGH == "QUEENS"),]

set.seed(123)
queens_data_subset <- queens_data %>% slice_sample(n = 1000, replace = FALSE)
nyc_data_subset <- nyc_data %>% slice_sample(n = 1000, replace = FALSE)

#Created a function to calculate Precision, Recall, and F1 to 
#eliminate repeating code
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

#1.b - Histogram for year built (exploratory data analysis)
queens_data_YB_clean <- queens_data[!is.na(queens_data$YEAR.BUILT) & 
                                      queens_data$YEAR.BUILT >= 1850 & 
                                      queens_data$YEAR.BUILT <= 2025, ]
breaks = seq(min(queens_data_YB_clean$YEAR.BUILT, na.rm = TRUE), 
             max(queens_data_YB_clean$YEAR.BUILT, na.rm = TRUE), 
             by = 1)
hist(queens_data_YB_clean$YEAR.BUILT, breaks = breaks, prob=TRUE, main="Histogram of Year Built for Buildings in Queens") 
lines(density(queens_data_YB_clean$YEAR.BUILT,na.rm=TRUE,bw=5.)) 


#1.b - Linear Model for SALE.PRICE (exploratory data analysis)
  # NOTE: To conduct a linear model, I have to narrow my dataset in order for 
  # my computer to handle the large computation without crashing - therefore 
  # I chose to do a linear model for the "ARVERNE" neighborhood 

arverne_data <- queens_data[which(queens_data$NEIGHBORHOOD == "ARVERNE"),]
arverne_data$GROSS.SQUARE.FEET <- as.numeric(arverne_data$GROSS.SQUARE.FEET)
arverne_data_clean <- arverne_data[!is.na(arverne_data$SALE.PRICE) & !is.na(arverne_data$TOTAL.UNITS) & !is.na(arverne_data$GROSS.SQUARE.FEET) & !is.na(arverne_data$TAX.CLASS.AT.TIME.OF.SALE), ]

#REMOVE OUTLIERS
arverne.TC.Q1 <- quantile(arverne_data_clean$TAX.CLASS.AT.TIME.OF.SALE, .25)
arverne.TC.Q3 <- quantile(arverne_data_clean$TAX.CLASS.AT.TIME.OF.SALE, .75)
arverne.TC.IQR <- IQR(arverne_data_clean$TAX.CLASS.AT.TIME.OF.SALE)
arverne.no_outliers <- subset(arverne_data_clean, arverne_data_clean$TAX.CLASS.AT.TIME.OF.SALE> (arverne.TC.Q1 - 1.5*arverne.TC.IQR) & arverne_data_clean$TAX.CLASS.AT.TIME.OF.SALE < (arverne.TC.Q3 + 1.5*arverne.TC.IQR))

arverne.TU.Q1 <- quantile(arverne_data_clean$TOTAL.UNITS, .25)
arverne.TU.Q3 <- quantile(arverne_data_clean$TOTAL.UNITS, .75)
arverne.TU.IQR <- IQR(arverne_data_clean$TOTAL.UNITS)
arverne.no_outliers <- subset(arverne_data_clean, arverne_data_clean$TOTAL.UNITS> (arverne.TU.Q1 - 1.5*arverne.TU.IQR) & arverne_data_clean$TOTAL.UNITS < (arverne.TU.Q3 + 1.5*arverne.TU.IQR))
 
arverne.GSF.Q1 <- quantile(arverne_data_clean$GROSS.SQUARE.FEET, .25)
arverne.GSF.Q3 <- quantile(arverne_data_clean$GROSS.SQUARE.FEET, .75)
arverne.GSF.IQR <- IQR(arverne_data_clean$GROSS.SQUARE.FEET)
arverne.no_outliers <- subset(arverne_data_clean, arverne_data_clean$GROSS.SQUARE.FEET> (arverne.GSF.Q1 - 1.5*arverne.GSF.IQR) & arverne_data_clean$GROSS.SQUARE.FEET < (arverne.GSF.Q3 + 1.5*arverne.GSF.IQR))

lin.mod.arverne.subset <- lm(SALE.PRICE ~ arverne.no_outliers$TOTAL.UNITS + arverne.no_outliers$GROSS.SQUARE.FEET + arverne.no_outliers$TAX.CLASS.AT.TIME.OF.SALE, data = arverne.no_outliers)
plot(SALE.PRICE ~ arverne.no_outliers$TOTAL.UNITS + arverne.no_outliers$GROSS.SQUARE.FEET + arverne.no_outliers$TAX.CLASS.AT.TIME.OF.SALE, data = arverne.no_outliers, main="Linear Model of Arverne, Queens Building Pricing")

print(summary(lin.mod.arverne.subset))


#1.b - Determining Outliers in SALE.PRICE for 1 Borough (Queens) 
queens.SP.Q1 <- quantile(queens_data$SALE.PRICE, .25)
queens.SP.Q3 <- quantile(queens_data$SALE.PRICE, .75)
queens.SP.IQR <- IQR(queens_data$SALE.PRICE)
queens.no_outliers <- subset(queens_data, queens_data$SALE.PRICE> (queens.SP.Q1 - 1.5*queens.SP.IQR) & queens_data$SALE.PRICE < (queens.SP.Q3 + 1.5*queens.SP.IQR))

#Scatter plots with variable outliers
ggplot(queens_data, aes(x=YEAR.BUILT, y=SALE.PRICE)) +
  geom_point() +                             
  geom_smooth(method="lm", color="red") +    
  ggtitle("Year Built vs Sale Price in Queens (with Outliers)") + 
  xlab("Year Built") + 
  ylab("Sale Price")

ggplot(queens_data, aes(x=GROSS.SQUARE.FEET, y=SALE.PRICE)) +
  geom_point() +                             
  geom_smooth(method="lm", color="red") +    
  ggtitle("Gross Square Feet vs Sale Price in Queens (with Outliers)") + 
  xlab("Gross Square Feet") + 
  ylab("Sale Price")

ggplot(queens_data, aes(x=TOTAL.UNITS, y=SALE.PRICE)) +
  geom_point() +                             
  geom_smooth(method="lm", color="red") +    
  ggtitle("Total Units vs Sale Price in Queens (with Outliers)") + 
  xlab("Total Units") + 
  ylab("Sale Price")

#Scatter plots without variable outliers
queens_data <- queens_data[!is.na(queens_data$SALE.PRICE) & !is.na(queens_data$TOTAL.UNITS) & !is.na(queens_data$GROSS.SQUARE.FEET) & !is.na(queens_data$YEAR.BUILT), ]
queens.YB.Q1 <- quantile(queens_data$YEAR.BUILT, .25)
queens.YB.Q3 <- quantile(queens_data$YEAR.BUILT, .75)
queens.YB.IQR <- IQR(queens_data$YEAR.BUILT)
queens.no_outliers <- subset(queens_data, queens_data$YEAR.BUILT> (queens.YB.Q1 - 1.5*queens.YB.IQR) & queens_data$YEAR.BUILT < (queens.YB.Q3 + 1.5*queens.YB.IQR))

queens.TU.Q1 <- quantile(queens_data$TOTAL.UNITS, .25)
queens.TU.Q3 <- quantile(queens_data$TOTAL.UNITS, .75)
queens.TU.IQR <- IQR(queens_data$TOTAL.UNITS)
queens.no_outliers <- subset(queens_data, queens_data$TOTAL.UNITS> (queens.TU.Q1 - 1.5*queens.TU.IQR) & queens_data$TOTAL.UNITS < (queens.TU.Q3 + 1.5*queens.TU.IQR))


ggplot(queens.no_outliers, aes(x=YEAR.BUILT, y=SALE.PRICE)) +
  geom_point() +                             
  geom_smooth(method="lm", color="blue") +    
  ggtitle("Year Built vs Sale Price in Queens (without Outliers)") + 
  xlab("Year Built") + 
  ylab("Sale Price")

ggplot(queens.no_outliers, aes(x=GROSS.SQUARE.FEET, y=SALE.PRICE)) +
  geom_point() +                             
  geom_smooth(method="lm", color="blue") +    
  ggtitle("Gross Square Feet vs Sale Price in Queens (without Outliers)") + 
  xlab("Gross Square Feet") + 
  ylab("Sale Price")

ggplot(queens.no_outliers, aes(x=TOTAL.UNITS, y=SALE.PRICE)) +
  geom_point() +                             
  geom_smooth(method="lm", color="blue") +    
  ggtitle("Total Units vs Sale Price in Queens (without Outliers)") + 
  xlab("Total Units") + 
  ylab("Sale Price")


#1.c - Multivariate Regression on the 1 borough dataset for Sale Price
##NOTE: To decrease the size of the dataset so my computer can run the 
##code required, I have taken a random sample of 1,000 rows from the queens
##dataset now called "queens_data_subset"

lin.mod.queens.subset <- lm(SALE.PRICE ~ TOTAL.UNITS + TAX.CLASS.AT.TIME.OF.SALE + BLOCK + LOT + ZIP.CODE + YEAR.BUILT, data = queens_data_subset)
summary(lin.mod.queens.subset)

queens_data_subset_lowSQFT <- queens_data_subset[which(queens_data_subset$GROSS.SQUARE.FEET <= 1500),]
queens_data_subset_highSQFT <- queens_data_subset[which(queens_data_subset$GROSS.SQUARE.FEET > 1500),]

lin.mod.queens.lowSQFT <- lm(SALE.PRICE ~ YEAR.BUILT + LOT + TAX.CLASS.AT.TIME.OF.SALE, data = queens_data_subset_lowSQFT)
plot(SALE.PRICE ~ YEAR.BUILT + LOT + TAX.CLASS.AT.TIME.OF.SALE, data = queens_data_subset_lowSQFT, main="Linear Model of Queens Building Sales Price based on \n Gross Square Feet <= 1500")

lin.mod.queens.highSQFT <- lm(SALE.PRICE ~ YEAR.BUILT + LOT + TAX.CLASS.AT.TIME.OF.SALE, data = queens_data_subset_highSQFT)
plot(SALE.PRICE ~ YEAR.BUILT + LOT + TAX.CLASS.AT.TIME.OF.SALE, data = queens_data_subset_highSQFT, main="Linear Model of Queens Building Sales Price based on \n Gross Square Feet > 1500")

#1.d - Comparing queens data using KNN and SVM linear kernel using precision, F1, 
##and recall values using "queens_data_subset"
queens_data_subset$TAX.CLASS.AS.OF.FINAL.ROLL <- as.numeric(queens_data_subset$TAX.CLASS.AS.OF.FINAL.ROLL)
queens_data_subset <- queens_data_subset[!is.na(queens_data_subset$YEAR.BUILT) &
                                           !is.na(queens_data_subset$TAX.CLASS.AS.OF.FINAL.ROLL) &
                                           queens_data_subset$YEAR.BUILT >= 1850 &
                                           queens_data_subset$YEAR.BUILT <= 2025, ]


dataset <- queens_data_subset[,-c(7, 30, 31)]
dataset <-dataset[!is.na(dataset$TOTAL.UNITS),]
train.indexes <- sample(749,0.7*749)

train <- dataset[train.indexes,]
test <- dataset[-train.indexes,]

 
## train SVM model - linear kernel
svm.mod <- svm(TAX.CLASS.AS.OF.FINAL.ROLL ~ YEAR.BUILT + TOTAL.UNITS + SALE.PRICE, data = train, kernel = 'linear')
print(svm.mod)
train.pred <- predict(svm.mod, train)
train.pred <- as.integer(train.pred)
train <- train %>% slice_sample(n = length(train.pred), replace = FALSE)
cm = as.matrix(table(Actual = train$TAX.CLASS.AS.OF.FINAL.ROLL, Predicted = train.pred))
metrics_svm <- calc_metrics(cm)

## train knn model 
queens_data_subset.X <- queens_data_subset[,-c(1:3, 5:13, 15,  16, 18, 19, 21:31)]
queens_data_subset.X <- queens_data_subset.X[complete.cases(queens_data_subset.X),]
principal_components <- princomp(queens_data_subset.X[,-1], cor = TRUE, score = TRUE)


# Elbow plot for determining the best K number
k_values <- 1:5
wss <- sapply(k_values, function(k) {
  kmeans(queens_data_subset.X[,-1], centers = k, nstart = 25)$tot.withinss
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
knn.pred <- knn(train = queens_data_subset.X[,-1], test = queens_data_subset.X[,-1], cl = as.factor(queens_data_subset.X$TAX.CLASS.AS.OF.FINAL.ROLL), k = k)
cm_knn <- table(Predicted=knn.pred, Actual = queens_data_subset.X$TAX.CLASS.AS.OF.FINAL.ROLL, dnn=list('predicted','actual'))
metrics_knn <- calc_metrics(cm_knn)

##Comparing Precision, F1, and Recall numbers for both CM 
all_metrics <- data.frame(
  Model = c("SVM (Linear)", "KNN (k=2)"),
  Precision = c(mean(metrics_svm$precision), mean(metrics_knn$precision)),
  Recall = c(mean(metrics_svm$recall), mean(metrics_knn$recall)),
  F1 = c(mean(metrics_svm$f1), mean(metrics_knn$f1))
)
print(all_metrics)


#2.a - Linear Model for all NYC boroughs using "nyc_data_subset"
lin.mod.nyc.subset <- lm(SALE.PRICE ~ TOTAL.UNITS + TAX.CLASS.AT.TIME.OF.SALE + BLOCK + LOT + ZIP.CODE + YEAR.BUILT, data = nyc_data_subset)
summary(lin.mod.nyc.subset)

lin.mod.nyc.SP <- lm(SALE.PRICE ~ LOT + TAX.CLASS.AT.TIME.OF.SALE, data = nyc_data_subset)
plot(SALE.PRICE ~ LOT + TAX.CLASS.AT.TIME.OF.SALE, data = nyc_data_subset, main="Linear Model of Sales Price in all 5 Boroughs")
summary(lin.mod.nyc.SP)

#2.b - Comparing nyc data using KNN and SVM linear kernel using precision, F1, 
##and recall values using "nyc_data_subset"
nyc_data_subset$TAX.CLASS.AS.OF.FINAL.ROLL <- as.numeric(nyc_data_subset$TAX.CLASS.AS.OF.FINAL.ROLL)
nyc_data_subset <- nyc_data_subset[!is.na(nyc_data_subset$YEAR.BUILT) &
                                           !is.na(nyc_data_subset$TAX.CLASS.AS.OF.FINAL.ROLL) &
                                           nyc_data_subset$YEAR.BUILT >= 1850 &
                                           nyc_data_subset$YEAR.BUILT <= 2025, ]


dataset <- nyc_data_subset[,-c(7, 30, 31)]
dataset <-dataset[!is.na(dataset$TOTAL.UNITS),]
train.indexes <- sample(726,0.7*726)

train <- dataset[train.indexes,]
test <- dataset[-train.indexes,]

## train SVM model - linear kernel
svm.mod.nyc <- svm(TAX.CLASS.AS.OF.FINAL.ROLL ~ YEAR.BUILT + TOTAL.UNITS + SALE.PRICE, data = train, kernel = 'linear')
print(svm.mod.nyc)
train.pred <- predict(svm.mod.nyc, train)
train.pred <- as.integer(train.pred)
train <- train %>% slice_sample(n = length(train.pred), replace = FALSE)
cm.nyc = as.matrix(table(Actual = train$TAX.CLASS.AS.OF.FINAL.ROLL, Predicted = train.pred))
cm.nyc <- cm.nyc[, -which(colnames(cm.nyc) == "8")]
metrics_svm <- calc_metrics(cm.nyc)

## train knn model 
nyc_data_subset.X <- nyc_data_subset[,-c(1:3, 5:13, 15,  16, 18, 19, 21:31)]
nyc_data_subset.X <- nyc_data_subset.X[complete.cases(nyc_data_subset.X),]
principal_components <- princomp(nyc_data_subset.X[,-1], cor = TRUE, score = TRUE)


# Elbow plot for determining the best K number
k_values <- 1:5
wss <- sapply(k_values, function(k) {
  kmeans(nyc_data_subset.X[,-1], centers = k, nstart = 25)$tot.withinss
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
knn.pred <- knn(train = nyc_data_subset.X[,-1], test = nyc_data_subset.X[,-1], cl = as.factor(nyc_data_subset.X$TAX.CLASS.AS.OF.FINAL.ROLL), k = k)
cm_knn.nyc <- table(Predicted=knn.pred, Actual = nyc_data_subset.X$TAX.CLASS.AS.OF.FINAL.ROLL, dnn=list('predicted','actual'))
metrics_knn <- calc_metrics(cm_knn.nyc)

##Comparing Precision, F1, and Recall numbers for both CM 
all_metrics <- data.frame(
  Model = c("SVM (Linear)", "KNN (k=2)"),
  Precision = c(mean(metrics_svm$precision), mean(metrics_knn$precision)),
  Recall = c(mean(metrics_svm$recall), mean(metrics_knn$recall)),
  F1 = c(mean(metrics_svm$f1), mean(metrics_knn$f1))
)
print(all_metrics)
