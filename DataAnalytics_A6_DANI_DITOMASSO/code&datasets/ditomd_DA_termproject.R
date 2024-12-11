##Data Analytics Term Project Dani DiTomasso ##

install.packages("e1071")
install.packages("ggcorrplot")
install.packages("rpart")
install.packages("randomForest")
install.packages("PRROC")
install.packages("rpart.plot")

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
library(car)
library(rpart)
library(randomForest)
library(pROC)
library(PRROC)
library(rpart.plot)


setwd("~/masters docs/fall 2024/data_analytics/term project")
tendencies <- read.csv("~/masters docs/fall 2024/data_analytics/term project/manager.csv")
records <- read.csv("~/masters docs/fall 2024/data_analytics/term project/records.csv")

#cleaning the data & adding groupings
records <- records[, -c(10:12)]
records$status <- ifelse(records$W.L. > 0.53, "winning", "losing")
records <- records[-nrow(records), ]
tendencies <- tendencies[-nrow(tendencies), ]
tendencies <- tendencies[-1, ]
colnames(tendencies)  <- c("Rk", "Mgr", "Tm", "Age", "G", "Ch", "Att", "Rate", "Rate+", 
                      "Ch.1", "Att.1", "Rate.1", "Rate+.1", "Ch.2", "Att.2", "Rate.2", "Rate+.2", 
                      "PA", "IBB", "Rate.3", "Rate+.3", "PH/G", "PH/G+", "PR/G", 
                      "PR/G+", "P/G", "P/G+")
tendencies[c(8, 12, 16, 20)] <- lapply(tendencies[c(8, 12, 16, 20)], function(x) as.numeric(gsub("%", "", x)))
tendencies[4:27] <- lapply(tendencies[4:27], as.numeric)

#exploratory analysis 
breaks1 = seq(floor(min(records$W.L., na.rm = TRUE)), 
             ceiling(max(records$W.L., na.rm = TRUE)), by = .1)
hist(records$W.L., 
     breaks = breaks1, 
     prob = TRUE, 
     col = "skyblue", 
     main = "Histogram of Win-Loss% for all Managers",
     xlab = "Win-Loss Percentage")

ggplot(records, aes(x = "", fill = status)) +
  geom_bar(width = 1, stat = "count") +
  coord_polar(theta = "y") +
  labs(title = "Managers with Winning vs Losing Records for the 2024 season") +
  theme_void() +  
  theme(legend.title = element_blank())

breaks2 = seq(floor(min(records$Ejections, na.rm = TRUE)), 
              ceiling(max(records$Ejections, na.rm = TRUE)), by = 1)
hist(records$Ejections, 
     breaks = breaks2, 
     prob = TRUE, 
     col = "orange", 
     main = "Histogram of Ejections for all Managers for 2024 Season",
     xlim = c(0, 7),
     xlab = "Number of Ejections")

breaks3 = seq(floor(min(tendencies$Rate, na.rm = TRUE)), 
              ceiling(max(tendencies$Rate, na.rm = TRUE)), by = 1)
hist(tendencies$Rate, 
     breaks = breaks3, 
     prob = TRUE, 
     col = "lightpink", 
     main = "Histogram of attepts to steal second base\n divided by chances to steal in 2024",
     xlim = c(0, 20),
     xlab = "Rate Percentage (%)")

breaks4 = seq(floor(min(tendencies$Rate.1, na.rm = TRUE)), 
              ceiling(max(tendencies$Rate.1, na.rm = TRUE)), by = .5)
hist(tendencies$Rate.1, 
     breaks = breaks4, 
     prob = TRUE, 
     col = "purple", 
     main = "Histogram of attepts to steal third base\n divided by chances to steal in 2024",
     xlim = c(0, 6),
     xlab = "Rate Percentage (%)")

breaks5 = seq(floor(min(tendencies$Rate.2, na.rm = TRUE)), 
              ceiling(max(tendencies$Rate.2, na.rm = TRUE)), by = .25)
hist(tendencies$Rate.2, 
     breaks = breaks5, 
     prob = TRUE, 
     col = "red", 
     main = "Histogram of attepts to sac bunt\n divided by chances to sac bunt in 2024",
     xlim = c(0, 3),
     xlab = "Rate Percentage (%)")

breaks6 = seq(floor(min(tendencies$Rate.3, na.rm = TRUE)), 
              ceiling(max(tendencies$Rate.3, na.rm = TRUE)), by = .1)
hist(tendencies$Rate.3, 
     breaks = breaks6, 
     prob = TRUE, 
     col = "lightgreen", 
     main = "Histogram of player intentional walks divided \nby plate apperances in 2024",
     xlim = c(0, 0.6),
     xlab = "Rate Percentage (%)")

#Model 1 - Linear models for each type of data
  #NOTE: MSV = most significant variable below 0.05
tendencies <- tendencies[,-c(1, 2, 3, 5) ]
combined <- cbind(records, tendencies)
combined$status <- factor(combined$status)

lm.records <- lm(W.L. ~ Challenges + Ejections + Age + Overturned + Finish, data = combined)
summary(lm.records) #MSV "Finish"

lm.second.base <- lm(W.L. ~ Ch + Att + Rate + `Rate+`,  data = combined)
summary(lm.second.base) #MSV "Ch"

lm.third.base <- lm(W.L. ~ Ch.1 + Att.1 + Rate.1 + `Rate+.1`,  data = combined)
summary(lm.third.base) #MSV "Ch.1"

lm.sac.bunts <- lm(W.L. ~ Ch.2 + Att.2 + Rate.2 + `Rate+.2`,  data = combined)
summary(lm.sac.bunts) #MSV "Ch.2"

lm.walks <- lm(W.L. ~ PA + IBB + Rate.3 + `Rate+.3`,  data = combined)
summary(lm.walks) #MSV "IBB & Rate.3"

lm.subs <- lm(W.L. ~ `PH/G` + `PH/G+` + `PR/G` + `PR/G+` + `P/G` + `P/G+`, data = combined)
summary(lm.subs) #MSV - None

#Model 2 - Decision Tree 
#Record Data
tree.record <- rpart(W.L. ~ Challenges + Ejections + Age + Overturned + Finish, data = combined, 
              control = rpart.control(maxdepth = 5, minsplit = 10, minbucket = 5, cp = 0.01))
par(mar = c(3, 3, 3, 3))
rpart.plot(tree.record, 
           type = 3,            
           box.palette = c("red", "green"),  
           shadow.col = "gray",
           nn = TRUE)   
text(tree.record, use.n = TRUE, cex = 0.6, font = 2)
title(main = "Decision Tree for Win-Loss Predictions \n for Manager Record Data", col.main = "darkblue", font.main = 2)

#Second Base Data
tree.second.base <- rpart(W.L. ~ Ch + Att + Rate + `Rate+`, data = combined, 
                     control = rpart.control(maxdepth = 5, minsplit = 10, minbucket = 5, cp = 0.01))
par(mar = c(3, 3, 3, 3))
rpart.plot(tree.second.base, 
           type = 3,            
           box.palette = c("red", "green"),  
           shadow.col = "gray",
           nn = TRUE) 
text(tree.second.base, use.n = TRUE, cex = 0.6, font = 2)
title(main = "Decision Tree for Win-Loss Predictions \n for Second Base Data", col.main = "darkblue", font.main = 2)

#Third Base Data
tree.third.base <- rpart(W.L. ~ Ch.1 + Att.1 + Rate.1 + `Rate+.1`, data = combined, 
                          control = rpart.control(maxdepth = 5, minsplit = 10, minbucket = 5, cp = 0.01))
par(mar = c(3, 3, 3, 3))
rpart.plot(tree.third.base, 
           type = 3,            
           box.palette = c("red", "green"),  
           shadow.col = "gray",
           nn = TRUE) 
text(tree.third.base, use.n = TRUE, cex = 0.6, font = 2)
title(main = "Decision Tree for Win-Loss Predictions \n for Third Base Data", col.main = "darkblue", font.main = 2)

#Sac Bunts Data
tree.sac.bunts <- rpart(W.L. ~ Ch.2 + Att.2 + Rate.2 + `Rate+.2`, data = combined, 
                         control = rpart.control(maxdepth = 5, minsplit = 10, minbucket = 5, cp = 0.01))
par(mar = c(3, 3, 3, 3))
rpart.plot(tree.sac.bunts, 
           type = 3,            
           box.palette = c("red", "green"),  
           shadow.col = "gray",
           nn = TRUE) 
text(tree.sac.bunts, use.n = TRUE, cex = 0.6, font = 2)
title(main = "Decision Tree for Win-Loss Prediction for Sac Bunts Data", col.main = "darkblue", font.main = 2)

#Intentional Walks Data
tree.walks <- rpart(W.L. ~ PA + IBB + Rate.3 + `Rate+.3`, data = combined, 
                    control = rpart.control(maxdepth = 5, minsplit = 10, minbucket = 5, cp = 0.01))
par(mar = c(3, 3, 3, 3))
rpart.plot(tree.walks, 
           type = 3,            
           box.palette = c("red", "green"),  
           shadow.col = "gray",
           nn = TRUE) 
text(tree.walks, use.n = TRUE, cex = 0.6, font = 2)
title(main = "Decision Tree for Win-Loss Predictions \n for Intentional Walk Data", col.main = "darkblue", font.main = 2)

#Most Significant Variable Data (not including "Finish" - not baseball stat)
tree.msv <- rpart(W.L. ~ Finish+ Ch + Ch.1 + Ch.2 + IBB + Rate.3, data = combined, 
                    control = rpart.control(maxdepth = 5, minsplit = 10, minbucket = 5, cp = 0.01))
par(mar = c(3, 3, 3, 3))
rpart.plot(tree.msv, 
           type = 3,            
           box.palette = c("red", "green"),  
           shadow.col = "gray",
           nn = TRUE) 
text(tree.msv, use.n = TRUE, cex = 0.6, font = 2)
title(main = "Decision Tree for Win-Loss Predictions \n for Most Sigificant Variables (including 'Finish')", col.main = "darkblue", font.main = 2)

pred <- predict(tree.msv, newdata = combined, type = "vector")
actual <- combined$W.L.  
mse <- mean((pred - actual)^2)
print(paste("Mean Squared Error: ", mse))
rss <- sum((pred - actual)^2)  
tss <- sum((actual - mean(actual))^2)  
r_squared <- 1 - rss / tss
print(paste("R-squared: ", r_squared))

tree1.msv <- rpart(W.L. ~ Ch + Ch.1 + Ch.2 + IBB + Rate.3, data = combined, 
                  control = rpart.control(maxdepth = 5, minsplit = 10, minbucket = 5, cp = 0.01))
par(mar = c(3, 3, 3, 3))
rpart.plot(tree1.msv, 
           type = 3,            
           box.palette = c("red", "green"),  
           shadow.col = "gray",
           nn = TRUE) 
text(tree1.msv, use.n = TRUE, cex = 0.6, font = 2)
title(main = "Decision Tree for Win-Loss Predictions \n for Most Sigificant Variables (excluding 'Finish')", col.main = "darkblue", font.main = 2)

pred <- predict(tree1.msv, newdata = combined, type = "vector")
actual <- combined$W.L.  
mse <- mean((pred - actual)^2)
print(paste("Mean Squared Error: ", mse))
rss <- sum((pred - actual)^2)  
tss <- sum((actual - mean(actual))^2)  
r_squared <- 1 - rss / tss
print(paste("R-squared: ", r_squared))


#Model 3 - Random Forest for MSV
forest <- randomForest(W.L. ~ Finish + Ch + Ch.1 + Ch.2 + IBB + Rate.3, 
                         data = combined, 
                         ntree = 100,  
                         mtry = 3,     
                         importance = TRUE)
plot(forest, main = "Error Rate Plot for Random Forest")
mtext("Number of Trees", side = 1, line = 2)
mtext("Error Rate", side = 2, line = 2)
varImpPlot(forest, main = "Variable Importance Plot")

pred1 <- predict(forest, newdata = combined)
actual <- combined$W.L.  
mse <- mean((pred1 - actual)^2)
print(paste("Mean Squared Error: ", mse))
rss <- sum((pred1 - actual)^2)  
tss <- sum((actual - mean(actual))^2)  
r_squared <- 1 - rss / tss
print(paste("R-squared: ", r_squared))

#Model 4 - NaiveBayes
classifier<-naiveBayes(combined[,c(9, 18, 21, 25, 30, 31)], combined[,14]) 
pred <- predict(classifier, combined[, c(9, 18, 21, 25, 30, 31)])
cm <- table(Predicted = pred, Actual = combined[, 14])
cm_df <- as.data.frame(as.table(cm))
colnames(cm_df) <- c("Predicted", "Actual", "Count")

ggplot(cm_df, aes(x = Predicted, y = Actual, fill = Count)) +
  geom_tile() + 
  geom_text(aes(label = Count), color = "black") +  
  scale_fill_gradient(low = "white", high = "blue") +  
  labs(title = "Confusion Matrix for NaiveBayes", x = "Predicted", y = "Actual") +
  theme_minimal()

prob_pred <- predict(classifier, combined[, c(9, 18, 21, 25, 30, 31)], type = "raw")
roc_curve <- roc(combined[, 14], prob_pred[,2])  
plot(roc_curve, main = "ROC Curve", col = "blue")

r_curve <- pr.curve(scores.class0 = prob_pred[,1], scores.class1 = prob_pred[,2], curve = TRUE)
plot(pr_curve, main = "PR Curve", xlab = "Precision", ylab = "Recall")
