##Assignment 3 Dani DiTomasso ##

library(ggplot2)
library(tidyverse)
library(class)

setwd("~/masters docs/fall 2024/data_analytics/DataAnalytics_A3_DANI_DITOMASSO")
covid2020<- read.csv("~/masters docs/fall 2024/data_analytics/DataAnalytics_A3_DANI_DITOMASSO/us-counties-2020.csv")
covid2021<- read.csv("~/masters docs/fall 2024/data_analytics/DataAnalytics_A3_DANI_DITOMASSO/us-counties-2021.csv")
summary(covid2020)
summary(covid2021)
#attach(covid2020)
#attach(covid2021)

#QUESTION 1 - NOTE: In order to consolidate the data set to a size my personal 
#computer can handle and run visual diagrams for, I will be analyzing 
#the Covid-19 data for 2020 and 2021 for all counties in the state of 
#New York. 
covid2020.ny <- covid2020[which(covid2020$state == "New York"),]
covid2021.ny <- covid2021[which(covid2021$state == "New York"),]


#NOTE: in addition to narrowing the dataset to just NY, outliers 
#will also be removed to have consistent and comprehesive visual 
#diagrams, using interquartile range data (https://www.statology.org/remove-outliers-r/).
covid2020.ny.cases.Q1 <- quantile(covid2020.ny$cases, .25)
covid2020.ny.cases.Q3 <- quantile(covid2020.ny$cases, .75)
covid2020.ny.cases.IQR <- IQR(covid2020.ny$cases)
covid2020.ny.cases.no_outliers <- subset(covid2020.ny, covid2020.ny$cases> (covid2020.ny.cases.Q1 - 1.5*covid2020.ny.cases.IQR) & covid2020.ny$cases < (covid2020.ny.cases.Q3 + 1.5*covid2020.ny.cases.IQR))

covid2021.ny.cases.Q1 <- quantile(covid2021.ny$cases, .25)
covid2021.ny.cases.Q3 <- quantile(covid2021.ny$cases, .75)
covid2021.ny.cases.IQR <- IQR(covid2021.ny$cases)
covid2021.ny.cases.no_outliers <- subset(covid2021.ny, covid2021.ny$cases> (covid2021.ny.cases.Q1 - 1.5*covid2021.ny.cases.IQR) & covid2021.ny$cases < (covid2021.ny.cases.Q3 + 1.5*covid2021.ny.cases.IQR))

covid2020.ny.deaths.Q1 <- quantile(covid2020.ny$deaths, .25)
covid2020.ny.deaths.Q3 <- quantile(covid2020.ny$deaths, .75)
covid2020.ny.deaths.IQR <- IQR(covid2020.ny$deaths)
covid2020.ny.deaths.no_outliers <- subset(covid2020.ny, covid2020.ny$deaths > (covid2020.ny.deaths.Q1 - 1.5*covid2020.ny.deaths.IQR) & covid2020.ny$deaths < (covid2020.ny.deaths.Q3 + 1.5*covid2020.ny.deaths.IQR))

covid2021.ny.deaths.Q1 <- quantile(covid2021.ny$deaths, .25)
covid2021.ny.deaths.Q3 <- quantile(covid2021.ny$deaths, .75)
covid2021.ny.deaths.IQR <- IQR(covid2021.ny$deaths)
covid2021.ny.deaths.no_outliers <- subset(covid2021.ny, covid2021.ny$deaths > (covid2021.ny.deaths.Q1 - 1.5*covid2021.ny.deaths.IQR) & covid2021.ny$deaths < (covid2021.ny.deaths.Q3 + 1.5*covid2021.ny.deaths.IQR))


#1a - Boxplots for the “Cases” and “Deaths” in NY from 2020 and 2021
boxplot(covid2020.ny.cases.no_outliers$cases, covid2021.ny.cases.no_outliers$cases, names=c("2020 Cases","2021 Cases"), main="Covid Cases in NY Counties in 2020 vs 2021")
boxplot(covid2020.ny.deaths.no_outliers$deaths, covid2021.ny.deaths.no_outliers$deaths, names=c("2020 Deaths","2021 Deaths"), main="Covid Deaths in NY Counties in 2020 vs 2021")

summary(covid2020.ny.cases.no_outliers$cases)
summary(covid2021.ny.cases.no_outliers$cases)
summary(covid2020.ny.deaths.no_outliers$deaths)
summary(covid2021.ny.deaths.no_outliers$deaths)


#1b - Histograms for the “Cases” and “Deaths” in NY from 2020 and 2021
  #Normal Distribution is overlayed on the histograms represented by the red line
hist(covid2020.ny.cases.no_outliers$cases, seq(min(covid2020.ny.cases.no_outliers$cases, na.rm=TRUE), max(covid2020.ny.cases.no_outliers$cases, na.rm=TRUE), 1), prob=TRUE, main="Histogram of COVID Cases within New York in 2020\n vs Normal Distribution") 
x1 <- seq(min(covid2020.ny.cases.no_outliers$cases), max(covid2020.ny.cases.no_outliers$cases), length = 100)
y1 <- dnorm(x1, mean = mean(covid2020.ny.cases.no_outliers$cases), sd = sd(covid2020.ny.cases.no_outliers$cases))
lines(x1, y1, col = "red", lwd = 2)

hist(covid2021.ny.cases.no_outliers$cases, seq(min(covid2021.ny.cases.no_outliers$cases, na.rm=TRUE), max(covid2021.ny.cases.no_outliers$cases, na.rm=TRUE), 1), prob=TRUE, main="Histogram of COVID Cases within New York in 2021\n vs Normal Distribution") 
x2 <- seq(min(covid2021.ny.cases.no_outliers$cases), max(covid2021.ny.cases.no_outliers$cases), length = 100)
y2 <- dnorm(x2, mean = mean(covid2021.ny.cases.no_outliers$cases), sd = sd(covid2021.ny.cases.no_outliers$cases))
lines(x2, y2, col = "red", lwd = 2)

hist(covid2020.ny.deaths.no_outliers$deaths, seq(min(covid2020.ny.deaths.no_outliers$deaths, na.rm=TRUE), max(covid2020.ny.deaths.no_outliers$deaths, na.rm=TRUE), 1), prob=TRUE, main="Histogram of COVID Deaths within New York in 2020\n vs Normal Distribution") 
x3 <- seq(min(covid2020.ny.deaths.no_outliers$deaths), max(covid2020.ny.deaths.no_outliers$deaths), length = 100)
y3 <- dnorm(x3, mean = mean(covid2020.ny.deaths.no_outliers$deaths), sd = sd(covid2020.ny.deaths.no_outliers$deaths))
lines(x3, y3, col = "red", lwd = 2)

hist(covid2021.ny.deaths.no_outliers$deaths, seq(min(covid2021.ny.deaths.no_outliers$deaths, na.rm=TRUE), max(covid2021.ny.deaths.no_outliers$deaths, na.rm=TRUE), 1), prob=TRUE, main="Histogram of COVID Deaths within New York in 2021\n vs Normal Distribution") 
x4 <- seq(min(covid2021.ny.deaths.no_outliers$deaths), max(covid2021.ny.deaths.no_outliers$deaths), length = 100)
y4 <- dnorm(x4, mean = mean(covid2021.ny.deaths.no_outliers$deaths), sd = sd(covid2021.ny.deaths.no_outliers$deaths))
lines(x4, y4, col = "red", lwd = 2)


#1c - ECDFs & QQ-plots for Covid "cases" and "deaths" in New York 2020 vs 2021
plot(ecdf(covid2020.ny.cases.no_outliers$cases), do.points=FALSE, main="Covid Cases in 2020 vs. 2021 within New York")
lines(ecdf(covid2021.ny.cases.no_outliers$cases))

plot(ecdf(covid2020.ny.deaths.no_outliers$deaths), do.points=FALSE, main="Covid Deaths in 2020 vs. 2021 within New York")
lines(ecdf(covid2021.ny.deaths.no_outliers$deaths))

#QQ-Plot for New York Covid cases from 2020 
qqnorm(covid2020.ny.cases.no_outliers$cases)
qqline(covid2020.ny.cases.no_outliers$cases)

x <- seq(0, 2000, 50)
qqplot(qnorm(ppoints(200)), x)
qqline(x)

qqplot(qnorm(ppoints(200)), covid2020.ny.cases.no_outliers$cases)
qqline(covid2020.ny.cases.no_outliers$cases)

qqplot(rnorm(1000),covid2020.ny.cases.no_outliers$cases)
qqline(covid2020.ny.cases.no_outliers$cases)

d1 <- rnorm(10000)
d2 <- rnorm(10000)
qqplot(d1,d2)
qqline(d1)

#QQ-Plot for New York Covid Cases from 2021
qqnorm(covid2021.ny.cases.no_outliers$cases)
qqline(covid2021.ny.cases.no_outliers$cases)

x <- seq(0, 10000, 50)
qqplot(qnorm(ppoints(200)), x)
qqline(x)

qqplot(qnorm(ppoints(200)), covid2021.ny.cases.no_outliers$cases)
qqline(covid2021.ny.cases.no_outliers$cases)

qqplot(rnorm(1000),covid2021.ny.cases.no_outliers$cases)
qqline(covid2021.ny.cases.no_outliers$cases)

d1 <- rnorm(10000)
d2 <- rnorm(10000)
qqplot(d1,d2)
qqline(d1)

#QQ-Plot for New York Covid Deaths from 2020
qqnorm(covid2020.ny.cases.no_outliers$deaths)
qqline(covid2020.ny.cases.no_outliers$deaths)

x <- seq(0, 700, 25)
qqplot(qnorm(ppoints(200)), x)
qqline(x)

qqplot(qnorm(ppoints(200)), covid2020.ny.cases.no_outliers$deaths)
qqline(covid2020.ny.cases.no_outliers$deaths)

qqplot(rnorm(1000),covid2020.ny.cases.no_outliers$deaths)
qqline(covid2020.ny.cases.no_outliers$deaths)

d1 <- rnorm(10000)
d2 <- rnorm(10000)
qqplot(d1,d2)
qqline(d1)

#QQ-Plot for New York Covid Deaths from 2021
qqnorm(covid2021.ny.cases.no_outliers$deaths)
qqline(covid2021.ny.cases.no_outliers$deaths)

x <- seq(0, 5000, 50)
qqplot(qnorm(ppoints(200)), x)
qqline(x)

qqplot(qnorm(ppoints(200)), covid2021.ny.cases.no_outliers$deaths)
qqline(covid2021.ny.cases.no_outliers$deaths)

qqplot(rnorm(1000),covid2021.ny.cases.no_outliers$deaths)
qqline(covid2021.ny.cases.no_outliers$deaths)

d1 <- rnorm(10000)
d2 <- rnorm(10000)
qqplot(d1,d2)
qqline(d1)

#QUESTION 2 - To further narrow the dataset, two counties in New York 
#were chosen in order to represent Long Island (LI), which is Nassau and Suffolk county.
covid2020.nassau.suffolk <- covid2020.ny[which(covid2020.ny$county == "Nassau"|covid2020.ny$county == "Suffolk"),]
covid2021.nassau.suffolk <- covid2021.ny[which(covid2021.ny$county == "Nassau"|covid2021.ny$county == "Suffolk"),]

#Remove outliers for new datasets
covid2020.nassau.suffolk.Q1 <- quantile(covid2020.nassau.suffolk$cases, .25)
covid2020.nassau.suffolk.Q3 <- quantile(covid2020.nassau.suffolk$cases, .75)
covid2020.nassau.suffolk.IQR <- IQR(covid2020.nassau.suffolk$cases)
covid2020.nassau.suffolk.compl <- subset(covid2020.nassau.suffolk, covid2020.nassau.suffolk$cases> (covid2020.nassau.suffolk.Q1 - 1.5*covid2020.nassau.suffolk.IQR) & covid2020.nassau.suffolk$cases < (covid2020.nassau.suffolk.Q3 + 1.5*covid2020.nassau.suffolk.IQR))

covid2021.nassau.suffolk.Q1 <- quantile(covid2021.nassau.suffolk$cases, .25)
covid2021.nassau.suffolk.Q3 <- quantile(covid2021.nassau.suffolk$cases, .75)
covid2021.nassau.suffolk.IQR <- IQR(covid2021.nassau.suffolk$cases)
covid2021.nassau.suffolk.compl <- subset(covid2021.nassau.suffolk, covid2021.nassau.suffolk$cases> (covid2021.nassau.suffolk.Q1 - 1.5*covid2021.nassau.suffolk.IQR) & covid2021.nassau.suffolk$cases < (covid2021.nassau.suffolk.Q3 + 1.5*covid2021.nassau.suffolk.IQR))

#2a - Boxplots for the “Cases” and “Deaths” in LI from 2020 and 2021
boxplot(covid2020.nassau.suffolk.compl$cases, covid2021.nassau.suffolk.compl$cases, names=c("2020 Cases","2021 Cases"), main="Covid Cases in LI Counties in 2020 vs 2021")
boxplot(covid2020.nassau.suffolk.compl$deaths, covid2021.nassau.suffolk.compl$deaths, names=c("2020 Deaths","2021 Deaths"), main="Covid Deaths in LI Counties in 2020 vs 2021")

summary(covid2020.nassau.suffolk.compl$cases)
summary(covid2021.nassau.suffolk.compl$cases)
summary(covid2020.nassau.suffolk.compl$deaths)
summary(covid2021.nassau.suffolk.compl$deaths)

#2b - Histograms for the “Cases” and “Deaths” in LI from 2020 and 2021
#Normal Distribution is overlayed on the histograms represented by the red line
hist(covid2020.nassau.suffolk.compl$cases, seq(min(covid2020.nassau.suffolk.compl$cases, na.rm=TRUE), max(covid2020.nassau.suffolk.compl$cases, na.rm=TRUE), 1), prob=TRUE, main="Histogram of COVID Cases within Long Island\n in 2020 vs Normal Distribution") 
x1 <- seq(min(covid2020.nassau.suffolk.compl$cases), max(covid2020.nassau.suffolk.compl$cases), length = 100)
y1 <- dnorm(x1, mean = mean(covid2020.nassau.suffolk.compl$cases), sd = sd(covid2020.nassau.suffolk.compl$cases))
lines(x1, y1, col = "red", lwd = 2)

hist(covid2021.nassau.suffolk.compl$cases, seq(min(covid2021.nassau.suffolk.compl$cases, na.rm=TRUE), max(covid2021.nassau.suffolk.compl$cases, na.rm=TRUE), 1), prob=TRUE, main="Histogram of COVID Cases within Long Island\n in 2021 vs Normal Distribution") 
x2 <- seq(min(covid2021.nassau.suffolk.compl$cases), max(covid2021.nassau.suffolk.compl$cases), length = 100)
y2 <- dnorm(x2, mean = mean(covid2021.nassau.suffolk.compl$cases), sd = sd(covid2021.nassau.suffolk.compl$cases))
lines(x2, y2, col = "red", lwd = 2)

hist(covid2020.nassau.suffolk.compl$deaths, seq(min(covid2020.nassau.suffolk.compl$deaths, na.rm=TRUE), max(covid2020.nassau.suffolk.compl$deaths, na.rm=TRUE), 1), prob=TRUE, main="Histogram of COVID Deaths within Long Island\n in 2020 vs Normal Distribution") 
x3 <- seq(min(covid2020.nassau.suffolk.compl$deaths), max(covid2020.nassau.suffolk.compl$deaths), length = 100)
y3 <- dnorm(x3, mean = mean(covid2020.nassau.suffolk.compl$deaths), sd = sd(covid2020.nassau.suffolk.compl$deaths))
lines(x3, y3, col = "red", lwd = 2)

hist(covid2021.nassau.suffolk.compl$deaths, seq(min(covid2021.nassau.suffolk.compl$deaths, na.rm=TRUE), max(covid2021.nassau.suffolk.compl$deaths, na.rm=TRUE), 1), prob=TRUE, main="Histogram of COVID Deaths within Long Island\n in 2021 vs Normal Distribution") 
x4 <- seq(min(covid2021.nassau.suffolk.compl$deaths), max(covid2021.nassau.suffolk.compl$deaths), length = 100)
y4 <- dnorm(x4, mean = mean(covid2021.nassau.suffolk.compl$deaths), sd = sd(covid2021.nassau.suffolk.compl$deaths))
lines(x4, y4, col = "red", lwd = 2)


#2c - ECDFs & QQ-plots for Covid "cases" and "deaths" on Long Island 2020 vs 2021
plot(ecdf(covid2020.nassau.suffolk.compl$cases), do.points=FALSE, main="Covid Cases in 2020 vs. 2021 within Long Island")
lines(ecdf(covid2021.nassau.suffolk.compl$cases))

plot(ecdf(covid2020.nassau.suffolk.compl$deaths), do.points=FALSE, main="Covid Deaths in 2020 vs. 2021 within Long Island")
lines(ecdf(covid2021.nassau.suffolk.compl$deaths))

#QQ-Plot for Long Island Covid cases from 2020 
qqnorm(covid2020.nassau.suffolk.compl$cases)
qqline(covid2020.nassau.suffolk.compl$cases)

x <- seq(0, 2000, 50)
qqplot(qnorm(ppoints(200)), x)
qqline(x)

qqplot(qnorm(ppoints(200)), covid2020.nassau.suffolk.compl$cases)
qqline(covid2020.nassau.suffolk.compl$cases)

qqplot(rnorm(1000),covid2020.nassau.suffolk.compl$cases)
qqline(covid2020.nassau.suffolk.compl$cases)

d1 <- rnorm(10000)
d2 <- rnorm(10000)
qqplot(d1,d2)
qqline(d1)

#QQ-Plot for Lond Island Covid Cases from 2021
qqnorm(covid2021.nassau.suffolk.compl$cases)
qqline(covid2021.nassau.suffolk.compl$cases)

x <- seq(0, 10000, 50)
qqplot(qnorm(ppoints(200)), x)
qqline(x)

qqplot(qnorm(ppoints(200)), covid2021.nassau.suffolk.compl$cases)
qqline(covid2021.nassau.suffolk.compl$cases)

qqplot(rnorm(1000),covid2021.nassau.suffolk.compl$cases)
qqline(covid2021.nassau.suffolk.compl$cases)

d1 <- rnorm(10000)
d2 <- rnorm(10000)
qqplot(d1,d2)
qqline(d1)

#QQ-Plot for New York Covid Deaths from 2020
qqnorm(covid2020.nassau.suffolk.compl$deaths)
qqline(covid2020.nassau.suffolk.compl$deaths)

x <- seq(0, 700, 25)
qqplot(qnorm(ppoints(200)), x)
qqline(x)

qqplot(qnorm(ppoints(200)), covid2020.nassau.suffolk.compl$deaths)
qqline(covid2020.nassau.suffolk.compl$deaths)

qqplot(rnorm(1000),covid2020.nassau.suffolk.compl$deaths)
qqline(covid2020.nassau.suffolk.compl$deaths)

d1 <- rnorm(10000)
d2 <- rnorm(10000)
qqplot(d1,d2)
qqline(d1)

#QQ-Plot for New York Covid Deaths from 2021
qqnorm(covid2021.nassau.suffolk.compl$deaths)
qqline(covid2021.nassau.suffolk.compl$deaths)

x <- seq(0, 5000, 50)
qqplot(qnorm(ppoints(200)), x)
qqline(x)

qqplot(qnorm(ppoints(200)), covid2021.nassau.suffolk.compl$deaths)
qqline(covid2021.nassau.suffolk.compl$deaths)

qqplot(rnorm(1000),covid2021.nassau.suffolk.compl$deaths)
qqline(covid2021.nassau.suffolk.compl$deaths)

d1 <- rnorm(10000)
d2 <- rnorm(10000)
qqplot(d1,d2)
qqline(d1)

#QUESTION 3

#3a - Linear Model, Scatterplot, and residuals of linear model for 
#the NY house dataset

ny.house <-  read.csv("~/masters docs/fall 2024/data_analytics/DataAnalytics_A3_DANI_DITOMASSO/NY-House-Dataset.csv")

#Remove outliers for PRICE
ny.house.price.Q1 <- quantile(ny.house$PRICE, .25)
ny.house.price.Q3 <- quantile(ny.house$PRICE, .75)
ny.house.price.IQR <- IQR(ny.house$PRICE)
ny.house.price.compl <- subset(ny.house, ny.house$PRICE> (ny.house.price.Q1 - 1.5*ny.house.price.IQR) & ny.house$PRICE < (ny.house.price.Q3 + 1.5*ny.house.price.IQR))

#Linear Model 
lin.mod.ny.house <- lm(PRICE ~ BEDS + BATH + PROPERTYSQFT, data = ny.house.price.compl)
plot(PRICE ~ BEDS + BATH + PROPERTYSQFT, data = ny.house.price.compl, main="Linear Model of NY House Data Pricing")

print(summary(lin.mod.ny.house))

#Compare BEDS and BATHS in a scatterplot
ggplot(ny.house.price.compl, aes(x = BEDS, y = BATH), main="Baths and Beds for NY Housing Units") +
  geom_point() +
  stat_smooth(method = "lm")

#Plot of Residuals of the Linear Model
ny.house.residuals <- lin.mod.ny.house$residuals
ny.house.fitted.values <- lin.mod.ny.house$fitted.values

ggplot(ny.house.price.compl, aes(x = ny.house.fitted.values, y = ny.house.residuals)) +
  geom_point() +         
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  ggtitle("Residuals of Linear Model") +
  xlab("Fitted Values") +              
  ylab("Residuals")

#3b - Criteria of new subset is the PRICE of the house is less than 500,000
ny.house.subset <-  ny.house.price.compl[which(ny.house.price.compl$PRICE < 500000),]

#Linear Model
lin.mod.ny.house.subset <- lm(PRICE ~ BEDS + BATH + PROPERTYSQFT, data = ny.house.subset)
plot(PRICE ~ BEDS + BATH + PROPERTYSQFT, data = ny.house.subset, main="Linear Model of NY House Data Pricing")

print(summary(lin.mod.ny.house.subset))

#Compare BEDS and BATHS of subset in a scatterplot 
ggplot(ny.house.subset, aes(x = BEDS, y = BATH), main="Baths and Beds for NY Housing Units under $500,000") +
  geom_point() +
  stat_smooth(method = "lm")

#Plot of Residuals of the Linear Model subset data 
ny.house.subset.residuals <- lin.mod.ny.house.subset$residuals
ny.house.subset.fitted.values <- lin.mod.ny.house.subset$fitted.values

ggplot(ny.house.subset, aes(x = ny.house.subset.fitted.values, y = ny.house.subset.residuals)) +
  geom_point() +         
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  ggtitle("Residuals of Linear Model Subset Data") +
  xlab("Fitted Values") +              
  ylab("Residuals")
