####### Data Analytics Fall 2024 Lab 02 ######

library(ggplot2)

### set working directory
setwd("~/masters docs\\fall 2024\\data_analytics\\lab2")

### read in data
epi.results <- read.csv("epi2024results06022024.csv", header=TRUE)
epi.weights <- read.csv("epi2024weights.csv")

View(epi.results)
View(epi.weights)

#### Exploratory Analysis ####

epi.results$EPI.new

epi.results[1,5]

#attach(epi.results)

EPI.new

EPI.new[1]

## NA values
na.indices <- is.na(EPI.new) 

## drop NAs
Epi.new.compl <- EPI.new[!na.indices]

## convert to data frame and add country
Epi.new.compl <- data.frame(Country = country[!na.indices], EPI = EPI.new[!na.indices])

## summary stats
summary(EPI.new)

fivenum(EPI.new,na.rm=TRUE)

## histograms
hist(EPI.new)

hist(EPI.new, seq(20., 80., 2.0), prob=TRUE)

rug(EPI.new)

lines(density(EPI.new,na.rm=TRUE,bw=1))
lines(density(EPI.new,na.rm=TRUE,bw="SJ"))

x <- seq(20., 80., 1.0)
qn<- dnorm(x,mean=42, sd=5,log=FALSE)
lines(x,0.4*qn)

qn<- dnorm(x,mean=65, sd=5,log=FALSE)
lines(x,0.12*qn)

##################

### Comparing distributions of 2 variables

boxplot(EPI.old, EPI.new, names=c("EPI.old","EPI.new"))


### Quantile-quantile plots

qqnorm(EPI.new)
qqline(EPI.new)

x <- seq(20., 80., 1.0)
qqplot(qnorm(ppoints(200)), x)
qqline(x)

qqplot(qnorm(ppoints(200)),EPI.new)
qqline(EPI.new)

qqplot(rnorm(1000),EPI.new)
qqline(EPI.new)

d1 <- rnorm(10000)
d2 <- rnorm(10000)
qqplot(d1,d1)
qqline(d1)


### Empirical Cumulative Distribution Function
plot(ecdf(EPI.new), do.points=FALSE) 

plot(ecdf(rnorm(1000, 45, 10)), do.points=FALSE, main="Norm Dist vs. EPI.new ECDF")
lines(ecdf(EPI.new))

plot(ecdf(EPI.old), do.points=FALSE, main="EPI.old vs. EPI.new ECDF")
lines(ecdf(EPI.new))


#### Populations Dataset ####

## read data
populations_2023 <- read.csv("~/masters docs\\fall 2024\\data_analytics\\lab2\\countries_populations_2023.csv")

## drop country populations that don't exist in epi results
populations <- populations_2023[-which(!populations_2023$Country %in% epi.results$country),]

## sort populations by country name
populations <- populations[order(populations$Country),]

## drop country results that don't exist in populations
epi.results.sub <- epi.results[-which(!epi.results$country %in% populations$Country),]

## sort results by country name
epi.results.sub <- epi.results.sub[order(epi.results.sub$country),]

## only keep relevant columns
epi.results.sub <- epi.results.sub[,c("country","EPI.old","EPI.new")]

## convert to mnumeric
epi.results.sub$population <- as.numeric(populations$Population)

## compute population log
epi.results.sub$population_log <- log10(epi.results.sub$population)

boxplot(epi.results.sub$population_log)

#attach(epi.results.sub)

## created linear model of EPI.new = a(population_log) + b
lin.mod.epinew <- lm(EPI.new~population_log,epi.results.sub)

plot(EPI.new~population_log, epi.results.sub)
abline(lin.mod.epinew)

summary(lin.mod.epinew)

plot(lin.mod.epinew)


ggplot(epi.results.sub, aes(x = population_log, y = EPI.new)) +
  geom_point() +
  stat_smooth(method = "lm")

ggplot(lin.mod.epinew, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  labs(title='Residual vs. Fitted Values Plot', x='Fitted Values', y='Residuals')


## another lm
lin.mod.pop <- lm(population_log~EPI.new,epi.results.sub)
plot(population_log~EPI.old)
abline(lin.mod.pop, epi.results.sub)

summary(lin.mod.pop)

plot(lin.mod.pop)


ggplot(epi.results.sub, aes(x = EPI.old, y = population_log)) +
  geom_point() +
  stat_smooth(method = "lm")

ggplot(lin.mod.pop, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  labs(title='Residual vs. Fitted Values Plot', x='Fitted Values', y='Residuals')

#EXERCISE 2 Dani DiTomasso
#Boxplot
boxplot(epi.results.sub$population_log, epi.results.sub$EPI.old, epi.results.sub$EPI.new)
boxplot(epi.results.sub$population_log, epi.results.sub$EPI.old, epi.results.sub$EPI.new, names=c('population_log', 'EPI.old', 'EPI.new'))

#Q-Q Plot 1
qqnorm(EPI.old)
qqline(EPI.old)

x <- seq(20., 80., 1.0)
qqplot(qnorm(ppoints(200)), x)
qqline(x)

qqplot(qnorm(ppoints(200)),EPI.old)
qqline(EPI.old)

qqplot(rnorm(1000),EPI.old)
qqline(EPI.old)

d1 <- rnorm(10000)
d2 <- rnorm(10000)
qqplot(d1,d1)
qqline(d1)

#Q-Q Plot 2
qqnorm(epi.results.sub$population)
qqline(epi.results.sub$population)

x <- seq(20., 80., 1.0)
qqplot(qnorm(ppoints(200)), x)
qqline(x)

qqplot(qnorm(ppoints(200)),epi.results.sub$population)
qqline(epi.results.sub$population)

qqplot(rnorm(1000),epi.results.sub$population)
qqline(epi.results.sub$population)

d1 <- rnorm(10000)
d2 <- rnorm(10000)
qqplot(d1,d1)
qqline(d1)

#Q-Q Plot 3
qqnorm(epi.results.sub$population_log)
qqline(epi.results.sub$population_log)

x <- seq(20., 80., 1.0)
qqplot(qnorm(ppoints(200)), x)
qqline(x)

qqplot(qnorm(ppoints(200)),epi.results.sub$population_log)
qqline(epi.results.sub$population_log)

qqplot(rnorm(1000),epi.results.sub$population_log)
qqline(epi.results.sub$population_log)

d1 <- rnorm(10000)
d2 <- rnorm(10000)
qqplot(d1,d1)
qqline(d1)

#ECDF 1
plot(ecdf(rnorm(1000, 45, 10)), do.points=FALSE, main="Norm Dist vs. ECO.new ECDF")
lines(ecdf(ECO.new))

#ECDF 2
plot(ecdf(ECO.old), do.points=FALSE, main="ECO.old vs. ECO.new ECDF")
lines(ecdf(ECO.new))

#ECDF 3
plot(ecdf(BDH.old), do.points=FALSE, main="BCH.old vs. BCH.new ECDF")
lines(ecdf(BDH.new))

#Summary stats 1 
lin.mod.epiold <- lm(EPI.old~population_log,epi.results.sub)

plot(EPI.old~population_log, epi.results.sub)
abline(lin.mod.epiold)

summary(lin.mod.epiold)

plot(lin.mod.epiold)

#Summary stats 2
lin.mod.pop <- lm(population~population_log,epi.results.sub)

plot(population~population_log, epi.results.sub)
abline(lin.mod.pop)

summary(lin.mod.pop)

plot(lin.mod.pop)

#Summary stats 3
lin.mod.epi <- lm(EPI.old~EPI.new,epi.results)

plot(EPI.old~EPI.new, epi.results)
abline(lin.mod.epi)

summary(lin.mod.epi)

plot(lin.mod.epi)
