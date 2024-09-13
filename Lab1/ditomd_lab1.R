library(readr)

#exercise 1 
EPI_data <- read_csv("masters docs/fall 2024/data_analytics/Lab1/epi2024results06022024.csv")
summary(EPI_data) # stats 
attach(EPI_data) # sets the ‘default’ object 

fivenum(EPI.new,na.rm=TRUE)

stem(EPI.new) 
hist(EPI.new)
hist(EPI.new, seq(20., 80., 1.0), prob=TRUE) 
lines(density(EPI.new,na.rm=TRUE,bw=1.)) 
rug(EPI.new)

#Exercise 2A
epi2024results <- read_csv("masters docs/fall 2024/data_analytics/Lab1/epi2024results06022024.csv")
class(epi2024results)
head(epi2024results)

class(epi2024results$EPI.old) #access a column of data 
str(epi2024results)

# mean number of EPI.old
mean(epi2024results$EPI.old)
avg <- mean(epi2024results$EPI.old)

epi2024results$dev <- epi2024results$EPI.old - avg

# max number of EPI.old
summary(epi2024results$EPI.old)

#Diagrams 
fivenum(EPI.old,na.rm=TRUE)
stem(EPI.old) 
hist(EPI.old)

fivenum(ECO.old,na.rm=TRUE)
stem(ECO.old) 
hist(ECO.old)

