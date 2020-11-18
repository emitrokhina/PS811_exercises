####Exercise 8 for PS811
###Evgeniya Mitrokhia
#15.11.2020

library(tidyverse)
library(ggplot2)

#1
USArrests <- USArrests

#2
plot(USArrests$Murder, USArrests$Assault, main = "Correlation between murder \n and assault arrests", 
     xlab = "Murder",
     ylab = "Assault")
abline(lm(Assault ~ Murder, data = USArrests), col="red")


ggplot(USArrests, aes(Murder, Assault)) + 
  labs(title = "Correlation between murder \n and assault arrests", y = "Assault",  x = "Murder") +
  geom_smooth(method = "lm") +
  geom_point()
 
#3
boxplot(USArrests$Rape,
        main = "Rape arrests",
        ylab = "Number of rape arrests")

ggplot(USArrests, aes(Rape)) + 
  labs(title = "Rape arrests",  x = "Number of rape arrests") +
  geom_boxplot()

#4
USArrests[ "State" ] <- rownames(USArrests)

barplot(USArrests$Rape, main = "Number of rape arrests per state",
        xlab = "State",
        ylab = "Number of rape arrests",
        names.arg = c(USArrests$State),
        las=2
       # cex.names = 0.5 you might want to make the text smaller so it shows all the states)

ggplot(USArrests, aes(factor(State), Rape)) + 
  geom_bar(stat = 'identity') +
  labs(title = "Number of rape arrests per state", y = "Number of rape arrests",  x = "State") +
  theme(axis.text.x = element_text(angle=90, hjust=1))

#5
hist(USArrests$UrbanPop, main = "Urban Population crime frequency", 
     xlab = "Percent of urban population")

ggplot(USArrests, aes(UrbanPop)) + 
  labs(title = "Urban Population crime frequency", y = "Frequency",  x = "Urban Population") +
  geom_histogram(binwidth = 5, color="black", fill="white")

#My project


