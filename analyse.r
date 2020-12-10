install.packages('ggplot2')
library(ggplot2)
install.packages('plyr')
library(plyr)
setwd("Documents/analyse-exp/analyse-exp")
file <- read.csv("Kickstarter_data.csv", encoding="UTF-8")
str(file)
summary(file)

##Analyse du titre

# Selon le nombre de mots

###### tous types de status ######
str(file)
file
datasLength <- aggregate(file$name_length, by=list(Length=file$name_length, Status=file$status), FUN=length)
datasLength
ggplot(data=datasLength, aes(x=factor(Length), y=x, fill=Status)) 
+ geom_bar(stat="identity", position=position_dodge())


# selon la longueur


# s'ils sont en majuscules

##Analyse categories
#succès par catégorie
success <- ddply(file, .(main_category), summarize, rate=sum(status=="successful")*100/length(status))
success

ggplot(data = success, aes(x=main_category, y=rate)) + 
  geom_bar(stat="identity", position=position_dodge())

#TODO
#trier + couleurs

#projets par pays -> DONUT
#succés par catégorie pour chaque pays 

