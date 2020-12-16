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

ggplot(data = success, aes(x=main_category, y=rate)) + 
  geom_bar(stat="identity", position=position_dodge())

#TODO
#trier + couleurs


#projets par pays -> DONUT
data <- aggregate(file$id, by=list(Country=file$country), FUN=length)
data <- data[order(- data$x),]
country <- data[1:5,] 
levels(country$Country) <- c(levels(country$Country), "Others")
country[6,] <- c("Others", sum(data[-(1:5),]$x))
country$x <- as.numeric(country$x)
country$fraction = country$x / sum(country$x)
country$ymax = cumsum(country$fraction)
country$ymin = c(0, head(country$ymax, n=-1))
ggplot(country, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Country)) +
  geom_rect() +
  coord_polar(theta="y") +
  xlim(c(2,4))

#succés par catégorie pour chaque pays 

