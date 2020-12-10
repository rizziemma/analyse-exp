install.packages('ggplot2')
library(ggplot2)


file <- read.csv("./project/Kickstarter_projects_Feb19.csv", encoding="UTF-8")
str(file)
summary(file)

##Analyse du titre

# Selon le nombre de mots

###### tous types de status ######
str(file)
file
datasLength <- aggregate(file$name_length, by=list(Length=file$name_length, Status=file$status), FUN=length)
datasLength
ggplot(data=datasLength, aes(x=factor(Length), y=x, fill=Status)) + geom_bar(stat="identity", position=position_dodge())


# selon la longueur


# s'ils sont en majuscules

