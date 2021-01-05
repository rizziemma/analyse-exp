#libs
install.packages('ggplot2')
install.packages('plyr')
install.packages('dplyr')
install.packages('hrbrthemes')
install.packages('fmsb')
library(ggplot2)
library(plyr)
library(dplyr)
library(hrbrthemes)
library(fmsb)

#upload data
setwd("Documents/analyse-exp/analyse-exp")
file <- read.csv("Kickstarter_data.csv", encoding="UTF-8")
str(file)
summary(file)

### Introduction ###

# figure 1
# Nombre de projets par pays donut

#count the number of projects
data <- aggregate(file$id, by=list(Country=file$country), FUN=length)
#get top 5 countries
data <- data[order(- data$x),]
country <- data[1:5,] 
#add a category for others countries
levels(country$Country) <- c(levels(country$Country), "Others")
country[6,] <- c("Others", sum(data[-(1:5),]$x))

#plot as a donut
country$x <- as.numeric(country$x)
country$fraction = country$x / sum(country$x)
country$ymax = cumsum(country$fraction)
country$ymin = c(0, head(country$ymax, n=-1))
ggplot(country, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Country)) +
  geom_rect() +
  theme_minimal() +
  coord_polar(theta="y") +
  xlim(c(2,4))



### Analyse du titre ###

#figures 2 et 3
# Pourcentage de succès selon le nombre de mots dans le titre

#calcul du pourcentage de succès par nombre de mots dans le titre
dataRadar <- ddply(file, .(name_length), summarize, rate=sum(status=="successful")*100/length(status))
#ajout des colonnes pour les valeur min et max (0% ? 100%)
dataRadar$max <- rep(100, 20)
dataRadar$min <-rep(0, 20)
df2 <- data.frame(t(dataRadar[-1])) # inversion des colonnes/lignes
colnames(df2) <- dataRadar[, 1]     # inversion des colonnes/lignes
#plot spider
radarData <- radarchart(df2[c("max", "min", "rate"),], axistype=1, caxislabels=c(0,25,50,75,100), pfcol=rgb(0.2,0.6,0.2, 0.5), title="Pourcentage de succ?s selon le nombre de mots dans le titre")


# Nombre de projets selon le nombre de mots dans le titre

#calcul du nombre de projet par nombre de mots dans le titre
datasOccurence <- aggregate(file$name_length, by=list(Length=file$name_length), FUN=length)
#ajout des colonnes pour les valeur min et max (0 ? 25000 projets)
datasOccurence$max <- rep(25000, 20)
datasOccurence$min <- rep(0, 20)
dfOc <- data.frame(t(datasOccurence[-1])) # inversion des colonnes/lignes
colnames(dfOc) <- datasOccurence[, 1]     # inversion des colonnes/lignes
#plot spider
radarData <- radarchart(dfOc[c("max", "min", "x"),], axistype=1, caxislabels=c(0,5000, 10000,15000, 20000,25000), pfcol=rgb(0.5,0.3,0.8,0.5),  title="Nombre de projets selon le nombre de mots dans le titre")




### Analyse des catégories ###

# figure 4
# Nombre de projets et succès par catégories
#nombre total de projets
data <- ddply(file, .(main_category), summarize, value=length(id), type="total projects")
#ajoute le nombre e succès
data <- rbind(data, ddply(file, .(main_category), summarize, value=sum(status=="successful"), type="successful projects"))

#bar plot
ggplot(data = data, aes(x=main_category, y=value, fill=type)) +
  geom_bar(stat="identity", position="identity") +
  coord_flip() +
  scale_fill_manual(values=c("#2de1d9", "#f78d82"))


# figure 5
# Sommes moyennes : goal / pledged
#count
data <- ddply(file, .(main_category), summarize, goal_fail=mean(goal_usd[status=="failed"]), 
              goal_success=mean(goal_usd[status=="successful"]), 
              pledged=mean(usd_pledged[status=="successful"]))

#plot lollipop chart
ggplot(data) +
  geom_segment(aes(x=main_category, xend=main_category, y=goal_success, yend=pledged), color="grey") +
  geom_point(aes(x=main_category, y=goal_fail, colour='failed goal'), size=5) +
  geom_point(aes(x=main_category, y=goal_success, colour='successful goal'), size=5) +
  geom_point(aes(x=main_category, y=pledged, colour='successful pledged'), size=5) +
  coord_flip()+
  xlab("category") +
  ylab("USD")



### Analyse de la durée des projets ###

# figure 6
# Pourcentage de succès selon la durée du projet

#recuperation des projets de moins de 60jours
#filtre quelques projets dépassant la limite avant qu'elle soit imposée
file2 <-file[file$duration<=60,]
# d?coupage par paliers de 5 jours
file2$palier <- cut(as.numeric(file2$duration), breaks=c(5*(0:12), Inf), right=FALSE, labels=c(5*(0:12)))
# calcul du nombre de projets et du pourcentage de succ?s (par palier)
paliers <- ddply(file2, .(palier), summarize, success_rate=sum(status=="successful")*100/length(status), occur=length(palier))
#plot
ggplot(data = paliers, aes(x=palier, y=success_rate, size=occur, color=occur, group=1)) +
  geom_line(size=1, color="grey") +
  geom_point(alpha=0.8) + 
  scale_size(range = c(.1, 30)) +
  ggtitle("Pourcentage de succ?s selon la dur?e du projet") +
  guides(colour = guide_legend(override.aes = list(size=7), title="Nombre de projets"), size="none") + 
  labs(x="Dur?e en jour", y="Pourcentage de r?ussite") +
  theme_ipsum()


# figures 7 à 9
# succès et nb de projets / goal

#add goal levels
data <- file
data$goal_level <- cut(data$goal_usd, breaks=c(10^(0:8), Inf), right=FALSE, labels=c(10^(0:8)))

#count projects an successes by goal level
data1 <- rbind(ddply(data[file$duration <= 15,], .(goal_level), summarize, duration="< 2 weeks", type="projects", value=length(id)),
               ddply(data[file$duration <= 15,], .(goal_level), summarize, duration="< 2 weeks", type="successes", value=sum(status=="successful")))
data2 <- rbind(ddply(data[file$duration > 15 && file$duration <= 30,], .(goal_level), summarize, duration="2-4 weeks", type="projects", value=length(id)),
               ddply(data[file$duration > 15 && file$duration <= 30,], .(goal_level), summarize, duration="2-4 weeks", type="successes", value=sum(status=="successful")))
data3 <- rbind(ddply(data[file$duration > 30,], .(goal_level), summarize, duration="> 4 weeks", type="projects", value=length(id)),
               ddply(data[file$duration > 30,], .(goal_level), summarize, duration="> 4 weeks", type="successes", value=sum(status=="successful")))

# bar plot
ggplot(data=data1, aes(x=goal_level, y=value, fill=type)) +
  geom_bar(position="identity", stat = "identity") +
  ggtitle("moins de 2 semaines")

ggplot(data=data2, aes(x=goal_level, y=value, fill=type)) +
  geom_bar(position="identity", stat = "identity")+
  ggtitle("2 à 4 semaines")

ggplot(data=data3, aes(x=goal_level, y=value, fill=type)) +
  geom_bar(position="identity", stat = "identity")+
  ggtitle("plus de 4 semaines")


