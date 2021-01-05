install.packages('ggplot2')
install.packages('plyr')
install.packages('dplyr')
install.packages('hrbrthemes')
library(ggplot2)
library(plyr)
library(dplyr)
library(hrbrthemes)
setwd("Documents/analyse-exp/analyse-exp")
file <- read.csv("Kickstarter_data.csv", encoding="UTF-8")
str(file)
summary(file)

######## Analyse du titre ########

##Pourcentage de succés selon le nombre de mots dans le titre

#calcul du pourcentage de succès par nombre de mots dans le titre
dataRadar <- ddply(file, .(name_length), summarize, rate=sum(status=="successful")*100/length(status))
#ajout des colonnes pour les valeur min et max (0% à 100%)
dataRadar$max <- rep(100, 20)
dataRadar$min <-rep(0, 20)
df2 <- data.frame(t(dataRadar[-1])) # inversion des colonnes/lignes
colnames(df2) <- dataRadar[, 1]     # inversion des colonnes/lignes
#plot spider
radarData <- radarchart(df2[c("max", "min", "rate"),], axistype=1, caxislabels=c(0,25,50,75,100), pfcol=rgb(0.2,0.6,0.2, 0.5), title="Pourcentage de succés selon le nombre de mots dans le titre")

##Nombre de projets selon le nombre de mots dans le titre

#calcul du nombre de projet par nombre de mots dans le titre
datasOccurence <- aggregate(file$name_length, by=list(Length=file$name_length), FUN=length)
#ajout des colonnes pour les valeur min et max (0 à 25000 projets)
datasOccurence$max <- rep(25000, 20)
datasOccurence$min <- rep(0, 20)
dfOc <- data.frame(t(datasOccurence[-1])) # inversion des colonnes/lignes
colnames(dfOc) <- datasOccurence[, 1]     # inversion des colonnes/lignes
#plot spider
radarData <- radarchart(dfOc[c("max", "min", "x"),], axistype=1, caxislabels=c(0,5000, 10000,15000, 20000,25000), pfcol=rgb(0.5,0.3,0.8,0.5),  title="Nombre de projets selon le nombre de mots dans le titre")

######## Analyse de la durée / succés ########

## Pourcentage de succés selon la durée du projet

#recuperation des projets de moins de 60jours
file2 <-file[file$duration<=60,]
# découpage par paliers de 5 jours
file2$palier <- cut(as.numeric(file2$duration), breaks=c(5*(0:12), Inf), right=FALSE, labels=c(5*(0:12)))
# calcul du nombre de projets et du pourcentage de succès (par palier)
paliers <- ddply(file2, .(palier), summarize, success_rate=sum(status=="successful")*100/length(status), occur=length(palier))
#plot
ggplot(data = paliers, aes(x=paliers$palier, y=success_rate, size=paliers$occur, color=paliers$occur, group=1)) +
  geom_line(size=1, color="grey") +
  geom_point(alpha=0.8) + 
  scale_size(range = c(.1, 30)) +
  ggtitle("Pourcentage de succés selon la durée du projet") +
  guides(colour = guide_legend(override.aes = list(size=7), title="Nombre de projets"), size="none") + 
  labs(x="Durée en jour", y="Pourcentage de réussite") +
  theme_ipsum()

