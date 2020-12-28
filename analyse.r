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
ggplot(data=datasLength, aes(x=factor(Length), y=x, fill=Status)) + geom_bar(stat="identity", position=position_dodge())

### nombre de mots moyen pour le succès
dataSuccess <- file[file$status=="successful",]
averageLength <- mean(dataSuccess$name_length)
averageLength

### % succès ou échec par nombre de mots

# % success
my_plot_s = function(data){
  data <- ddply(data, .(name_length), summarize, rate=sum(status=="successful")*100/length(status))
  ggplot(data = data, aes(x=name_length, y=rate, fill=name_length)) + 
    geom_bar(stat="identity", position=position_dodge())
}
my_plot_s(file)
# % failed
my_plot_f = function(data){
  data <- ddply(data, .(name_length), summarize, rate=sum(status=="failed")*100/length(status))
  ggplot(data = data, aes(x=name_length, y=rate, fill=name_length)) + 
    geom_bar(stat="identity", position=position_dodge())
}
my_plot_f(file)
##PLOT RADAR -- perc success en fonction du nombre de mot
install.packages('fmsb')
install.packages('dplyr')
library(dplyr)
library(fmsb)
max_min <- data.frame (
  max = c(100, 100, 100, 100, 100, 100, 100, 100, 100, 100, 100, 100, 100, 100, 100, 100, 100, 100, 100, 100),
  min = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
)

dataRadar <- ddply(file, .(name_length), summarize, rate=sum(status=="successful")*100/length(status))
dataRadar$max <- max
dataRadar$min <-min
df2 <- data.frame(t(dataRadar[-1])) # inversion des colonnes/lignes
colnames(df2) <- dataRadar[, 1]     # inversion des colonnes/lignes
sum(df2)
df2 %>% slice(match(c("max", "min", "rate"), 1))
radarData <- radarchart(df2[c("max", "min", "rate"),], axistype=1, caxislabels=c(0,25,50,75,100))

# selon la longueur


# s'ils sont en majuscules

##Analyse categories
#succÃ¨s par catÃ©gorie
plot_success = function(data){
  data <- ddply(data, .(main_category), summarize, rate=sum(status=="successful")*100/length(status))
  #success <- success[order(-success$rate),]
  ggplot(data = data, aes(x=main_category, y=rate, fill=main_category)) + 
    geom_bar(stat="identity", position=position_dodge())
}


plot_success(file)

plot_success(file[file$country=="GB",])
plot_success(file[file$country=="US",])
plot_success(file[file$country=="CA",])
plot_success(file[file$country=="DE",])
plot_success(file[file$country=="AU",])

data <- ddply(file, .(main_category, country), summarize, rate=sum(status=="successful")*100/length(status))
data

ggplot(data = data, aes(x=main_category, y=rate, fill=country)) + 
  geom_bar(position="identity", stat = "identity")
#trier

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

#succÃ©s par catÃ©gorie pour chaque pays 




ggplot(dat2, aes(x = variable, y = value, fill = row)) + 
  geom_bar(stat = "identity") +
  xlab("\nType") +
  ylab("Time\n") +
  guides(fill = FALSE) +
  theme_bw()
