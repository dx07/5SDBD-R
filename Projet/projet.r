# https://www.kaggle.com/gregorut/videogamesales

vgsales = read.csv("~/../PycharmProjects/5SDBD-R/Projet/vgsales.csv",sep=",")
#vgsales = read.csv("C:/Users/Fatine/Desktop/INSA 5A/5SDBD-R-master/Projet/vgsales.csv", sep=",")

install.packages("ggplot2")
install.packages("gridExtra")
library(ggplot2)
library(plyr)
library(stringr)
library(grid)
library(gridExtra)

head(vgsales)

#Top 10 des jeux vidéo les plus vendus années confondues // années spécifiques
classement <- arrange(vgsales, desc(Global_Sales))
top10 <- subset(classement, Global_Sales %in% Global_Sales[1:10])

#Top 10 des jeux vidéo les plus vendus de tous les temps
dfAll <- aggregate(Global_Sales ~ Name, vgsales, sum)
classementAll <- arrange(dfAll, desc(Global_Sales)) 
topAll <- subset(classementAll, Global_Sales %in% Global_Sales[1:10])

vg2013 <- subset(vgsales, Year == "2013")
df2013 <- aggregate(Global_Sales ~ Name, vg2013, sum)
classement2013 <- arrange(df2013, desc(Global_Sales))
top2013 <- subset(classement2013, Global_Sales %in% Global_Sales[1:10])

vg2014 <- subset(vgsales, Year == "2014")
df2014 <- aggregate(Global_Sales ~ Name, vg2014, sum)
classement2014 <- arrange(df2014, desc(Global_Sales))
top2014 <- subset(classement2014, Global_Sales %in% Global_Sales[1:10])

vg2015 <- subset(vgsales, Year == "2015")
df2015 <- aggregate(Global_Sales ~ Name, vg2015, sum)
classement2015 <- arrange(df2015, desc(Global_Sales))
top2015 <- subset(classement2015, Global_Sales %in% Global_Sales[1:10])

vg2016 <- subset(vgsales, Year == "2016")
df2016 <- aggregate(Global_Sales ~ Name, vg2016, sum)
classement2016 <- arrange(df2016, desc(Global_Sales))
top2016 <- subset(classement2016, Global_Sales %in% Global_Sales[1:10])

dd.col <- rainbow(length(top10$Name))
names(dd.col) <- top10$Name
ggplot(data = top10, aes(x= reorder(paste(Name, Platform, sep=" - "), Global_Sales), y=Global_Sales, fill = Name)) + geom_bar(stat="identity", show.legend = FALSE) + coord_flip() + xlab("Jeu Vidéo") + ylab("Nombre de ventes en M") + ggtitle("Top 10 des jeux vidéo les plus vendus de tous les temps dans le monde sur une console") + scale_fill_manual("Legend", values = dd.col)

dd.col <- rainbow(length(topAll$Name))
names(dd.col) <- topAll$Name
ggplot(data = topAll, aes(x= reorder(Name, Global_Sales), y=Global_Sales, fill = Name)) + geom_bar(stat="identity", show.legend = FALSE) + coord_flip() + xlab("Jeu Vidéo") + ylab("Nombre de ventes en M") + ggtitle("Top 10 des jeux les plus vendus de tous les temps dans le monde toute console confondue") + scale_fill_manual("Legend", values = dd.col)

dd.col <- rainbow(length(top2013$Name))
names(dd.col) <- top2013$Name
g1 <- ggplot(data = top2013, aes(x= reorder(Name, Global_Sales), y=Global_Sales, fill = Name)) + geom_bar(stat="identity", show.legend = FALSE) + coord_flip() + xlab("Jeu Vidéo") + ylab("Nombre de ventes en M") + ggtitle("Top 10 2013")  + scale_fill_manual("Legend", values = dd.col)

dd.col <- rainbow(length(top2014$Name))
names(dd.col) <- top2014$Name
g2 <- ggplot(data = top2014, aes(x= reorder(Name, Global_Sales), y=Global_Sales, fill = Name)) + geom_bar(stat="identity", show.legend = FALSE) + coord_flip() + xlab("Jeu Vidéo") + ylab("Nombre de ventes en M") + ggtitle("Top 10 2014")  + scale_fill_manual("Legend", values = dd.col)

dd.col <- rainbow(length(top2015$Name))
names(dd.col) <- top2015$Name
g3 <- ggplot(data = top2015, aes(x= reorder(Name, Global_Sales), y=Global_Sales, fill = Name)) + geom_bar(stat="identity", show.legend = FALSE) + coord_flip() + xlab("Jeu Vidéo") + ylab("Nombre de ventes en M") + ggtitle("Top 10 2015")  + scale_fill_manual("Legend", values = dd.col)

dd.col <- rainbow(length(top2016$Name))
names(dd.col) <- top2016$Name
g4 <- ggplot(data = top2016, aes(x= reorder(Name, Global_Sales), y=Global_Sales, fill = Name)) + geom_bar(stat="identity", show.legend = FALSE) + coord_flip() + xlab("Jeu Vidéo") + ylab("Nombre de ventes en M") + ggtitle("Top 10 2016")  + scale_fill_manual("Legend", values = dd.col)

grid.arrange(g1, g2, g3, g4, ncol=2, top = "Top 10 des jeux vidéo les plus vendus dans le monde par années")




#Quel éditeur est le plus productif
#celui qui produit le plus
editeurs_jeu = vgsales[c('Name','Publisher')]
editeurs_uni = unique(editeurs_jeu)
editeurs_cou <- count(df = editeurs_uni, vars = c("Publisher"))
classement_editeurs <- arrange(editeurs_cou, desc(freq))
classement_editeurs5 <- subset(classement_editeurs, freq %in% freq[1:5])
classement_editeurs5_2 <- subset(classement_editeurs, freq %in% freq[6:10])
#Pour éviter les noms qui se chevauchent sur le ggplot
classement_editeurs5$Publisher = str_wrap(classement_editeurs5$Publisher, width = 10)
classement_editeurs5_2$Publisher = str_wrap(classement_editeurs5_2$Publisher, width = 10)

#Plots
dd.col <- rainbow(length(classement_editeurs5$Publisher))
names(dd.col)  <- classement_editeurs5$Publisher
ggplot(data = classement_editeurs5, aes(x= reorder(Publisher, desc(freq)), y=freq, fill = Publisher)) + geom_bar(stat="identity", show.legend = FALSE) + ylab("Nombre de publications") + xlab("Editeurs") + ggtitle("Les 5 éditeurs de jeux vidéo les plus productifs") + scale_fill_manual("Legend", values = dd.col)

dd.col <- rainbow(length(classement_editeurs5_2$Publisher))
names(dd.col)  <- classement_editeurs5_2$Publisher
ggplot(data = classement_editeurs5_2, aes(x= reorder(Publisher, desc(freq)), y=freq, fill = Publisher)) + geom_bar(stat="identity", show.legend = FALSE) + ylab("Nombre de publications") + xlab("Editeurs") + ggtitle("Les 5 AUTRES éditeurs de jeux vidéo les plus productifs") + scale_fill_manual("Legend", values = dd.col)


#Quel est l'éditeur « favori » des joueurs ? 
#Celui qui vend le plus
favoris <- ddply(vgsales, c("Publisher"), function(x){sum(x$Global_Sales)})
classement_favoris <- arrange(favoris, desc(V1))
classement_favoris5 <- subset(classement_favoris, V1 %in% V1[1:5])
#Pour éviter les noms qui se chevauchent sur le ggplot
classement_favoris5$Publisher = str_wrap(classement_favoris5$Publisher, width = 10)

#Plots
dd.col <- rainbow(length(classement_favoris5$Publisher))
names(dd.col)  <- classement_favoris5$Publisher
ggplot(data = classement_favoris5, aes(x= reorder(Publisher, desc(V1)), y=V1, fill = Publisher)) + geom_bar(stat="identity", show.legend = FALSE) + ylab("Nombre de ventes en millions d'exemplaires") + xlab("Editeurs") + ggtitle("Les 5 éditeurs de jeux vidéo favoris") + scale_fill_manual("Legend", values = dd.col)



#Mérite-t-il toujours son titre aujourd'hui ?
#Evolution des ventes au cours des années
favoris5 = gsub("\n"," ",classement_favoris5$Publisher)
favoris = subset(vgsales,Publisher %in% favoris5 &  Year != "N/A" & strtoi(Year) < 2016 & strtoi(Year) > 1994)
favoris$Year = strtoi(favoris$Year)
favoris_year = ddply(favoris, c("Year","Publisher"), function(x){sum(x$Global_Sales)})
ggplot(data = favoris_year, aes(x = Year, y = V1, color = Publisher)) + 
  geom_point(size=2) + 
  geom_line(size=1) + 
  xlab("Année") + 
  ylab("Nombre de ventes en millions d'exemplaires") + 
  ggtitle("Evolution du nombre de ventes annuelles des éditeurs favoris") +
  scale_x_continuous(breaks = seq(1995, 2015,2))


# Les ventes US / JP
fun_origin = function(origin_year) {
  origin_year$Year = strtoi(origin_year$Year)
  origin_na = subset(origin_year, Publisher %in% c("Electronic Arts","Activision"))
  origin_jp = subset(origin_year, Publisher %in% c("Nintendo","Sony Computer Entertainment"))
  origin_na$Publisher = "American"
  origin_jp$Publisher = "Japanese"
  origin_all = rbind(origin_na, origin_jp)
  origin_syn = ddply(origin_all, c("Publisher"), function(x){c(sum(x$NA_Sales),sum(x$JP_Sales),sum(x$EU_Sales))})
  return(origin_syn)
}

percent <- function(x, digits = 2, format = "f", ...) {
  paste0(formatC(100 * x, format = format, digits = digits, ...), "%")
}

origin_year1 = subset(vgsales, Year != "N/A" & strtoi(Year) < 2006 & strtoi(Year) > 1995)
origin_year2 = subset(vgsales, Year != "N/A" & strtoi(Year) < 2016 & strtoi(Year) > 2005)

origin_syn1 = fun_origin(origin_year1)
origin_syn2 = fun_origin(origin_year2)

pie_theme = theme(
  axis.title.x = element_blank(),
  axis.title.y = element_blank(),
  panel.border = element_blank(),
  panel.grid=element_blank(),
  axis.ticks = element_blank(),
  axis.text.x=element_blank()
)

g1 = ggplot(origin_syn1, aes(x="", y=V1, fill=Publisher))+
  coord_polar("y",start=0)+
  geom_bar(width = 1, stat = "identity")+
  ggtitle("Ventes en North America (1996-2005)")+
  geom_text(aes(y = V1/2 + c(0, cumsum(V1)[-length(V1)]),label = percent(V1/sum(V1))), size=5)+
  pie_theme
  

g2 = ggplot(origin_syn1, aes(x="", y=V2, fill=Publisher))+
  coord_polar("y",start=0)+
  geom_bar(width = 1, stat = "identity")+
  ggtitle("Ventes au Japon (1996-2005)")+
  geom_text(aes(y = V2/2 + c(0, cumsum(V2)[-length(V2)]),label = percent(V2/sum(V2))), size=5)+
  pie_theme

g3 = ggplot(origin_syn2, aes(x="", y=V1, fill=Publisher))+
  coord_polar("y",start=0)+
  geom_bar(width = 1, stat = "identity")+
  ggtitle("Ventes en North America (2006-2015)")+
  geom_text(aes(y = V1/2 + c(0, cumsum(V1)[-length(V1)]),label = percent(V1/sum(V1))), size=5)+
  pie_theme


g4 = ggplot(origin_syn2, aes(x="", y=V2, fill=Publisher))+
  coord_polar("y",start=0)+
  geom_bar(width = 1, stat = "identity")+
  ggtitle("Ventes au Japon (2006-2015)")+
  geom_text(aes(y = V2/2 + c(0, cumsum(V2)[-length(V2)]),label = percent(V2/sum(V2))), size=5)+
  pie_theme

grid.arrange(g1, g2, g3, g4, ncol=2, top="Volume des ventes de jeux vidéo selon leur origine (sur les 4 éditeurs les plus populaires)")


#Evolution pour chaque genre
genres = gsub("\n"," ",unique(vgsales$Genre))
g = subset(vgsales, Genre %in% genres &  Year != "N/A" & strtoi(Year) < 2016 & strtoi(Year) > 1995)
g$Year = strtoi(g$Year)
genres_year = ddply(g, c("Year","Genre"), summarise, Global_Sales = sum(Global_Sales))

g2014 = subset(genres_year, Year == 2014)
g2015 = subset(genres_year, Year == 2015)

for (row in 1:nrow(g2014)) {
  s2014 = g2014[row, "Global_Sales"]
  s2015 = g2015[row, "Global_Sales"]
  s2016 = s2015 + (s2015-s2014)
  if(s2016 < 0) s2016 = 0
  genres_year = rbind(genres_year,list(2016,g2014[row, "Genre"],s2016))
}

ggplot(data = genres_year, aes(x = Year, y = Global_Sales, color = Genre)) + 
  geom_point(size=2) + 
  geom_line(size=1) + 
  xlab("Année") + 
  ylab("Nombre de ventes en millions d'exemplaires") + 
  ggtitle("Evolution du nombre de ventes annuelles par genre") +
  scale_x_continuous(breaks = seq(1996, 2016,2))

