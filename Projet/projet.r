# https://www.kaggle.com/nicapotato/womens-ecommerce-clothing-reviews

#vgsales = read.csv("~/../PycharmProjects/5SDBD-R/Projet/vgsales.csv",sep=",")
vgsales = read.csv("C:/Users/Fatine/Desktop/INSA 5A/5SDBD-R-master/Projet/vgsales.csv", sep=",")

#install.packages("ggplot2")
#install.packages("gridExtra")
library(ggplot2)
library(plyr)
library(stringr)
library(grid)
library(gridExtra)

head(vgsales)


#Top 10 des jeux vidéos les plus vendus années confondues // années spécifiques
classement <- arrange(vgsales, desc(Global_Sales))
top10 <- subset(classement, Global_Sales %in% Global_Sales[1:10])

vg2013 <- subset(vgsales, Year == "2013")
df2013 <- aggregate(Global_Sales ~ Name, transform(vg2013, Name = sub("\\d+$","", Name)), sum)
classement2013 <- arrange(df2013, desc(Global_Sales))
top2013 <- subset(classement2013, Global_Sales %in% Global_Sales[1:10])

vg2014 <- subset(vgsales, Year == "2014")
df2014 <- aggregate(Global_Sales ~ Name, transform(vg2014, Name = sub("\\d+$","", Name)), sum)
classement2014 <- arrange(df2014, desc(Global_Sales))
top2014 <- subset(classement2014, Global_Sales %in% Global_Sales[1:10])

vg2015 <- subset(vgsales, Year == "2015")
df2015 <- aggregate(Global_Sales ~ Name, transform(vg2015, Name = sub("\\d+$","", Name)), sum)
classement2015 <- arrange(df2015, desc(Global_Sales))
top2015 <- subset(classement2015, Global_Sales %in% Global_Sales[1:10])

vg2016 <- subset(vgsales, Year == "2016")
df2016 <- aggregate(Global_Sales ~ Name, transform(vg2016, Name = sub("\\d+$","", Name)), sum)
classement2016 <- arrange(df2016, desc(Global_Sales))
top2016 <- subset(classement2016, Global_Sales %in% Global_Sales[1:10])

#Top 10 des jeux vidéos les plus vendus de tous les temps
dfAll <- aggregate(Global_Sales ~ Name, transform(vgsales, Name = sub("\\d+$","", Name)), sum)
classementAll <- arrange(dfAll, desc(Global_Sales)) 
topAll <- subset(classementAll, Global_Sales %in% Global_Sales[1:10])

#Couleurs
dd <- union(union(union(union(union(top10$Name, topAll$Name), top2016$Name), top2015$Name), top2014$Name), top2013$Name)
dd.col <- rainbow(length(dd))
names(dd.col)  <- dd

#Plots
ggplot(data = top10, aes(x= reorder(Name, Global_Sales), y=Global_Sales, fill = Name)) + geom_bar(stat="identity", show.legend = FALSE) + coord_flip() + xlab("Jeu Vidéo") + ylab("Nombre de ventes dans le monde") + ggtitle("Top 10 des jeux vidéos les plus vendus") + scale_fill_manual("Legend", values = dd.col)
ggplot(data = topAll, aes(x= reorder(Name, Global_Sales), y=Global_Sales, fill = Name)) + geom_bar(stat="identity", show.legend = FALSE) + coord_flip() + xlab("Jeu Vidéo") + ylab("Nombre de ventes dans le monde") + ggtitle("Top 10 des jeux vidéos les plus vendus de tous les temps") + scale_fill_manual("Legend", values = dd.col)


g1 <- ggplot(data = top2013, aes(x= reorder(Name, Global_Sales), y=Global_Sales, fill = Name)) + geom_bar(stat="identity", show.legend = FALSE) + coord_flip() + xlab("Jeu Vidéo") + ylab("Nombre de ventes dans le monde") + ggtitle("Top 10 des jeux vidéos les plus vendus en 2013")  + scale_fill_manual("Legend", values = dd.col)
g2 <- ggplot(data = top2014, aes(x= reorder(Name, Global_Sales), y=Global_Sales, fill = Name)) + geom_bar(stat="identity", show.legend = FALSE) + coord_flip() + xlab("Jeu Vidéo") + ylab("Nombre de ventes dans le monde") + ggtitle("Top 10 des jeux vidéos les plus vendus en 2014")  + scale_fill_manual("Legend", values = dd.col)
g3 <- ggplot(data = top2015, aes(x= reorder(Name, Global_Sales), y=Global_Sales, fill = Name)) + geom_bar(stat="identity", show.legend = FALSE) + coord_flip() + xlab("Jeu Vidéo") + ylab("Nombre de ventes dans le monde") + ggtitle("Top 10 des jeux vidéos les plus vendus en 2015")  + scale_fill_manual("Legend", values = dd.col)
g4 <- ggplot(data = top2016, aes(x= reorder(Name, Global_Sales), y=Global_Sales, fill = Name)) + geom_bar(stat="identity", show.legend = FALSE) + coord_flip() + xlab("Jeu Vidéo") + ylab("Nombre de ventes dans le monde") + ggtitle("Top 10 des jeux vidéos les plus vendus en 2016")  + scale_fill_manual("Legend", values = dd.col)

grid.arrange(g1, g2, g3, g4, ncol=2)






#Quel éditeur est le plus productif
#celui qui produit le plus
editeurs <- count(df = vgsales, vars = c("Publisher"))
classement_editeurs <- arrange(editeurs, desc(freq))
classement_editeurs5 <- subset(classement_editeurs, freq %in% freq[1:5])

#Quel est l'éditeur « favori » des joueurs ? 
#Celui qui vend le plus
favoris <- ddply(vgsales, c("Publisher"), function(x){sum(x$Global_Sales)})
classement_favoris <- arrange(favoris, desc(V1))
classement_favoris5 <- subset(classement_favoris, V1 %in% V1[1:5])
#-->Editeur favori : Nintendo

#Couleurs
dd <- union(classement_editeurs5$Publisher, classement_favoris5$Publisher)
dd.col <- rainbow(length(dd))
names(dd.col)  <- dd

#Plots
ggplot(data = classement_editeurs5, aes(x= reorder(Publisher, desc(freq)), y=freq, fill = Publisher)) + geom_bar(stat="identity", show.legend = FALSE) + ylab("Nombre de jeux vidéos publiés") + xlab("Editeurs les plus productifs") + ggtitle("Les 5 éditeurs de jeux vidéos les plus productifs") + scale_fill_manual("Legend", values = dd.col)
ggplot(data = classement_favoris5, aes(x= reorder(Publisher, desc(V1)), y=V1, fill = Publisher)) + geom_bar(stat="identity", show.legend = FALSE) + ylab("Nombre de jeux vidéos vendus") + xlab("Editeurs favoris") + ggtitle("Les 5 éditeurs de jeux vidéos favoris") + scale_fill_manual("Legend", values = dd.col)





#Mérite-t-il toujours son titre aujourd'hui ?
#Evolution des ventes au cours des années
nintendo <- subset(vgsales, Publisher == "Nintendo")
nintendo_peryear <- ddply(nintendo, c("Year"), function(x){sum(x$Global_Sales)})
ggplot(data = subset(nintendo_peryear, Year != "N/A"), aes(x = Year, y = V1, group=1)) + geom_line(size=2, color = "#0066CC") + scale_x_discrete(breaks =seq(1983, 2016, by = 2)) + xlab("Année") + ylab("Nombre de ventes") + ggtitle("Evolution du nombre de ventes annuelles de Nintendo")








