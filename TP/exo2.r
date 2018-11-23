# http://homepages.laas.fr/gtredan/tmds/

# ---------------- 1 ---------------- #

prenoms = read.csv("~/Documents/TMDS/prenoms.csv",sep=";")

library(ggplot2)
library(plyr)
library(stringr)

#prenSex = ddply(prenoms, c("Année"), function(x){sum(x$Nombre)})
prenSex = ddply(prenoms, c("Année"), summarise,Total=sum(Nombre))
ggplot(prenSex,aes(factor(Année),Total)) + geom_bar(stat="identity",position="dodge") + xlab("Year") + ylab("Number")

prenSex = ddply(prenoms, c("Année","Sexe"), function(x){sum(x$Nombre)})
ggplot(prenSex,aes(factor(Année),V1,fill=Sexe)) + geom_bar(stat="identity",position="dodge") + xlab("Year") + ylab("Number")

"Jean-Célestin" %in% prenoms$Prénom

prenSex = ddply(prenoms, c("Prénom","Sexe"), function(x){sum(x$Nombre)})
prenSex[order(-(prenSex$V1)),][1:10,]

prenTop = ddply(prenoms, c("Année","Sexe","Prénom"), function(x){sum(x$Nombre)})
prenTop = prenSex[order(prenSex$Année,prenSex$Sexe,(-(prenSex$V1))),]
prenTopF = subset(prenTop,Sexe == 'F')
prenTopM = subset(prenTop,Sexe == 'M')
prenTop5F = subset(prenTopF,Prénom %in% Prénom[1:5])
prenTop5M = subset(prenTopM,Prénom %in% Prénom[1:5])
ggplot(data=prenTop5F, aes(x=Année, y=V1, colour=Prénom)) + geom_point(size=2) + geom_line(size=1) + ggtitle("Popularity of Top 5 female names since 2003") + xlab("Années") + ylab("Nombre de naissances") + scale_x_continuous(breaks = seq(2003, 2012))
ggplot(data=prenTop5M, aes(x=Année, y=V1, colour=Prénom)) + geom_point(size=2) + geom_line(size=1) + ggtitle("Popularity of Top 5 male names since 2003") + xlab("Années") + ylab("Nombre de naissances") + scale_x_continuous(breaks = seq(2003, 2012))

prenLen = ddply(prenoms, c("Année","Sexe","Prénom"), summarise,Taille=nchar(toString(Prénom)))
prenLen = ddply(prenLen, c("Année"),summarise,Moyenne=mean(Taille))
ggplot(data=prenLen, aes(x=Année, y=Moyenne)) + geom_point(size=2) + geom_line(size=1) + ggtitle("Taille moyenne des prénoms par année") + xlab("Années") + ylab("Nombre de lettres") + scale_x_continuous(breaks = seq(2003, 2012))

a = 
a[a %in% vowels]
a[a %in% setdiff(letters,vowels)]

prenLen = ddply(prenoms, 
                   c("Année","Sexe","Prénom"), 
                   summarise,
                   Splitted = strsplit(str_to_lower(iconv(toString(Prénom),to="ASCII//TRANSLIT")),""),
                   Vowels = length(Splitted[[1]][Splitted[[1]] %in% vowels]),
                   Consonants = length(Splitted[[1]][Splitted[[1]] %in% setdiff(letters,vowels)])
)
prenLen = ddply(prenLen, c("Année"),summarise,MoyenneV=mean(Vowels),MoyenneC=mean(Consonants))
ggplot(data=prenLen, aes(x=Année, y=MoyenneC)) + geom_point(size=2) + geom_line(size=1) + ggtitle("Nombre moyen de consonnes par année") + xlab("Années") + ylab("Nombre de consonnes") + scale_x_continuous(breaks = seq(2003, 2012))
ggplot(data=prenLen, aes(x=Année, y=MoyenneV)) + geom_point(size=2) + geom_line(size=1) + ggtitle("Nombre moyen de voyelles par année") + xlab("Années") + ylab("Nombre de voyelles") + scale_x_continuous(breaks = seq(2003, 2012))

prenComp = ddply(prenoms, 
                c("Année","Sexe","Nombre","Prénom"), 
                summarise,
                Splitted = strsplit(toString(Prénom),""),
                Composed = '-' %in% Splitted[[1]]
)
prenComp = ddply(prenComp, c("Année"),summarise,MoyenneComposés=sum(Composed*Nombre))
ggplot(data=prenComp, aes(x=Année, y=MoyenneComposés)) + geom_point(size=2) + geom_line(size=1) + ggtitle("Nombre de prénoms composés par année") + xlab("Années") + ylab("Nombre de prénoms composés") + scale_x_continuous(breaks = seq(2003, 2012))











