# http://homepages.laas.fr/gtredan/tmds/

# ---------------- 1 ---------------- #

prenoms = read.csv("~/../PycharmProjects/5SDBD-R/TP/prenoms.csv",sep=";")

library(ggplot2)
library(plyr)
library(stringr)

#prenSex = ddply(prenoms, c("Ann√.e"), function(x){sum(x$Nombre)})
prenSex = ddply(prenoms, c("Ann√.e"), summarise,Total=sum(Nombre))
ggplot(prenSex,aes(factor(Ann√.e),Total)) + geom_bar(stat="identity",position="dodge") + xlab("Year") + ylab("Number")

prenSex = ddply(prenoms, c("Ann√.e","Sexe"), function(x){sum(x$Nombre)})
ggplot(prenSex,aes(factor(Ann√.e),V1,fill=Sexe)) + geom_bar(stat="identity",position="dodge") + xlab("Year") + ylab("Number")

"Jean-CÈlestin" %in% prenoms$Pr√.nom

prenSex = ddply(prenoms, c("Pr√.nom","Sexe"), function(x){sum(x$Nombre)})
prenSex[order(-(prenSex$V1)),][1:10,]

prenTop = ddply(prenoms, c("Ann√.e","Sexe","Pr√.nom"), function(x){sum(x$Nombre)})
prenTop = prenSex[order(prenSex$Ann√.e,prenSex$Sexe,(-(prenSex$V1))),]
prenTopF = subset(prenTop,Sexe == 'F')
prenTopM = subset(prenTop,Sexe == 'M')
prenTop5F = subset(prenTopF,Pr√.nom %in% Pr√.nom[1:5])
prenTop5M = subset(prenTopM,Pr√.nom %in% Pr√.nom[1:5])
ggplot(data=prenTop5F, aes(x=Ann√.e, y=V1, colour=Pr√.nom)) + geom_point(size=2) + geom_line(size=1) + ggtitle("Popularity of Top 5 female names since 2003") + xlab("Ann√.es") + ylab("Nombre de naissances") + scale_x_continuous(breaks = seq(2003, 2012))
ggplot(data=prenTop5M, aes(x=Ann√.e, y=V1, colour=Pr√.nom)) + geom_point(size=2) + geom_line(size=1) + ggtitle("Popularity of Top 5 male names since 2003") + xlab("Ann√.es") + ylab("Nombre de naissances") + scale_x_continuous(breaks = seq(2003, 2012))

prenLen = ddply(prenoms, c("Ann√.e","Sexe","Pr√.nom"), summarise,Taille=nchar(toString(Pr√.nom)))
prenLen = ddply(prenLen, c("Ann√.e"),summarise,Moyenne=mean(Taille))
ggplot(data=prenLen, aes(x=Ann√.e, y=Moyenne)) + geom_point(size=2) + geom_line(size=1) + ggtitle("Taille moyenne des Pr√.noms par Ann√.e") + xlab("Ann√.es") + ylab("Nombre de lettres") + scale_x_continuous(breaks = seq(2003, 2012))

a = "test"
a[a %in% vowels]
a[a %in% setdiff(letters,vowels)]

prenLen = ddply(prenoms, 
                   c("Ann√.e","Sexe","Pr√.nom"), 
                   summarise,
                   Splitted = strsplit(str_to_lower(iconv(toString(Pr√.nom),to="ASCII//TRANSLIT")),""),
                   Vowels = length(Splitted[[1]][Splitted[[1]] %in% vowels]),
                   Consonants = length(Splitted[[1]][Splitted[[1]] %in% setdiff(letters,vowels)])
)
prenLen = ddply(prenLen, c("Ann√.e"),summarise,MoyenneV=mean(Vowels),MoyenneC=mean(Consonants))
ggplot(data=prenLen, aes(x=Ann√.e, y=MoyenneC)) + geom_point(size=2) + geom_line(size=1) + ggtitle("Nombre moyen de consonnes par Ann√.e") + xlab("Ann√.es") + ylab("Nombre de consonnes") + scale_x_continuous(breaks = seq(2003, 2012))
ggplot(data=prenLen, aes(x=Ann√.e, y=MoyenneV)) + geom_point(size=2) + geom_line(size=1) + ggtitle("Nombre moyen de voyelles par Ann√.e") + xlab("Ann√.es") + ylab("Nombre de voyelles") + scale_x_continuous(breaks = seq(2003, 2012))

prenComp = ddply(prenoms, 
                c("Ann√.e","Sexe","Nombre","Pr√.nom"), 
                summarise,
                Splitted = strsplit(toString(Pr√.nom),""),
                Composed = '-' %in% Splitted[[1]]
)
prenComp = ddply(prenComp, c("Ann√.e"),summarise,MoyenneComposÈs=sum(Composed*Nombre))
ggplot(data=prenComp, aes(x=Ann√.e, y=MoyenneComposÈs)) + geom_point(size=2) + geom_line(size=1) + ggtitle("Nombre de Pr√.noms composÈs par Ann√.e") + xlab("Ann√.es") + ylab("Nombre de Pr√.noms composÈs") + scale_x_continuous(breaks = seq(2003, 2012))











