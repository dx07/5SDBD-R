# http://homepages.laas.fr/gtredan/tmds/

# --------------- 1.1 --------------- #

e1 = c(2,5,0,8)
e1
e2 = 1:200
e2
e3 = 2*(-100:-105)
e3
e4 = 2^(1:7)
e4

v = ((1:50)%%2)*2-1
v

e5 = c(e2,e3)
e5

?seq

e6 = seq(from=0, to=1, length.out=70)
e6

?rep

e7 = rep(e1,10)
e7

e2-e3

# --------------- 1.2 --------------- #

vowels = c('a','e','i','o','u','y')
vowels

letters

letters %in% vowels

which(letters %in% vowels)

which(!(letters %in% vowels))

letters[which(letters %in% vowels)+1]

myname = "jean-kévin"

strsplit(myname,"")[[1]][1]
str(strsplit(myname,""))
str(strsplit(myname,"")[[1]])
str(strsplit(myname,"")[[1]][1])

which(letters %in% strsplit(myname,"")[[1]])

neighbour = "fatine"

myname_list = which(letters %in% strsplit(myname,"")[[1]])
neighbour_list = which(letters %in% strsplit(neighbour,"")[[1]])

mean(myname_list)
mean(neighbour_list)

# --------------- 2.1 --------------- #

df = data.frame(letters = letters, number = 1:26, voyel = letters %in% vowels)

df[myname_list,]

library(ggplot2)

head(msleep)
str(msleep)
names(msleep)
summary(msleep)

all(msleep$awake + msleep$sleep_total < 24) # any for OR

msleep$name[which.max(msleep$sleep_total)]
subset(msleep,sleep_total == max(msleep$sleep_total),select="name")

msleep$name[msleep$bodywt < 100 & msleep$sleep_total > 12]
subset(msleep,bodywt < 100 & sleep_total > 12,select="name")

mean(na.omit(msleep$brainwt/msleep$bodywt))

msleep$name[which.max(na.omit(msleep$brainwt/msleep$bodywt))]

# --------------- 2.2 --------------- #

library(plyr)
msleep_cp = msleep
msleep_cp$conservation <- factor(msleep_cp$conservation, levels = c("lc","domesticated","cd","nt","vu","en"), ordered=TRUE)
arrange(msleep_cp,conservation)
msleep_cp = msleep[order(msleep$conservation),]
msleep_cp

not_threatened = subset(msleep_cp,conservation <= 'nt')
threatened = subset(msleep_cp,conservation > 'nt')

mean(not_threatened$bodywt) < mean(threatened$bodywt)

msleep_cp$threatened = msleep_cp$conservation > 'nt'
msleep_cp

# --------------- 2.3 --------------- #

add = function (x, y = 1){
  x + y
}

fun1 = function(name) {
  name_list = strsplit(name, split="")
  name_num = ""
  for (i in 1:length(name_list[[1]])) {
    name_num = c(name_num, which(dataframe$letter == name_list[[1]][i]))
  }
  return(name_num)
}
fun1("fatine")
fun1("")


fun2 = function(animal) {
  if (animal %in% msleep$name) {
    genus = paste("The", animal, "is a", msleep$genus[which(msleep$name == animal)], sep = " ")
  }
  else {
    genus = "I don't know"
  }
  return(genus)
}
fun2("Goat")
fun2("pch")


