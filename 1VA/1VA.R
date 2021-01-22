#1VA
##Setup Workspace
setwd("~/workspace/Análise de Dados/1VA")
getwd()

##Install packages and load them
install.packages("tidyr")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("AER", dependencies=TRUE, repos='http://cran.rstudio.com/')
library(tidyr)
library(dplyr)
library(ggplot2)
library(AER)

#1 Questão
strg <- c("Voda 30", "bylinky 25", "ZEM 23", "zlAto 22")
paste(tolower(strg),"%",sep="")

#2 Questão
vec <- c("5!eAZ", "!6!rrrrDFGT", "R99Tf34!RR", "123?aSd")
grep("^([0-9][[:punct:]][a-z])",vec, value=T)
str_extract_all(string=vec,pattern="^([0-9][[:punct:]][a-z])")

#3 Questão
library(readr)
poem <- read_file("gambler.txt")
nchar(poem)

#4 Questão
domains <- c("www.dogman.com", "http://rotterdam.com", "https://facebook.com", "httpx://sims.com", "fungame.http")
grep("^(http|https)",domains, value=T)

#5 Questão
Forbes2000 <- read.csv("Forbes2000_V2.csv")
View(Forbes2000)
`%notin%` <- Negate(`%in%`)

ranqueamento  <- function(ranque, categoria=NULL, opcao){
  
  if(opcao == 1){
    result <- Forbes2000 %>% arrange(desc(marketvalue))
    count <- (Forbes2000 %>% summarise(count=n()))$count
  }
  else if (is.null(categoria) | categoria %notin% Forbes2000$category){
    return("Categoria não existe")
  }
  else if(opcao ==2){
    result <- Forbes2000 %>% filter(category == categoria) %>% arrange(desc(marketvalue))
    count <- (Forbes2000 %>% filter(category == categoria) %>% summarise(count=n()))$count
  }
  
  if(count < ranque){
    return("Rank não existe")
  }
  return(select(result[ranque,], name, category))
}

ranqueamento(ranque=1, opcao = 1)
ranqueamento(ranque = 1, categoria = "Comida e Serviço", opcao = 2)


ranqueamento(ranque=1234, opcao=1)
ranqueamento(ranque=198, opcao=1)
ranqueamento(ranque=45, categoria="Technology hardware & equipment", opcao=2)
ranqueamento(ranque=7, categoria="Bancos", opcao=2)

#6 Questão
diamonds
summary(diamonds)

sub1 <- diamonds %>% filter(cut=="Very Good", carat > 0.7)
summary(sub1)

sub2 <- diamonds %>% filter(carat > 0.5)
sub2 <- sub2 %>% filter(price == min(sub2$price))

sub3 <- diamonds %>% filter(cut == "Premium")



nrow(sub3) / nrow(diamonds)

#7 Questão
data("Fertility")
?Fertility

Fertility%>%select(age,work)%>%slice(35:50)

len_filter = Fertility %>% filter(morekids=="yes", work > 30) %>% summarize(count=n())
len_total = Fertility %>% summarize(count=n())

len_filter$count/len_total$count 

len_total = Fertility %>% filter(age >= 22, age <= 24) %>% summarize(count=n())
len_filter = Fertility %>% filter(age >= 22, age <= 24, gender1 == "male") %>% summarize(count=n())

len_filter$count/len_total$count

#8 Questão
Catfish = read.csv("Catfish.csv")
Treatment = read.csv("Treatment.csv")

View(Catfish)
head(Catfish)
head(Treatment)

Catfish <- Catfish %>% gather(key="Month", value="Weight", -ID, -Genus, -Species, -Sex, -Year, -Tank)
Catfish <- Catfish %>% unite(col="Species", Genus, Species, sep=".")
summary(Catfish)

Catfish_Treatment <- merge(x=Catfish, y=Treatment, by="Tank")
View(Catfish_Treatment)
summary(Catfish_Treatment)

mean(filter(Catfish_Treatment, Food == "Treatment2", Month == "April", Sex == "Male")$Weight)

Catfish_Treatment$AcimaMedia<-ifelse(Catfish_Treatment$Weight>mean(Catfish_Treatment$Weight),'V','F')

Catfish_Treatment %>% filter(AcimaMedia == "V") %>% group_by(Tank) %>% summarise(Count=n())


a <- Catfish_Treatment %>% filter(AcimaMedia == "V")

#9 Questão

media <- c(30,155,50,1829,75)
dp <- c(6, 23, 8, 274, 12)

maria <- c(42, 70, 40, 1500, 97)
teresa <- c(38, 173, 71, 1554, 70)
francisca <- c(50, 150, 40, 1900, 40)
joaquina <- c(40, 140, 80, 2093, 100)

maria.n <- (maria-media)/dp
teresa.n <- (teresa-media)/dp
francisca.n <- (francisca-media)/dp 
joaquina.n <- (joaquina-media)/dp

mean(maria.n)
mean(teresa.n)
mean(francisca.n)
mean(joaquina.n)

#10 Questão
summary(airquality)
comp <- airquality[!apply(airquality, 1, anyNA),]


aux<-read.csv("tb.csv")
summary(aux)

aux <- aux %>% slice(1348:4954)
comp2<-aux[!apply(aux, 1, anyNA),]

