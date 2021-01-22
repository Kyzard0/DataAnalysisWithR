## 1 Questão
VADeaths

categories <- colnames(VADeaths)
idades <- row.names(VADeaths)
colors <- c("green", "blue", "yellow", "orange", "red")
barplot(VADeaths,
        main="Taxa de Mortes em Virginia (1940)",
        xlab="Categoria",ylab="Taxa de Mortalidade",
        col=colors,
        beside = F)
legend("topright", pch=c(15,15,15,15), col=colors, legend=row.names(VADeaths))

## 2 Questão
dados <- c("moderado", "leve", "leve", "severo", "leve", "moderado", "moderado", "moderado", "leve", "leve", "severo","leve", "moderado", "moderado", "leve", "severo", "moderado", "moderado", "moderado","leve")
dados <- as.data.frame(table(dados))
pct <- round(dados$Freq/sum(dados$Freq)*100) # Calcula porcentagem de vendas
lbls <- paste(pct, "%", sep="") #Adiciona % no final dos valores
pie(dados$Freq,
    lbls,
    main="Porcentagem de Casos da Doença por Estágio",
    col = c("yellow","purple","grey"))
legend("topleft",
       legend=dados$dados,
       pch=15,
       col=c("yellow","purple","grey"))


## 3 Questão
library(tm)
library(wordcloud)
library(twitteR)
library(syuzhet)

consumer_key <- 'ijoshXZ38K6aZWOu5Mu9icHOm'
consumer_secret <- 'GA7n7Ltx7yDwTSBFOMzspEN9RGBIup0AUOG4ijkXlqoaXNmUDQ'
access_token <- '1616525624-ZiKHRhHKUjella6iGdrya4BLIJyBSzXBEJQIaff'
access_secret <- '9RcdboV4utBeZT6h5LRyXkuwGgSXsUKVmEgRjpOw6pEVC'
setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)


#Carrega os tweets
tweets <- searchTwitter("#racismo", n=500, lang = "pt")
tweets <- twListToDF(tweets)
tweets_t <- paste(tweets$text, collapse = " ")
tweets <- tweets$text

#Cria o corpus
tweets_S <- VectorSource(tweets_t)
corpus <- Corpus(tweets_S)

# Limpa os textos
corpus <- tm_map(corpus, tolower) #Coloca para minúsculo
corpus <- tm_map(corpus, removePunctuation) #Remove pontuação
corpus <- tm_map(corpus, removeNumbers) #Remove números
corpus <- tm_map(corpus, stripWhitespace) #Remove espaços em brancos extras
corpus <- tm_map(corpus, removeWords, stopwords('portuguese'))

#Remove URLs
removeURL <- function(x) gsub("http[^[:space:]]*", "", x)
corpus <- tm_map(corpus, removeURL)

#Normaliza ascii
removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
corpus <- tm_map(corpus, content_transformer(removeNumPunct))

#Cria a matriz dos textos
dtm <- TermDocumentMatrix(corpus)
dtm <- as.matrix(dtm)

#Obtém frequência das palavras
fre <- sort(rowSums(dtm), decreasing = T)

#Cria nuvem de palavras
wordcloud(corpus, min.freq = 1, max.words = 100, random.order = F,
          rot.per = 0.35, colors = brewer.pal(8, "Dark2"))

#Pontua os tweets para análise de sentimentos
s <- get_nrc_sentiment(tweets)

#Plota gráfico dos sentimentos
barplot(colSums(s), las=2, col = rainbow(10),
        ylab = "Quantidade", main="Pontuação de Sentimentos para os Tweets de Racismo")

## Questão 4

flu <- read.csv("flu.csv")

dens = density(flu$age) #Densidade
hist(flu$age,
     col="grey",
     main = "Histograma das Idades das mortes da epidemia da Gripe Espanhola ",
     xlab="Idade",
     ylab="Frequência",
     probability = T)
lines(dens)

n <- 200
TamMedia <- 35
xbar <- rep(NA, n)
#Cria o vetor de médias das 200 amostras
for(i in 1:n){
  MinhaAmostra <- sample(flu$age, size = TamMedia)
  xbar[i] <- mean(MinhaAmostra)
}

dens = density(xbar) #Densidade
hist(xbar,
     col="grey",
     main = "Histograma das Médias de 200 amostras da epidemia da Gripe Espanhola",
     xlab="Idade",
     ylab="Frequência",
     probability = T)
lines(dens)

## Questao 5

N <- 300
sd <- 0.5
nc <- (1-0.99)/2
erro <- 0.1
n <- (qnorm(nc, lower.tail = F)^2*sd^2*N)/((erro^2*(N-1)) + (qnorm(nc, lower.tail = F)^2*sd^2))

## Questão 6
install.packages("MASS")
install.packages("nortest")
library(MASS)
library(nortest)
anorexia <- anorexia
antes <- anorexia$Prewt
depois <- anorexia$Postwt

ad.test(antes)
ad.test(depois)
var.test(antes, depois)

t.test(antes, depois, paired=TRUE, var.equal = T, alternative="two.sided")

## Questão 7

grupo20 <- c(27, 26, 21, 24, 15, 18, 17, 12 ,13)
grupo60 <- c(26, 29, 29, 29, 27, 16, 20, 27)

shapiro.test(grupo20)
shapiro.test(grupo60)
var.test(grupo20, grupo60)

t.test(grupo20, grupo60, alternative="two.sided", var.equal = T)

## Questão 8
library(dplyr)
load("response.time.RData")

summary(response.time)

nearest <- response.time %>% filter(slave_count == 1, slave_info == "nearest") %>% select(value)
farthest <- response.time %>% filter(slave_count == 1, slave_info == "farthest")  %>% select(value)

ad.test(nearest$value)
ad.test(farthest$value)
hist(nearest$value)
hist(farthest$value)
wilcox.test(nearest$value,farthest$value,alternative = "two.sided")

## Questão 9

media <- 400
sd <- 45
z <- qnorm(prob,media, sd, lower.tail = F)
prob <- pnorm(500,media, sd, lower.tail = F)
prob*100

## Questão 10

load("bdims.RData")
altura_mulheres <- bdims %>% filter(sex == 0) %>% select(hgt)
media <- mean(altura_mulheres$hgt)
N <- length(altura_mulheres$hgt)
sd <- sd(altura_mulheres$hgt)
nc <- (1-0.985)/2
erro <- sd/sqrt(N)
left <- media-(qnorm(nc, lower.tail = F)*erro)
right <- media+(qnorm(nc, lower.tail = F)*erro)
cat("[", left, "-", right, "]")

