#Vetores
mean(vetor01, na.rm=TRUE)
summary(vetor01)
mean(is.na(vetor01))

sum((vetor01 > 7 & vetor01 < 8), na.rm=TRUE)
sum((vetor01 > 9 | vetor01 < 1), na.rm=TRUE)

vetor02 <- vetor01[!is.na(vetor01)]
sum(is.na(vetor02))

#Fatores
drinks <- factor(c("beer","beer","wine","water"))
length(fator)

drinks <- factor(c("beer","beer","wine","water"))
mean(drinks=="beer")

levels(drinks)[1] <- "water"

#Listas
lista_pessoas <- list(nomes=c("João", "Paula", "Maria", "Ingrid", "José", "Marcos"),
              pesos=c(80, 65, 70, 58, 78, 70),
              alturas=c(1.70, 1.66, 1.65, 1.60, 1.76, 1.70))
str(lista_pessoas)
names(lista_pessoas)

IMC <- lista_pessoas$pesos/lista_pessoas$alturas^2
lista_pessoas$IMC <- IMC
lista_pessoas[[1]][1]

lista<-lapply(airquality, function(x){mean(x)})

#Matrizes
chuvas[chuvas[,]>10]
ma <- chuvas>10

mean(chuvas)
max(chuvas)
total_mun <- rowSums(chuvas)
total_dia <- colSums(chuvas)

aux<-chuvas["mun_81" , ]
aux<-sum(aux[1:10])