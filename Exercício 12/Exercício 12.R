# questão 1 ---------------------------------------------------------------

MRT_1F <-c(517.1468515630205, 85.13094142168089, 30.333207896694553, 12.694776264558937, 3.3041601673945418, 1.1823111717498882, 1.1892293502386786)
MRT_3F <-c(156.68929936163462, 11.540837783562276, 0.4512835621696538, 0.4509797929766453, 0.4502068233039181, 0.4496185276300172, 0.4543157082191288)
MRT_5F <-c(83.90319666471157, 0.3068151086494968, 0.30522314133037304, 0.3072588968084928, 0.30655265997285697, 0.3055812715727718, 0.3053297166713006)
MRT_10F <-c(29.55430642951759, 0.19832832665772515, 0.1971923924717474, 0.19796648905716516, 0.19615594370806338, 0.2034569237883263, 0.19617420889447737)
MRT_15F <-c(11.317736530583566, 0.167364215666193, 0.16172168266811013, 0.16701085329580515, 0.1598052657153692, 0.1645934043532696, 0.16216563797118075)
MRT_sem_F <-c(11.93430909937736, 0.6095414637034009, 0.6060645101029295, 0.612167181646899, 0.6146761002685637, 0.6096747087200697, 0.6125810476877268)

clock <- c(0.1, 0.5, 1, 1.5, 2, 2.5, 3)

layout(matrix(c(1,1,1,1,2,3,4,5,6,0), 4, 2, byrow = T))
layout.show(6)

plot(clock, MRT_1F,
     type="o",
     pch=4,
     xlim=c(0,3),
     ylim=c(0,500),
     xlab="Time between Things requests (seconds)",
     ylab="Response Time (sec.)",
     cex=1) 
lines(clock, MRT_3F, type="o", col="yellow", pch=11)
lines(clock, MRT_5F, type="o", col="red", pch=1)
lines(clock, MRT_10F, type="o", col="purple", pch=2)
lines(clock, MRT_15F, type="o", col="pink", pch=5)
lines(clock, MRT_sem_F, type="o", col="green", pch=4)

legend("topright",
       pch=c(4,11,1,2,5,4),
       col=c("black", "yellow", "red", "purple", "pink", "green"),
       legend=c("1 Fog", "3 Fog", "5 Fog", "10 Fog", "15 Fog", "w/o Fog"))

values <- matrix(c(MRT_sem_F, MRT_1F), nrow=2, ncol=7, byrow=T)
barplot(values,
        log="y",
        xlab="Time between Things requests",
        ylab="Response time (s)",
        names.arg = clock,
        col=c("#E6E6E6", "#666666"),
        beside=T)

legend("topright",
       pch=c(15,15),
       col=c("#E6E6E6", "#666666"),
       legend=c("w/o Fog","1 Fog"))

values <- matrix(c(MRT_sem_F, MRT_3F), nrow=2, ncol=7, byrow=T)
barplot(values,
        log="y",
        xlab="Time between Things requests",
        ylab="Response time (s)",
        names.arg = clock,
        col=c("#E6E6E6", "#666666"),
        beside=T)

legend("topright",
       pch=c(15,15),
       col=c("#E6E6E6", "#666666"),
       legend=c("w/o Fog","3 Fog"))

values <- matrix(c(MRT_sem_F, MRT_5F), nrow=2, ncol=7, byrow=T)
barplot(values,
        log="y",
        xlab="Time between Things requests",
        ylab="Response time (s)",
        names.arg = clock,
        col=c("#E6E6E6", "#666666"),
        beside=T)

legend("topright",
       pch=c(15,15),
       col=c("#E6E6E6", "#666666"),
       legend=c("w/o Fog","5 Fog"))

values <- matrix(c(MRT_sem_F, MRT_10F), nrow=2, ncol=7, byrow=T)
barplot(values,
        log="y",
        xlab="Time between Things requests",
        ylab="Response time (s)",
        names.arg = clock,
        col=c("#E6E6E6", "#666666"),
        beside=T)

legend("topright",
       pch=c(15,15),
       col=c("#E6E6E6", "#666666"),
       legend=c("w/o Fog","10 Fog"))

values <- matrix(c(MRT_sem_F, MRT_15F), nrow=2, ncol=7, byrow=T)
barplot(values,
        log="y",
        xlab="Time between Things requests",
        ylab="Response time (s)",
        names.arg = clock,
        col=c("#E6E6E6", "#666666"),
        beside=T)

legend("topright",
       pch=c(15,15),
       col=c("#E6E6E6", "#666666"),
       legend=c("w/o Fog","15 Fog"))



# questão 2 ---------------------------------------------------------------
price10 = c(53.8, 43.6, 2.6)
price20 = c(33.9, 54.2, 11.9)
price30 = c(2.6, 60.5, 36.8)
price40 = c(0, 21.4, 78.6)

categories = c("Good", "Very Good", "Excellent")
colors <- c("yellow", "green", "blue")

values = matrix(c(price10,price20,price30,price40), nrow = 3, ncol = 4)
barplot(values,
        names.arg=c("$10-19", "$20-29", "$30-39", "$40-49"),
        xlab="Preços",ylab="Percentual",
        col=colors,
        beside = F)
legend("right", pch=c(15,15,15), col=colors, legend=categories)


# questão 3 ---------------------------------------------------------------
library(dplyr)

View(airquality)
data = airquality %>% filter(Month==5) %>% select(Temp, Day)
data$Temp = (data$Temp-32)/1.8
dens = density(data$Temp)

hist(data$Temp,
     col="grey",
     main = "Histogramas das Temperaturas do Mês de Maio",
     xlab="Temperatura",
     ylab="Frequência",
     breaks=5,
     probability = T)
lines(dens)

# questão 4 ---------------------------------------------------------------
sales <- read.table("https://training-course-material.com/images/8/8f/Sales.txt",header=TRUE)
pct <- round(sales$SALES/sum(sales$SALES)*100)
lbls <- paste(pct, "%", sep="")

pie(sales$SALES,
    lbls,
    main="Porcentagem de Vendas por País",
    col = c("yellow","purple","grey","orange","blue","red"))

legend("topleft",
       legend=sales$COUNTRY,
       pch=15,
       col=c("yellow","purple","grey","orange","blue","red"))


# questão 5 ---------------------------------------------------------------
View(InsectSprays)

boxplot(InsectSprays$count ~ InsectSprays$spray,
        outline=F,
        xlab="Sprays",
        ylab="Contagem",
        main="Distribuição da Contagem de Insetos por Inseticida",
        col="yellow")

# Questão 6 ---------------------------------------------------------------
View(mtcars)
str(mtcars)

reg<-lm(mpg ~ wt, data = mtcars)
plot(mtcars$wt,
     mtcars$mpg,
     xlab="Peso",
     ylab="Milhas Percorridas",
     main="Peso/Milhas Percorridas",
     pch=16)
abline(reg, col = "blue")

# Questão 7 ---------------------------------------------------------------
monitoring01 = read.csv("monitoringCloudData/monitoringCloudData_0.1.csv")
monitoring05 = read.csv("monitoringCloudData/monitoringCloudData_0.5.csv")
monitoring1 = read.csv("monitoringCloudData/monitoringCloudData_1.csv")
monitoringNone = read.csv("monitoringCloudData/monitoringCloudData_NONE.csv")


plotar_grafico <- function(monitoring, name){
  for(i in grep("GB",monitoring$usedMemory)){
    aux <- monitoring$usedMemory[i]
    aux <- substr(aux,1,nchar(aux)-2)
    aux <- as.numeric(aux) * 1024
    monitoring$usedMemory[i] = paste(aux,"MB", sep="")
  }
  
  i = 1
  for(item in monitoring$usedMemory){
    aux <- substr(item,1,nchar(item)-2)
    aux <- as.numeric(aux)
    monitoring$usedMemory[i] <- aux
    i = i + 1
  }
  
  monitoring$currentTime <- as.POSIXlt(monitoring$currentTime, format = "%Y-%m-%d %H:%M:%OS")
  monitoring$currentTime <- difftime(monitoring$currentTime, monitoring$currentTime[1], units = "hours")
  plot(monitoring$currentTime,
       monitoring$usedMemory,
       type="l",
       xlab="Time (hour)",
       ylab="Used Memory (MB)",
       main=name, 
  )
}

layout(matrix(c(1,2,3,4), 2, 2, byrow = TRUE))
plotar_grafico(monitoringNone, "Memory Analysis (None Workload)")
plotar_grafico(monitoring01, "Memory Analysis (Workload of 0.1)")
plotar_grafico(monitoring05, "Memory Analysis (Workload of 0.5)")
plotar_grafico(monitoring1, "Memory Analysis (Workload of 1.0)")


