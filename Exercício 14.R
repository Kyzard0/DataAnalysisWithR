suplemento <- c(15,18,20,14,16,19)
placebo <- c(16,12,15,15,14,18)

shapiro.test(suplemento)
shapiro.test(placebo)
var.test(suplemento, placebo)

t.test(suplemento, placebo, alternative="two.sided", var.equal = T)

#####################################
antes <- c(75,50,50,60,50,70)
depois <- c(85,75,70,65,60,90)

shapiro.test(antes)
shapiro.test(depois)
var.test(antes, depois)

t.test(antes, depois, paired=TRUE, var.equal = T, alternative="two.sided")

#####################################

notas <- c(6.2,9.2,7.5,6.8,8.3,9.5)
media <- 7
ns <- 0.1

shapiro.test(notas)

t.test(notas, mu=media, alternative="greater", conf.level = ns)

#####################################

tempos <- c(90,93,93,99,98,100,103,104,99,102)
media = 100

shapiro.test(tempos)

t.test(tempos, mu=media, alternative="less")  

#####################################
library(dplyr)

View(airquality) 
month5 <- airquality %>% filter(Month==5) %>% select(Month, Ozone)
month6 <- airquality %>% filter(Month==6) %>% select(Month, Ozone)

month5 <- month5[complete.cases(month5),]
month6 <- month6[complete.cases(month6),]

shapiro.test(month5$Ozone)
shapiro.test(month6$Ozone)

wilcox.test(month5$Ozone,month6$Ozone,alternative = "two.sided")

