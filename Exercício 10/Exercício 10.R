install.packages("dplyr")
library(dplyr)
#######
df=data.frame(Theoph)
select(df, Dose)

df %>%
select(Dose) %>%
filter(Dose>5)  

slice(df, 10:20)

filter(df,Dose>5,Time>mean(Time))

arrange(df, desc(Wt))

arrange(df,Wt, desc(Time))

mutate(df,tendencia=Time-mean(Time))

filter(df, conc==max(conc))

########

data1 <- read.csv("673598238_T_ONTIME_REPORTING.csv", sep = ",", quote ="\"")
data2 <- read.csv("L_UNIQUE_CARRIERS.csv_", sep = ",", quote ="\"")

data <- merge(data1, data2, by.x = "OP_UNIQUE_CARRIER", by.y = "Code")

#####################
install.packages("tidyr")
library(tidyr)

download.file("http://stat405.had.co.nz/data/tb.csv", destfile = "tb.csv")
tb = read.csv("tb.csv")

tb <- tb %>% gather(key = "Informacao", value = "Quantidade", 3:23)

tb <- tb %>% separate("Informacao", into = c("caso", "tipo", "sexofaixa"), sep = "_")

tb <- tb %>% separate("sexofaixa", into = c("sexo", "faixa"), sep = 1)

tbf <- filter(tb, sexo=="f")
aggregate(formula = Quantidade ~ faixa,
          FUN = sum,
          data = tbf)

tb%>%filter(faixa==2534,sexo=="f")%>%select(faixa)

tby <- aggregate(formula = Quantidade ~ year,
          FUN = sum,
          data = tb)

sum(tby$Quantidade[21:29])


