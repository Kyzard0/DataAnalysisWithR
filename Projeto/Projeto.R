install.packages("dplyr")
install.packages("ggplot2")
install.packages("ggcorrplot")
install.packages("chemCal")
install.packages("visreg")
install.packages("caret")
library(dplyr)
library(ggplot2)
library(ggcorrplot)
library(visreg)
library(DT)

# 42669 aluguéis
dataset <- read.csv("AB_NYC_2019.csv", na.strings=c("","NA"))
View(dataset)
str(dataset)
summary(dataset)

#Substitui valores NA's para valores padrões
dataset$reviews_per_month[which(is.na(dataset$reviews_per_month))] <- 0
dataset$name[which(is.na(dataset$name))] <- "NoName"
dataset$host_name[which(is.na(dataset$host_name))] <- "NoName"
dataset$last_review[which(is.na(dataset$last_review))] <- "NotReviewed"

summary(dataset)
dataset <- select(dataset, -id, -host_id,-host_name, -name, -last_review, -latitude, -longitude) # Remove colunas não interessantes
dataset$room_type[which(dataset$room_type == "Entire home/apt")] <- "Entire home"


Prices <- dataset$price

# Verifica distribuição dos preços
hist(dataset$price)

# Verifica distribuição dos preços abaixo de 1000
hist(Prices[Prices<1000])

# Conta quantos preços acima de 1000 existem
price.count.1000 <- dataset$price[dataset$price > 1000]
length(price.count.1000) # A maioria dos preços é abaixo de 1000

# Remove dados com preço acima de 1000
dataset <- filter(dataset,price<1000)
summary(dataset)


# Verifica distribuição dos preços
Prices <- dataset$price[dataset$price<300]
hist(Prices)

dataset <- filter(dataset,price<300)
dataset <- filter(dataset,price>0)
hist(dataset$price)
summary(dataset)


# Verifica a quantidade alugueis por bairro em ordem decrescente
neighbourhood.count <- dataset %>% group_by(neighbourhood) %>% summarise(Count=n())
neighbourhood.count <- neighbourhood.count[order(neighbourhood.count$Count, decreasing = T),]
colnames(neighbourhood.count) <- c("Bairro", "Qtd")
datatable(neighbourhood.count)
barplot(neighbourhood.count$Count, 
        las=2,
        col = rainbow(10),
        names.arg = neighbourhood.count$neighbourhood,
        main = "Gráfico de Quatidade de Aluguéis por Bairro") #Plota o gráfico das palavra

# Verifica quais bairros têm mais que 200 alugueis
neighbourhood.count <- filter(neighbourhood.count, Count>200)
sum(neighbourhood.count$Count) # A maioria dos dados está inclusa
neighbourhood.mean <- dataset %>% group_by(neighbourhood) %>% summarise(Média=mean(price))
neighbourhood.mean <- neighbourhood.mean[order(neighbourhood.mean$Média, decreasing = T),]

neighbourhood_group.count <- dataset %>% group_by(neighbourhood_group) %>% summarise(Count=n()) #Brooklyn e Manhattan juntos possuiem 85% dos alugueis
datatable(neighbourhood_group.count)

neighbourhood_group.mean <- dataset %>% group_by(neighbourhood_group) %>% summarise(Mean=mean(price)) #Manhattan é o bairro mais caro e Bronx o mais barato
datatable(neighbourhood_group.mean)

# Visualiza distribuição de tipo de quartos por preço e bairro

subset <- dataset %>% select(neighbourhood_group, price, room_type)
ggplot(subset, aes(x = neighbourhood_group, 
                     y = price, 
                     color=room_type)) +
  geom_point() +
  labs(title = "Distribuição dos tipos de quartos por preço e bairro")


room_type.count <- dataset %>% group_by(room_type) %>% summarise(Count=n()) # A maioria dos alugueis são de casa inteira ou quartos privados
datatable(room_type.count)
room_type.mean <- dataset %>% group_by(room_type) %>% summarise(Mean=mean(price)) # Entire home é bem mais caro
datatable(room_type.mean)

summary(dataset$minimum_nights)# Valor máximo muito alto

# Verifica distribuição de noites mínimas
hist(dataset$minimum_nights)

hist(dataset$minimum_nights[dataset$minimum_nights<10])

length(dataset$minimum_nights[dataset$minimum_nights>30]) # Poucos dados tem noites mínimas maiores que 30

dataset$minimum_nights[dataset$minimum_nights>30] = 30 # Setamos o valor máximo de noites mínimas para 30

hist(dataset$minimum_nights[dataset$minimum_nights<30])


# Plota a matriz de correlação entre as colunas
cor.matriz <- cor(select(dataset, -neighbourhood, -neighbourhood_group, -room_type), use = "complete.obs") #Retira colunas não numéricas
round(cor.matriz, 2)
ggcorrplot(cor.matriz, hc.order = T, lab = T, type = "lower")


# Separa os dados em treino e teste
set.seed(100) 

index = sample(1:nrow(dataset), 0.7*nrow(dataset)) 

train = dataset[index,] # Create the training data 
test = dataset[-index,] # Create the test data

dim(train)
dim(test)


# Função para calcular as métricas RMSE e R-squared
eval_metrics = function(model, df, predictions, target){
  resids = df[,target] - predictions
  resids2 = resids**2
  N = length(predictions)
  r2 = as.character(round(summary(model)$r.squared, 2))
  adj_r2 = as.character(round(summary(model)$adj.r.squared, 2))
  cat("Adjusted R-squared: ",adj_r2, " ") #Adjusted R-squared
  cat("RMSE: ",as.character(round(sqrt(sum(resids2)/N), 2))) #RMSE
}

# Treina um modelo regressão linear para predizer os preços
modelo <- lm(price ~ neighbourhood_group + room_type + minimum_nights + availability_365, data = train)
summary(modelo1)

modelo2 <- lm(price ~ neighbourhood_group + neighbourhood +room_type + minimum_nights + availability_365, data = train)
summary(modelo2)

predictions = predict(modelo, newdata = train)
eval_metrics(modelo1, train, predictions, target = 'price')

predictions = predict(modelo, newdata = test)
eval_metrics(modelo1, test, predictions, target = 'price')

data_test <- data.frame(neighbourhood_group = "Manhattan", room_type= "Private room", minimum_nights = 10, availability_365 = 365)
predict.lm(modelo, data_test)

data_test <- data.frame(neighbourhood_group = "Manhattan", room_type= "Entire home", minimum_nights = 10, availability_365 = 365)
predict.lm(modelo, data_test)

data_test <- data.frame(neighbourhood_group = "Manhattan", room_type= "Shared room", minimum_nights = 10, availability_365 = 365)
predict.lm(modelo, data_test)

data_test <- data.frame(neighbourhood_group = "Bronx", room_type= "Entire home", minimum_nights = 10, availability_365 = 365)
predict.lm(modelo1, data_test)

data_test <- data.frame(neighbourhood_group = "Bronx", room_type= "Private room", minimum_nights = 10, availability_365 = 365)
predict.lm(modelo1, data_test)

data_test <- data.frame(neighbourhood_group = "Bronx", room_type= "Shared room", minimum_nights = 10, availability_365 = 365)
predict.lm(modelo1, data_test)


bronx <- dataset %>% filter(neighbourhood_group == "Bronx", room_type == "Entire home")
bronx100 <- bronx %>% filter(price > 100)

# Plota gráfico da regressão linear

visreg(modelo1, "neighbourhood_group", gg = TRUE)

visreg(modelo1, "room_type", gg = TRUE)

visreg(modelo1, "minimum_nights", gg = TRUE)



