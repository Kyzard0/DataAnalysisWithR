
elementos <- c("A","B","C","D","E")
x <- c(5,7,12,15,10)
dados <- data.frame("elementos"=elementos, "X"=x, stringsAsFactors = FALSE)

summary(dados)

var(dados$X)


p <- 2 / 5


# Questão 2
options(digits = 2)
sd <- 4250
erro <- 300
nc <- (1-0.9)/2
n <- ((qnorm(nc, lower.tail = F)*sd)/erro)^2

# Questão 3
sd <- 5
nc <- (1-0.955)/2
erro <- 1.5
N <- 1500
n <- (qnorm(nc, lower.tail = F)^2*sd^2*N)/((erro^2*(N-1)) + (qnorm(nc, lower.tail = F)^2*sd^2))

# Questão 4
sd <- (30-10)/4
nc <- (1-0.98)/2
erro <- 1
n <- ((qnorm(nc, lower.tail = F)*sd)/erro)^2

# Questão 5
sd <- (30-10)/4
nc <- (1-0.98)/2
erro <- 1
N <- 5000
n <- (qnorm(nc, lower.tail = F)^2*sd^2*N)/((erro^2*(N-1)) + (qnorm(nc, lower.tail = F)^2*sd^2))

# Questão 6
set.seed(100)
prob <- 1 - pnorm(30, 50, 10)

# Questão 7
prob <- pnorm(30, 50, 10)

# Questão 8
prob <- pnorm(1, lower.tail = F)
prob <- pnorm(-2)
prob <- pnorm(0)
prob <- pnorm(1.28, lower.tail = F)

# Questão 9
media <- 90
sd <- 21
?qnorm
z <- qnorm(0.9,media, sd)
prob <- pnorm(259,media, sd)

# Questão 10
N <- 100 
nc <- (1-0.95)/2
media <- 30.2
sd <- 3.8
erro <- sd/sqrt(N)

left <- media-(qnorm(nc, lower.tail = F)*erro)
right <- media+(qnorm(nc, lower.tail = F)*erro)

cat("[", left, "-", right, "]")

# Questão 11
N <- 100
media <- 658
sd <- 47
nc <- (1-0.90)/2
erro <- sd/sqrt(N)
left <- media-(qnorm(nc, lower.tail = F)*erro)
right <- media+(qnorm(nc, lower.tail = F)*erro)

# Questão 12
amostras <- c(4.37, 3.63, 2.78, 5.46, 2.18, 6.07, 3.24, 5.89, 4.86, 4.64)
media <- mean(amostras)
N <- 10
sd <- sd(amostras)
nc <- (1-0.90)/2
erro <- sd/sqrt(N)
left <- media-(qt(nc, df=N-1, lower.tail = F)*erro)
right <- media+(qt(nc, df=N-1, lower.tail = F)*erro)





