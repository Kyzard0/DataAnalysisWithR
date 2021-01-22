y <- 5
mult <- function(x){
  return(x*y)
}
mult(10)
###########
USArrests
prisoes <- function(estados, tiposPrisoes){
  if(!all(estados %in% row.names(USArrests))){
    cat("Estado Inválido")
  }else if(!all(tiposPrisoes %in% colnames(USArrests))){
    cat("Tipo de Prisão Inválida")
  }else{
    result <- sum(rowSums(USArrests[estados,tiposPrisoes]))
    cat("O total de prisoes do estado(s) é", estados, result)
  }
}

prisoes(estados=c("Nevada", "Washington"), tiposPrisoes = c("UrbanPop", "Assault"))
prisoes(estados="Tennessee", tiposPrisoes=c("Rape","Murder"))
prisoes(estados=c("California ","Miami", "Arizona"), tiposPrisoes=("Assault"))
prisoes(estados=c("Pennsylvania","Mississippi", "Nebraska"), tiposPrisoes=c("Rape","UrbanPop","Assault"))
prisoes(estados=c("Vermont","Wisconsin", "Texas"), tiposPrisoes=c("Rape","Assalto"))
##############

minhasNotas_1 <- function(Exe_1=0, VA_1=0, Exe_2=0, Proj=0, VA_2=0, VA_3=0, Opt=1, threshold=7){
  Exe_1 <- ifelse(is.na(Exe_1), 0, Exe_1)
  nExe_1 <- mean(Exe_1)
  Exe_2 <- ifelse(is.na(Exe_2), 0, Exe_2)
  nExe_2 <- mean(Exe_2)
  notas <- c(nExe_1, VA_1, nExe_2, Proj, VA_2, VA_3)
  notas <- ifelse(is.na(notas), 0, notas)
  if (any(notas>10 | notas<0) | any(Exe_1>10 | Exe_1<0) | any(Exe_2>10 | Exe_2<0)){
    cat("Alguma nota possui valor inválido")
  }else if(length(VA_1)>1 | length(VA_2)>1 | length(Proj)>1 | length(VA_3)>1){
    cat("Quantidade de Notas Inválidas!")
  }else if (Opt==1){
    media <- (mean(notas[1])*5 + notas[2]*5)/10
    if (media >= threshold){
      cat("Média da 1VA:", media, " - Acima da Média")
    }else if (media<threshold & media>0){
      cat("Média da 1VA:", media, " - Abaixo da Média")
    }else{
      cat("Aluno não possui nota para a 1 VA")
    }
  }else if(Opt==2){
    media <- (mean(notas[3])*2 + notas[4]*5 + notas[5]*3)/10
    if (media >= threshold){
      cat("Média da 2VA:", media, " - Acima da Média")
    }else if (media<threshold & media>0){
      cat("Média da 2VA:", media, " - Abaixo da Média")
    }else{
      cat("Aluno não possui nota para a 2 VA")
    }
  }else if(Opt==3){
    nVA1 <- (notas[1]*5 + notas[2]*5)/10
    nVA2 <- (notas[3]*2 + notas[4]*5 + notas[5]*3)/10
    notas <- c(nVA1, nVA2, notas[6])
    media <- mean(notas[notas > min(notas)])
    if (media > threshold){
      cat(media, " -- Aprovado")
    }else if (media<threshold & media>0){
      cat(media, "-- Na Final!!")
    }else{
      cat("Aluno não possui nenhuma nota - Reprovado!!")
    }
  }else{
    cat("Opção Inválida")
  }
  
}

minhasNotas_1(NA,NA,c(7,9,10),c(7,8),7,10,3,8)
minhasNotas_1(Exe_1=c(10,9,7,1,11,10), VA_1=8, Exe_2=7, Proj=7, VA_2=7, VA_3=10, Opt=1, threshold=7)
minhasNotas_1(Exe_1=c(10,9,5,1,5,10), VA_1=8, Exe_2=7, Proj=7, VA_2=7, VA_3=10, Opt=1, threshold=7)
minhasNotas_1(Exe_1=NA, VA_1=8, Exe_2=c(10,1,5), Proj=10, VA_2=8, VA_3=10, Opt=2, threshold=8)
minhasNotas_1(Exe_1=c(10,9,7,1,NA,NA), VA_1=NA, Exe_2=7, Proj=7, VA_2=7, VA_3=10, Opt=1, threshold=7)
minhasNotas_1(Exe_1=NA, VA_1=8, Exe_2=NA, Proj=10, VA_2=8, VA_3=10, Opt=2, threshold=8)
minhasNotas_1(Exe_1=c(5,8), VA_1=8, Exe_2=c(10,1,5), Proj=10, VA_2=8, VA_3=2, Opt=3, threshold=8)
minhasNotas_1(Exe_1=NA, VA_1=8, Exe_2=c(10,1,5), Proj=10, VA_2=8, VA_3=10, Opt=3, threshold=8)
minhasNotas_1(Exe_1=c(5,8), VA_1=8, Exe_2=c(10,1,5), Proj=c(2,4), VA_2=8, VA_3=2, Opt=3, threshold=8)
minhasNotas_1(Exe_1=10, VA_1=NA, Exe_2=7, Proj=7, VA_2=7, VA_3=10, Opt=1, threshold=7)
###############


get.nota <- function(nota){
  if (nota>=9 & nota<=10){
    return("A")
  }else if (nota>=7.5 & nota<=8.9){
    return("B")
  }else if (nota>=6 & nota<=7.4){
    return("C")
  }else if (nota>=0 & nota<=5.9){
    return("D")
  }
}


minhasNotas_2 <- function(Exe_1=0, VA_1=0, Exe_2=0, Proj=0, VA_2=0, VA_3=0, Opt=1, threshold="C"){
  Exe_1 <- ifelse(is.na(Exe_1), 0, Exe_1)
  nExe_1 <- mean(Exe_1)
  Exe_2 <- ifelse(is.na(Exe_2), 0, Exe_2)
  nExe_2 <- mean(Exe_2)
  value.Notas <- c("D", "C", "B", "A")
  notas <- c(nExe_1, VA_1, nExe_2, Proj, VA_2, VA_3)
  notas <- ifelse(is.na(notas), 0, notas)
  if (any(notas>10 | notas<0) | any(Exe_1>10 | Exe_1<0) | any(Exe_2>10 | Exe_2<0)){
    cat("Alguma nota possui valor inválido")
  }else if(length(VA_1)>1 | length(VA_2)>1 | length(Proj)>1 | length(VA_3)>1){
    cat("Quantidade de Notas Inválidas!")
  }else if (Opt==1){
    media <- (mean(notas[1])*5 + notas[2]*5)/10
    nota <- get.nota(media)
    print(nota)
    if (which(value.Notas==nota) >= which(value.Notas == threshold)){
      cat("Média da 1VA:", nota, " - Acima da Média")
    }else if (which(value.Notas==nota) < which(value.Notas == threshold) & media>0){
      cat("Média da 1VA:", nota, " - Abaixo da Média")
    }else{
      cat("Aluno não possui nota para a 1 VA")
    }
  }else if(Opt==2){
    media <- (mean(notas[3])*2 + notas[4]*5 + notas[5]*3)/10
    nota <- get.nota(media)
    if (which(value.Notas==nota) >= which(value.Notas == threshold)){
      cat("Média da 2VA:", nota, " - Acima da Média")
    }else if (which(value.Notas==nota) < which(value.Notas == threshold) & media>0){
      cat("Média da 2VA:", nota, " - Abaixo da Média")
    }else{
      cat("Aluno não possui nota para a 2 VA")
    }
  }else if(Opt==3){
    nVA1 <- (notas[1]*5 + notas[2]*5)/10
    nVA2 <- (notas[3]*2 + notas[4]*5 + notas[5]*3)/10
    notas <- c(nVA1, nVA2, notas[6])
    media <- mean(notas[notas > min(notas)])
    nota <- get.nota(media)
    if (which(value.Notas==nota) >= which(value.Notas == threshold)){
      cat(nota, " -- Aprovado")
    }else if (which(value.Notas==nota) < which(value.Notas == threshold) & media>0){
      cat(nota, "-- Na Final!!")
    }else{
      cat("Aluno não possui nenhuma nota - Reprovado!!")
    }
  }else{
    cat("Opção Inválida")
  }
  
}

minhasNotas_2(NA,9,9,5,9,10,3,"A")

minhasNotas_2(Exe_1=c(8,9,NA), VA_1=5, Exe_2=9, Proj=5, VA_2=4, VA_3=1, Opt=1, threshold="C")
minhasNotas_2(Exe_1=c(8,9,10,9,9,8,6), VA_1=4, Exe_2=c(8,3,11,9,9,5), Proj=4, VA_2=4, VA_3=1, Opt=1, threshold="C")
minhasNotas_2(Exe_1=c(8,9,10,9,9,8,6), VA_1=7, Exe_2=c(8,3,10,9,9,5), Proj=8, VA_2=3, VA_3=1, Opt=3, threshold="B")
minhasNotas_2(Exe_1=c(8,9,10,9,9,8,6), VA_1=7, Exe_2=c(8,3,10,9,9,5), Proj=8, VA_2=8, VA_3=1, Opt=2, threshold="B")
minhasNotas_2(Exe_1=c(8,9,10,9,9,8,6), VA_1=7, Exe_2=c(8,3,10,9,9,5), Proj=5, VA_2=5, VA_3=10, Opt=4, threshold="B")
