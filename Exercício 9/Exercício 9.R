V1 <- read.table("Pica-pau.txt", header=TRUE, sep = "", dec = ".")
Caracol_data <- read.csv("Snail_feeding.csv", header = T, strip.white = T, na.strings = "")
Caracol_data <- Caracol_data[,1:7]
str(Caracol_data)
unique(Caracol_data$Sex)
levels(Caracol_data$Sex)[2]<-"female"
levels(Caracol_data$Sex)[3]<-"male"
levels(Caracol_data$Sex)[3]<-"male"

Caracol_data$Distance <- as.character(Caracol_data$Distance)
Caracol_data$Distance <- as.numeric(Caracol_data$Distance)
which(is.na(Caracol_data$Distance))
Caracol_data[682, "Distance"] <- 0.58
Caracol_data[755, "Distance"] <- 0.356452
which(duplicated(Caracol_data))
index <- which(duplicated(Caracol_data))
Caracol_data <- Caracol_data[-index,]
summary(Caracol_data)
Caracol_data[which(Caracol_data$Depth>2),]
Caracol_data[8,6] <- 1.62

write.csv(Caracol_data, file = "Caracol_data_checked.csv", row.names = F)
########################################

download.file("https://www.dropbox.com/s/w4xv9urbowbig3s/catsM.csv?dl=0", destfile = "catsM.csv")

catsM <- read.csv("catsM.csv")
mean(catsM$Bwt)
str(catsM)
summary(catsM)
which(duplicated(catsM))

#####

Caracol_data_checked <- read.csv("Caracol_data_checked.csv")
summary(Caracol_data_checked)
mean(Caracol_data_checked$Depth)
sub <- subset(Caracol_data_checked,
              subset = Caracol_data_checked$Sex=="female" & Caracol_data_checked$Size=="small")

max(sub$Distance)
which(sub$Distance==1)

#####

Sparrows <- read.csv("Sparrows.csv")
Sparrows_teste <- read.table(file = "Sparrows.csv", header = TRUE, sep = ",")

sub <- subset(Sparrows, subset = Sparrows$Species=="SSTS")
summary(sub)

which(duplicated(Sparrows))

levels(Sparrows$Sex)
levels(Sparrows$Sex)[1] <- "Female"
levels(Sparrows$Sex)[2] <- "Female"
levels(Sparrows$Sex)[3] <- "Male"

sub1 <- subset(Sparrows, subset = Sparrows$Sex=="Male")
mean(sub1$Tarsus)
summary(sub1)
which(is.na(Sparrows$Wing))

Sparrows[64,"Wing"] <- 59
Sparrows[250,"Wing"] <- 56.5
Sparrows[806,"Wing"] <- 57
summary(Sparrows)
mean(Sparrows$Wing)

Sparrows_Ordenado<-Sparrows[order(Sparrows$Wing, Sparrows$Head),]
