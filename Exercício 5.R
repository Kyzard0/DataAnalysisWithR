#Data Frames

mouse.color <- c("purple", "red", "yellow", "brown")
mouse.weight <- c(23, 21, 18, 26)
mouse.info <- data.frame("colour"=mouse.color, "weight"=mouse.weight, stringsAsFactors = FALSE)
str(mouse.info)
mouse.info[4,1]

airquality <- airquality
min(airquality$Ozone[airquality$Month==5], na.rm = T)

airquality.subset <- subset(airquality, 
                            subset = airquality$Ozone > 25 & airquality$Temp<90)
mean(airquality.subset$Solar.R, na.rm = T)

airquality[complete.cases(airquality),]


genomas <- as.data.frame(read.csv("https://www.dropbox.com/s/vgh6qk395ck86fp/genomes.csv?dl=1"))
genomas.40 <- genomas[genomas$Chromosomes > 40, ]

genomas.plasm <- subset(genomas,
                        subset = genomas$Plasmids>0 & genomas$Chromosomes > 1)
genomas.groups <- genomas$Groups

cancer_stats <- as.data.frame(read.csv("https://www.dropbox.com/s/g97bsxeuu0tajkj/cancer_stats.csv?dl=1"))

cancer_stats.site <- subset(cancer_stats,
                            subset = cancer_stats$Female.Cases > cancer_stats$Male.Cases,
                            select = c("Site", "Class"))

survival_male <- cancer_stats$Male.Deaths/cancer_stats$Male.Cases
survival_female <- cancer_stats$Female.Deaths/cancer_stats$Female.Cases

cancer_stats <- cbind(cancer_stats, survival_male)
cancer_stats <- cbind(cancer_stats, survival_female)
minimum_male <- min(cancer_stats$survival_male, na.rm = T)
maximum_female <- max(cancer_stats$survival_female, na.rm = T)

cancer_stats[cancer_stats$survival_male == minimum, ]
cancer_stats[cancer_stats$survival_female == maximum_female, ]
