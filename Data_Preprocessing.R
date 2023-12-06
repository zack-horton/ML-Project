setwd("~/Documents/MIT/15.095_Machine_Learning/Project/ML-Project")

library(readr)
library(readxl)
library(tidyverse)
library(mltools)
library(data.table)


########### Abalone ###########
abalone <- read.csv("./data/abalone.csv")
abalone

abalone <- abalone %>%
                mutate(Sex = as.integer(Sex == "M"))


for (i in 1:ncol(abalone)){
    abalone[is.na(abalone[,i]), i] <- mean(abalone[,i], na.rm = TRUE)
}

abalone_z <- data.frame(abalone)
abalone_mm <- data.frame(abalone)

for (k in 1:ncol(abalone)) {
    if (k != 1) {
        mu <- mean(abalone[, k])
        sigma <- sd(abalone[, k])
        min <- min(abalone[, k])
        max <- max(abalone[, k])
        abalone_z[, k] <- ((abalone[, k] - mu) / sigma)
        abalone_mm[, k] <- ((abalone[, k] - min) / (max - min))
    }
}

write.csv(abalone, "./processed_data/abalone.csv", row.names=FALSE)
write.csv(abalone_z, "./normalized_data/abalone_z.csv", row.names = FALSE)
write.csv(abalone_mm, "./scaled_data/abalone_mm.csv", row.names = FALSE)

########### ###########

########### CC GEN ##############
cc_gen <- read.csv("./data/cc_general.csv")

cc_gen <- cc_gen %>%
            select(-c("CUST_ID"))

for(i in 1:ncol(cc_gen)){
    cc_gen[is.na(cc_gen[,i]), i] <- mean(cc_gen[,i], na.rm = TRUE)
}

cc_gen_z <- data.frame(cc_gen)
cc_gen_mm <- data.frame(cc_gen)

for (k in 1:ncol(cc_gen)) {
    mu <- mean(cc_gen[, k])
    sigma <- sd(cc_gen[, k])
    min <- min(cc_gen[, k])
    max <- max(cc_gen[, k])
    cc_gen_z[, k] <- ((cc_gen[, k] - mu) / sigma)
    cc_gen_mm[, k] <- ((cc_gen[, k] - min) / (max - min))
}

write.csv(cc_gen, "./processed_data/cc_general.csv", row.names=FALSE)
write.csv(cc_gen_z, "./normalized_data/cc_general_z.csv", row.names = FALSE)
write.csv(cc_gen_mm, "./scaled_data/cc_general_mm.csv", row.names = FALSE)
########### ###########


########### Concrete Data ###########
concrete <- read.csv("./data/concrete_data.csv")

for (i in 1:ncol(concrete)){
    concrete[is.na(concrete[,i]), i] <- mean(concrete[,i], na.rm = TRUE)
}

concrete_z <- data.frame(concrete)
concrete_mm <- data.frame(concrete)

for (k in 1:ncol(concrete)) {
    mu <- mean(concrete[, k])
    sigma <- sd(concrete[, k])
    min <- min(concrete[, k])
    max <- max(concrete[, k])
    concrete_z[, k] <- ((concrete[, k] - mu) / sigma)
    concrete_mm[, k] <- ((concrete[, k] - min) / (max - min))
}

write.csv(concrete, "./processed_data/concrete_data.csv", row.names=FALSE)
write.csv(concrete_z, "./normalized_data/concrete_data_z.csv", row.names = FALSE)
write.csv(concrete_mm, "./scaled_data/concrete_data_mm.csv", row.names = FALSE)
########### ###########

########### KC Housing ###########
kc_house <- read.csv("./data/kc_house_data.csv")
kc_house <- kc_house %>%
                select(-c(2, 1))
kc_house

for (i in 1:ncol(kc_house)){
    kc_house[is.na(kc_house[,i]), i] <- mean(kc_house[,i], na.rm = TRUE)
}

kc_house_z <- data.frame(kc_house)
kc_house_mm <- data.frame(kc_house)

for (k in 1:ncol(kc_house)) {
    mu <- mean(kc_house[, k])
    sigma <- sd(kc_house[, k])
    min <- min(kc_house[, k])
    max <- max(kc_house[, k])
    kc_house_z[, k] <- ((kc_house[, k] - mu) / sigma)
    kc_house_mm[, k] <- ((kc_house[, k] - min) / (max - min))
}

write.csv(kc_house, "./processed_data/kc_housing.csv", row.names=FALSE)
write.csv(kc_house_z, "./normalized_data/kc_housing_z.csv", row.names = FALSE)
write.csv(kc_house_mm, "./scaled_data/kc_housing_mm.csv", row.names = FALSE)
########### ###########


########### Similarity Prediction ###########
similarity <- read.csv("./data/similarity_prediction.csv")
similarity <- similarity %>%
                select(-c(3, 2, 1))
similarity$pair_type <- as.factor(similarity$pair_type)
similarity$target_name <- as.factor(similarity$target_name)

similarity <- as.data.frame(one_hot(as.data.table(similarity)))

colnames(similarity) = gsub(",", "_", colnames(similarity))
similarity

similarity_z <- data.frame(similarity)
similarity_mm <- data.frame(similarity)

for (k in 1:ncol(similarity)) {
    if ((k %in% c(4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14)) == FALSE) {
    mu <- mean(similarity[, k])
    sigma <- sd(similarity[, k])
    min <- min(similarity[, k])
    max <- max(similarity[, k])
    similarity_z[, k] <- ((similarity[, k] - mu) / sigma)
    similarity_mm[, k] <- ((similarity[, k] - min) / (max - min))
    }
}

write.csv(similarity, "./processed_data/similarity_prediction.csv", row.names=FALSE)
write.csv(similarity_z, "./normalized_data/similarity_prediction_z.csv", row.names = FALSE)
write.csv(similarity_mm, "./scaled_data/similarity_prediction_mm.csv", row.names = FALSE)

########### ###########