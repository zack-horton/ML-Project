setwd("~/Documents/MIT/15.095_Machine_Learning/Project/ML-Project")

library(readr)
library(readxl)
library(tidyverse)
library(mltools)


########### Abalone ###########
abalone <- read.csv("./data/abalone.csv")
abalone

abalone <- abalone %>%
                mutate(Sex = as.integer(Sex == "M"))
abalone

for (k in 1:ncol(abalone)) {
    if (k != 1) {
        mu <- mean(abalone[, k])
        sigma <- sd(abalone[, k])
        abalone[, k] <- ((abalone[, k] - mu) / sigma)
    }
}

for(i in 1:ncol(abalone)){
    abalone[is.na(abalone[,i]), i] <- mean(abalone[,i], na.rm = TRUE)
}

write.csv(abalone, "./final_data/abalone.csv", row.names = FALSE)
########### ###########

########### CC GEN ##############
cc_gen <- read.csv("./data/cc_general.csv")

cc_gen <- cc_gen %>%
            select(-c("CUST_ID"))

for(i in 1:ncol(cc_gen)){
    cc_gen[is.na(cc_gen[,i]), i] <- mean(cc_gen[,i], na.rm = TRUE)
}

cc_gen

for (k in 1:ncol(cc_gen)) {
    mu <- mean(cc_gen[, k])
    sigma <- sd(cc_gen[, k])
    cc_gen[, k] <- ((cc_gen[, k] - mu) / sigma)
}

write.csv(cc_gen, "./final_data/cc_general.csv", row.names = FALSE)
########### ###########


########### Concrete Data ###########
concrete <- read.csv("./data/concrete_data.csv")

for (i in 1:ncol(concrete)){
    concrete[is.na(concrete[,i]), i] <- mean(concrete[,i], na.rm = TRUE)
}

concrete

for (k in 1:ncol(concrete)) {
        mu <- mean(concrete[, k])
        sigma <- sd(concrete[, k])
        concrete[, k] <- ((concrete[, k] - mu) / sigma)
}

write.csv(concrete, "./final_data/concrete_data.csv", row.names = FALSE)
########### ###########

########### KC Housing ###########
kc_house <- read.csv("./data/kc_house_data.csv")
kc_house <- kc_house %>%
                select(-c(2, 1))
kc_house

for (i in 1:ncol(kc_house)){
    kc_house[is.na(kc_house[,i]), i] <- mean(kc_house[,i], na.rm = TRUE)
}

for (k in 1:ncol(kc_house)) {
    mu <- mean(kc_house[, k])
    sigma <- sd(kc_house[, k])
    kc_house[, k] <- ((kc_house[, k] - mu) / sigma)
}

write.csv(kc_house, "./final_data/kc_housing.csv", row.names = FALSE)
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

for (k in 1:ncol(similarity)) {
    if ((k %in% c(4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14)) == FALSE) {
    mu <- mean(similarity[, k])
    sigma <- sd(similarity[, k])
    similarity[, k] <- ((similarity[, k] - mu) / sigma)
    }
}

write.csv(similarity, "./final_data/similarity_prediction.csv", row.names = FALSE)

########### ###########