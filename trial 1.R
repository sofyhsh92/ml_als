#instal dtw package for dtw time series clustering
install.packages("dtw")
library(dtw)
ls()
getwd()
setwd("C:/Users/user/Desktop/ml_als_data")
alsfrs <- read.csv("alsfrs.csv")

#data refinement to make time series data of ALSFRS
install.packages("dplyr")
library(dplyr)
head(alsfrs)
alsfrs_t <- select(alsfrs, subject_id, ALSFRS_Delta, ALSFRS_Total)
head(alsfrs_t1)
alsfrs_t1 <- na.omit(alsfrs_t)

#tidying ALSFRS timeseries
install.packages("tidyr")
alsfrs_t2 <- spread(alsfrs_t1, ALSFRS_Delta, ALSFRS_Total)

library(tidyr)

#omitting duplicates
alsfrs_t1 <- alsfrs_t1[-31371, ]
alsfrs_t1 <- alsfrs_t1[-30165, ]
alsfrs_t1 <- alsfrs_t1[-21648, ]
alsfrs_t1 <- alsfrs_t1[-1339, ]

#getting dtwDist
dtwDist(alsfrs_t2)
distance.matrix <- dist(alsfrs_t2, method="DTW")

#transposing data frame
alsfrs_t3 <- t(alsfrs_t2)
as.data.frame(alsfrs_t3)
transpose <- t(alsfrs_t2[ , -1])
str(transpose)
head(transpose)

#tidyng data
alsfrs_tidy <- spread(alsfrs_t1, subject_id, ALSFRS_Total)

#interpolating using imputeTS (linear interpolation of all NAs)
library(imputeTS)
alsfrs_ip <- na.interpolation(alsfrs_tidy)
#if all are NA, omit
alsfrs_inp <- subset(alsfrs_ip, select=colMeans(is.na(alsfrs_ip)) == 0) 
distMatrix <- dist(alsfrs_inp, method="DTW")

#hierarchical clustering
hc <- hclust(distMatrix, method="average")
plot(hc, labels=observedLabels, main="")
colnames(test1) <- test1[2, ]
test1 <- test1[-1, ]
dist.matrix <-dtwDist(test1[ , -1])

#too big a data... sample just 100 patients to see if it works
dist.matrix <- dist(test1[c(1:100), -1], method="DTW")
#takes quite a time
dim(dist.matrix)
class(dist.matrix)

#hierarchical clustering
hclust.out <- hclust(dist.matrix)
summary(hclust.out)
cutree(hclust.out, k = 3)
plot(hclust.out)
