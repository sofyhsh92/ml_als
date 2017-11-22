#set working directory
setwd("C:/Users/user/Desktop/ml_als_data")

#import data
alsfrs.raw <- read.csv("alsfrs.csv")

#convert ALSFRS_R to ALSFRS
for (i in 1:length(alsfrs.raw$ALSFRS_Total)) {
  if (is.na(alsfrs.raw$ALSFRS_Total[i])) {
    alsfrs.raw$ALSFRS_Total[i] <- sum(alsfrs.raw$Q1_Speech[i], alsfrs.raw$Q2_Salivation[i], alsfrs.raw$Q3_Swallowing[i], alsfrs.raw$Q4_Handwriting[i], alsfrs.raw$Q5a_Cutting_without_Gastrostomy[i], alsfrs.raw$Q5b_Cutting_with_Gastrostomy[i], alsfrs.raw$Q6_Dressing_and_Hygiene[i], alsfrs.raw$Q7_Turning_in_Bed[i], alsfrs.raw$Q8_Walking[i], alsfrs.raw$Q9_Climbing_Stairs[i], alsfrs.raw$R_1_Dyspnea[i], na.rm = TRUE)
  }
}

#select columns
library(dplyr)
alsfrs.sel <- select(alsfrs.raw, subject_id, ALSFRS_Delta, ALSFRS_Total)

#exclude subjects with NA values in ALSFRS_Delta or ALSFRS_Total
sum(!complete.cases(alsfrs.sel$ALSFRS_Delta))
# = 188
sum(!complete.cases(alsfrs.sel$ALSFRS_Total))
# = 0
n <- 0
for (i in 1:length(alsfrs.raw$ALSFRS_Delta)) {
  if (is.na(alsfrs.raw$ALSFRS_Delta[i])) {
    n[i] <- alsfrs.raw$subject_id[i]
  }
}
n <- n[!is.na(n)]
n <- unique(n)
alsfrs.sel2 <- filter(alsfrs.sel, !(subject_id %in% n))

#remove duplicate identifiers for rows (2196, 2197), (8391, 8392), (13035, 13036), (18213, 18214), (22792, 22793), (31348, 31349), (35525, 35526), (39314, 39315, 39316), (39751, 39752), (41977, 41978), (41980, 41981), (49768, 49769), (51652, 51653), (52229, 52230), (53648, 53649)
m <- c(2196, 8391, 13035, 18213, 22792, 31348, 35525, 39314, 39751, 41977, 41980, 49768, 51652, 52229, 53648)
alsfrs.sel3 <- filter(alsfrs.sel2, !(subject_id %in% subject_id[m]))

#total number of patients
length(unique(alsfrs.sel3$subject_id))

#tidy data
library(tidyr)
alsfrs.tidy <- spread(alsfrs.sel3, subject_id, ALSFRS_Total)

#select columns(patients) with more or equal to 3 alsfrs data
n <- 0
for (i in 1:ncol(alsfrs.tidy)) {
  if (sum(!is.na(alsfrs.tidy[ ,i])) >= 3)
  n[i] <- i
}
n <- n[!is.na(n)]
alsfrs.tidy2 <- alsfrs.tidy[n]
write.csv(alsfrs.tidy2, "alsfrs.tidy2.csv")

#impute
library(imputeTS)
alsfrs.imp <- na.interpolation(alsfrs.tidy2)
write.csv(alsfrs.imp, "alsfrs.imp.csv")

#transpose
alsfrs.tra <- as.data.frame(t(alsfrs.imp))
colnames(alsfrs.tra) <- alsfrs.tra[1, ]
alsfrs.tra <- alsfrs.tra[-1, ]

#distance matrix by DTW method [it took approximately 8 mins for just 100 rows]
library(dtw)
dist.matrix <- dist(alsfrs.tra[c(1:100), -1], method="DTW")

dim(dist.matrix)
class(dist.matrix)

#clustering (sample)
hclust.out <- hclust(dist.matrix)
summary(hclust.out)

plot(hclust.out)

group <- cutree(hclust.out, k = 4)

#original (not imputed)
alsfrs.ori <- as.data.frame(t(alsfrs.tidy2))
colnames(alsfrs.ori) <- alsfrs.ori[1, ]
alsfrs.ori <- alsfrs.ori[-1, ]

#sample with subgroup
sample <- cbind(alsfrs.ori[c(1:100), ], group)

#--------------------------------------------------
presample1 <- sample %>% 
  filter(group == 1) %>%
  select(- group)

sample1 <- as.data.frame(t(presample1))

length(sample1)

#--------------------------------------------------
presample2 <- sample %>% 
  filter(group == 2) %>%
  select(- group)

sample2 <- as.data.frame(t(presample2))

length(sample2)

#--------------------------------------------------
presample3 <- sample %>% 
  filter(group == 3) %>%
  select(- group)

sample3 <- as.data.frame(t(presample3))

length(sample3)

#--------------------------------------------------
presample4 <- sample %>% 
  filter(group == 4) %>%
  select(- group)

sample4 <- as.data.frame(t(presample4))

length(sample4)

#--------SAMPLE1 IMAGE------------------------------------------
par(mfrow = c(3,3))

plot(sample1$V5, ylim = c(0,40), xlab = "ALSFRS_Delta", ylab = "ALSFRS")
plot(sample1$V10, ylim = c(0,40), xlab = "ALSFRS_Delta", ylab = "ALSFRS")
plot(sample1$V15, ylim = c(0,40), xlab = "ALSFRS_Delta", ylab = "ALSFRS")
plot(sample1$V20, ylim = c(0,40), xlab = "ALSFRS_Delta", ylab = "ALSFRS")
plot(sample1$V25, ylim = c(0,40), xlab = "ALSFRS_Delta", ylab = "ALSFRS")
plot(sample1$V30, ylim = c(0,40), xlab = "ALSFRS_Delta", ylab = "ALSFRS")
plot(sample1$V35, ylim = c(0,40), xlab = "ALSFRS_Delta", ylab = "ALSFRS")
plot(sample1$V40, ylim = c(0,40), xlab = "ALSFRS_Delta", ylab = "ALSFRS")
plot(sample1$V45, ylim = c(0,40), xlab = "ALSFRS_Delta", ylab = "ALSFRS")

#--------SAMPLE2 IMAGE------------------------------------------
par(mfrow = c(3,3))

plot(sample2$V3, ylim = c(0,40), xlab = "ALSFRS_Delta", ylab = "ALSFRS")
plot(sample2$V6, ylim = c(0,40), xlab = "ALSFRS_Delta", ylab = "ALSFRS")
plot(sample2$V9, ylim = c(0,40), xlab = "ALSFRS_Delta", ylab = "ALSFRS")
plot(sample2$V12, ylim = c(0,40), xlab = "ALSFRS_Delta", ylab = "ALSFRS")
plot(sample2$V15, ylim = c(0,40), xlab = "ALSFRS_Delta", ylab = "ALSFRS")
plot(sample2$V2, ylim = c(0,40), xlab = "ALSFRS_Delta", ylab = "ALSFRS")
plot(sample2$V4, ylim = c(0,40), xlab = "ALSFRS_Delta", ylab = "ALSFRS")
plot(sample2$V10, ylim = c(0,40), xlab = "ALSFRS_Delta", ylab = "ALSFRS")
plot(sample2$V14, ylim = c(0,40), xlab = "ALSFRS_Delta", ylab = "ALSFRS")

#--------SAMPLE3 IMAGE------------------------------------------
par(mfrow = c(3,3))

plot(sample3$V3, ylim = c(0,40), xlab = "ALSFRS_Delta", ylab = "ALSFRS")
plot(sample3$V6, ylim = c(0,40), xlab = "ALSFRS_Delta", ylab = "ALSFRS")
plot(sample3$V9, ylim = c(0,40), xlab = "ALSFRS_Delta", ylab = "ALSFRS")
plot(sample3$V12, ylim = c(0,40), xlab = "ALSFRS_Delta", ylab = "ALSFRS")
plot(sample3$V15, ylim = c(0,40), xlab = "ALSFRS_Delta", ylab = "ALSFRS")
plot(sample3$V18, ylim = c(0,40), xlab = "ALSFRS_Delta", ylab = "ALSFRS")
plot(sample3$V21, ylim = c(0,40), xlab = "ALSFRS_Delta", ylab = "ALSFRS")
plot(sample3$V24, ylim = c(0,40), xlab = "ALSFRS_Delta", ylab = "ALSFRS")
plot(sample3$V27, ylim = c(0,40), xlab = "ALSFRS_Delta", ylab = "ALSFRS")

#--------SAMPLE4 IMAGE------------------------------------------
par(mfrow = c(3,3))

plot(sample4$V1, ylim = c(0,40), xlab = "ALSFRS_Delta", ylab = "ALSFRS")
plot(sample4$V2, ylim = c(0,40), xlab = "ALSFRS_Delta", ylab = "ALSFRS")
plot(sample4$V3, ylim = c(0,40), xlab = "ALSFRS_Delta", ylab = "ALSFRS")

#regression with linear model & Weibull model
ALSFRS_Delta <- as.numeric(rownames(alsfrs.tidy2))

#--------SAMPLE1------------------------------------------
rs <- 0
for (i in sample1){
  reg <- lm(i ~ ALSFRS_Delta, sample1)
  rs <- rs + summary(reg)$r.squared
}
sample1.lm.mean = rs / length(sample1)

#Weibull cannot be implemented by glm function
#need to have a look at gamlss package
#abort mission for now
