## script 19
#script to see what happens if we label using significance of linear model (failure)

setwd("C:/Users/user/Documents/ml_als")
library(dplyr)
library(tidyr)

## new data (length : 18 months, ALSFRS_Delta 0 ~ 548 // data points >= 6 // last point alsfrs_delta >= 180)
alsfrs.ori <- read.csv("alsfrs.ori.csv")
colnames(alsfrs.ori) <- alsfrs.ori[1, ]
alsfrs <- cbind(subject_id = alsfrs.ori[, 1], alsfrs.ori[, 13:561])
rm(alsfrs.ori)
#deleting "X" from each subject_id  
alsfrs$subject_id <- as.character(alsfrs$subject_id)
for (i in 2:nrow(alsfrs)) {
  alsfrs$subject_id[i] <- sub("^X", "", alsfrs$subject_id[i])
  
}
#data points >= 6
c <- rep(TRUE, nrow(alsfrs))
for(i in 2:nrow(alsfrs)) {
  if(sum(!is.na(alsfrs[i, ])) <= 6) {
    c[i] <- FALSE
  }
}
alsfrs <- alsfrs[c, ]

#last point alsfrs_delta >= 180
c <- rep(TRUE, nrow(alsfrs))
als180 <- cbind(subject_id = alsfrs[, 1], alsfrs[, 182:550])
for(i in 2:nrow(alsfrs)) {
  if(sum(!is.na(als180[i, ])) <= 1 ) {
    c[i] <- FALSE
  }
}
rm(als180)
alsfrs <- alsfrs[c, ]
rm(i)
rm(c)

rownames(alsfrs) <- alsfrs[, 1]
alsfrs[, 1] <- NULL

#error due to all same ALSFRS
h <- rep(NA, nrow(alsfrs))
h[1] <- 1
for(i in 2:nrow(alsfrs)) {
  if (!(sum(!duplicated(alsfrs[i, ][!is.na(alsfrs[i, ])])) == 1)) {
    h[i] <- i
  }
}
h <- h[!is.na(h)]

alsfrs <- alsfrs[h, ]

#calculation of relative errors of Weibull and linear
#omit i = 330, 2121, 4032, 4140, 4480 (singular gradient)
alsfrs <- alsfrs[-c(330, 2121, 4032, 4140, 4480), ]
b <- 0
rel.err.diff <- 0
p <- 0

#function to get p-value of F-statistic in lm
lmp <- function (modelobject) {
  if (class(modelobject) != "lm") stop("Not an object of class 'lm' ")
  f <- summary(modelobject)$fstatistic
  p <- pf(f[1],f[2],f[3],lower.tail=F)
  attributes(p) <- NULL
  return(p)
}

library(minpack.lm)
for (i in 2:nrow(alsfrs)) {
  
  x <- alsfrs[1, ]
  y <- alsfrs[i, ]
  
  x <- x[!is.na(y)]
  y <- y[!is.na(y)]
  x <- x+1
  fit.w <- nlsLM(y ~ A*exp(1)^(-( ((c^2) * x) ^ b )), start = list(A = 40, c= 1, b = 0.1), control = nls.lm.control(maxiter = 125))
  rmse.w <- sqrt( fit.w$m$deviance() / length(x) )
  fit.l <- lm(y ~ x)
  res.l <- summary(fit.l)$residuals
  rmse.l <- sqrt(sum(res.l^2)/length(x))
  rel.err.w <- rmse.w / sd(y)
  rel.err.l <- rmse.l / sd(y)
  rel.err.diff[i] <- rel.err.l - rel.err.w
  b[i] <- as.numeric(fit.w$m$getPars()[3])
  p[i] <- lmp(fit.l)
  
}

sum(p > 0.05) / length(p)
sum(p > 0.01) / length(p)


b_and_rel.err <- data.frame(cbind(subject_id = as.numeric(rownames(alsfrs)[-1]), b = b[-1], rel.err.diff = rel.err.diff[-1], p = p[-1]))

#labeling based on density plot of relative error difference
label <- rep("E", nrow(alsfrs))
label[1] <- NA

for(i in 2:nrow(alsfrs)) {
  if (p[i] > 0.05) {
    label[i] <- "W"
  }
}

table(label) / length(label)

alsfrs_label <- mutate(alsfrs, label = label)
alsfrs_label$subject_id <- rownames(alsfrs)

alsfrs.p <- alsfrs_label %>%
  filter(label == "E") %>%
  select(-label)

alsfrs.p$subject_id <- as.numeric(alsfrs.p$subject_id)

label <- rep("E", nrow(alsfrs.p))
label[1] <- NA

for(i in 2:nrow(alsfrs.p)) {
  if (alsfrs.p$p[i] > 0.005) {
    label[i] <- "W"
  }
}

table(label)

als_label <- mutate(alsfrs.p, label = label)
als_label$p <- NULL

b_and_rel.err <- semi_join(b_and_rel.err, alsfrs.p, by = "subject_id")

b_and_rel.err$label <- NULL
b_and_rel.err <- mutate(b_and_rel.err, label = label[-1])
plot(density(filter(b_and_rel.err, label == "W")$b), xlim = c(-1, 15), main = "b")
abline(v=1.55, col = "red")

for(i in 1:nrow(b_and_rel.err)){
  if(b_and_rel.err$label[i] == "W" & b_and_rel.err$b[i] > 1.55){
    b_and_rel.err$label[i] <- "W_concave"
  } else if (b_and_rel.err$label[i] == "W" & b_and_rel.err$b[i] <= 1.55){
    b_and_rel.err$label[i] <- "W_convex"
  }
}

table(b_and_rel.err$label)

alsfrs_label <- b_and_rel.err[, c(1, 4)]

rm(alsfrs)
rm(b_and_rel.err)
rm(fit.l)
rm(fit.w)
rm(b)
rm(h)
rm(i)
rm(label)
rm(rel.err.l)
rm(rel.err.w)
rm(rel.err.diff)
rm(res.l)
rm(rmse.l)
rm(rmse.w)
rm(x)
rm(y)