### script 14 : new label

#creating new label

alsfrs.548 <- read.csv("alsfrs.548.csv")
rownames(alsfrs.548) <- alsfrs.548[, 1]
alsfrs.548[, 1] <- NULL

rel.err.diff <- 0

#calculation of relative errors of Weibull(concave, convex) and linear
library(minpack.lm)
for (i in 2:nrow(alsfrs.548)) {
  
  x <- alsfrs.548[1, ]
  y <- alsfrs.548[i, ]
  
  x <- x[!is.na(y)]
  y <- y[!is.na(y)]
  x <- x+1
  fit.cc <- nlsLM(y ~ A*exp(1)^(-( ((c^2) * x) ^ b )), start = list(A = 40, c= 1, b = 4), control = nls.lm.control(maxiter = 125), lower = c(-Inf, -Inf, 1.6))
  rmse.cc <- sqrt( fit.cc$m$deviance() / length(x) )
  fit.cv <- nlsLM(y ~ A*exp(1)^(-( ((c^2) * x) ^ b )), start = list(A = 40, c= 1, b = 0.1), control = nls.lm.control(maxiter = 125), upper = c(Inf, Inf, 1))
  rmse.cv <- sqrt( fit.cv$m$deviance() / length(x) )
  fit.l <- lm(y ~ x)
  res.l <- summary(fit.l)$residuals
  rmse.l <- sqrt(sum(res.l^2)/length(x))
  rmse.w <- min(rmse.cc, rmse.cv)
  rel.err.w <- rmse.w / sd(y)
  rel.err.l <- rmse.l / sd(y)
  rel.err.diff[i] <- rel.err.l - rel.err.w
  
}

as.numeric(fit.w$m$getPars()[3])

#labeling based on density plot of relative error difference
library(dplyr)
r.e.df <- data.frame(cbind(row.names(alsfrs.w), rel.err.diff))
r.e.df <- r.e.df[-1, ]
r.e.df[,2] <- rel.err.diff[-1]
r.e.df <- filter(r.e.df, rel.err.diff > -2)
boxplot(r.e.df[,2])
plot(density(r.e.df[,2]), main = "relative error difference (linear - Weibull)")
summary(r.e.df[,2])
abline(v=0.08, col = "red")
abline(v=-0.05, col = "blue")

label <- rep("W", nrow(alsfrs.w))
label[1] <- NA

for(i in 2:nrow(alsfrs.w)) {
  if (rel.err.diff[i] > 0.08) {
    label[i] <- "W"
  } else if (rel.err.diff[i] <= 0.08 & rel.err.diff[i] > -0.05) {
    label[i] <- "E"
  } else {
    label[i] <- "L"
  }
}

table(label)