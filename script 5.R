##script 5. 171114

setwd("C:/Users/user/Desktop/ml_als")

alsfrs.ori <- read.csv("alsfrs.ori.csv")

alsfrs.365[, 1] <- NULL

#error due to all same ALSFRS
h <- rep(NA, nrow(alsfrs.365))
h[1] <- 1
for(i in 2:nrow(alsfrs.365)) {
  if (!(sum(!duplicated(alsfrs.365[i, ][!is.na(alsfrs.365[i, ])])) == 1)) {
    h[i] <- i
  }
}
h <- h[!is.na(h)]

alsfrs.365. <- alsfrs.365[h, ]


library(minpack.lm)

for (i in 5935:nrow(alsfrs.365.)) {

x <- alsfrs.365.[1, ]
y <- alsfrs.365.[i, ]

x <- x[!is.na(y)]
y <- y[!is.na(y)]
x <- x+1
fit <- nlsLM(y ~ A*exp(1)^(-( ((c^2) * x) ^ b )), start = list(A = 40, c= 1, b = 0.1), control = nls.lm.control(maxiter = 100))
rmse.w <- sqrt( fit$m$deviance() / length(x) )

}

# 점 다 똑같은거 빼고 나니 (365.)
#588, 1608, 1722, 5304 (점 3개)
#1701, 2236, 4875, 4893, 5519

#graph
plot(x = x, y = y, xlim = c(0, 365), ylim = c(0,40) )
lines(x, fitted(fit.w), col = "red")

#residual
fit$m$deviance()

#linear fitting
fit.l <- lm(y ~ x)

#graph
lines(x, fitted(fit.l), col = "blue")

#residuals
res.l <- summary(fit.l)$residuals
rmse.l <- sqrt(sum(res.l^2)/length(x))

############################################

#creating alsfrs.w
alsfrs.w <- alsfrs.365.[-5519, ]
alsfrs.w <- alsfrs.w[-5304, ]
alsfrs.w <- alsfrs.w[-4893, ]
alsfrs.w <- alsfrs.w[-4875, ]
alsfrs.w <- alsfrs.w[-2236, ]
alsfrs.w <- alsfrs.w[-1722, ]
alsfrs.w <- alsfrs.w[-1701, ]
alsfrs.w <- alsfrs.w[-1608, ]
alsfrs.w <- alsfrs.w[-588, ]


#creating label
label <- rep("W", nrow(alsfrs.w))
label[1] <- NA

library(minpack.lm)

for (i in 2:nrow(alsfrs.w)) {
  
  x <- alsfrs.w[1, ]
  y <- alsfrs.w[i, ]
  
  x <- x[!is.na(y)]
  y <- y[!is.na(y)]
  x <- x+1
  fit.w <- nlsLM(y ~ A*exp(1)^(-( ((c^2) * x) ^ b )), start = list(A = 40, c= 1, b = 0.1), control = nls.lm.control(maxiter = 100))
  rmse.w <- sqrt( fit.w$m$deviance() / length(x) )
  fit.l <- lm(y ~ x)
  res.l <- summary(fit.l)$residuals
  rmse.l <- sqrt(sum(res.l^2)/length(x))
  if (rmse.w > rmse.l) {
    label[i] <- "L"
  } else {
    label[i] <- "W"
  }
  
}

##GoF -> not Rsqure but standard error of regression (S) = standard error of estimate = rmse
# http://blog.minitab.com/blog/adventures-in-statistics-2/regression-analysis-how-to-interpret-s-the-standard-error-of-the-regression
# http://blog.minitab.com/blog/adventures-in-statistics-2/what-is-the-difference-between-linear-and-nonlinear-equations-in-regression-analysis
# http://blog.minitab.com/blog/adventures-in-statistics-2/why-is-there-no-r-squared-for-nonlinear-regression

table(label)
## out of 5923 patients, 1547 is W and 4376 is L


