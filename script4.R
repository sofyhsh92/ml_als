#script 4

#optimization using nls
x <- alsfrs.ori[1, ]
y <- alsfrs.ori[5, ]

x <- x[!is.na(y)]
y <- y[!is.na(y)]
x <- x+1
x <- x/28

plot(x, y)

fit <- nls(y ~ A*exp(1)^(-(x^b)), start = list(A = 40, b = 0.1))
summary(fit)
fit$m$deviance()

#optimization using optim (maybe later)

dat = data.frame(x, y)
SOS = function (data, para) {
  with(data, sum((y-para[1]*exp(1)^(-(x^para[2])^2))))
}
result <- optim(para = c(30, 1), SOS, data = dat)
result

plot(y ~ x, data = dat, main="Least square regression")
abline(a = result$par[1], b = result$par[2], col = "red")

#error due to all same ALSFRS
h <- rep(NA, nrow(alsfrs.ori))
h[1] <- 1
for(i in 2:nrow(alsfrs.ori)) {
  if (!(sum(!duplicated(alsfrs.ori[i, ][!is.na(alsfrs.ori[i, ])])) == 1)) {
    h[i] <- i
  }
}
h <- h[!is.na(h)]

alsfrs.ori2 <- alsfrs.ori[h, ]

#Weibull model fitting and mean of sum of squares
sos.wei <- 0
for (i in 1624:nrow(alsfrs.ori2)) {
  x <- alsfrs.ori2[1, ]
  y <- alsfrs.ori2[i, ]
  
  x <- x[!is.na(y)]
  y <- y[!is.na(y)]
  x <- x+1
  x <- x/28
  fit <- nls(y ~ A*exp(1)^(-(x^b)), start = list(A = 40, b = 0.1))
  print(i)
  sos.wei <- sos.wei + fit$m$deviance()
}

# Error
#207 326 717 1425 1574 1623 1749 ... quit

#try with 365 model
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

#Weibull model fitting and mean of sum of squares
sos.wei <- 0
for (i in 5624:nrow(alsfrs.365.)) {
  x <- alsfrs.365.[1, ]
  y <- alsfrs.365.[i, ]
  
  x <- x[!is.na(y)]
  y <- y[!is.na(y)]
  x <- x+1
  x <- x/28
  fit <- nls(y ~ A*exp(1)^(-(x^b)), start = list(A = 40, b = 0.1))
  print(i)
  sos.wei <- sos.wei + fit$m$deviance()
}

#error 716 2462 4423 5213 5623
#지정 최대 반복수 초과 4048 4379 5269 5544

alsfrs.365.c <- alsfrs.365.[-5623, ]
alsfrs.365.c <- alsfrs.365.c[-5544, ]
alsfrs.365.c <- alsfrs.365.c[-5269, ]
alsfrs.365.c <- alsfrs.365.c[-5213, ]
alsfrs.365.c <- alsfrs.365.c[-4423, ]
alsfrs.365.c <- alsfrs.365.c[-4379, ]
alsfrs.365.c <- alsfrs.365.c[-4048, ]
alsfrs.365.c <- alsfrs.365.c[-2462, ]
alsfrs.365.c <- alsfrs.365.c[-716, ]

#try with 365 model clean data
#Weibull model fitting and mean of sum of squares
sos.wei <- 0
for (i in 2:nrow(alsfrs.365.c)) {
  x <- alsfrs.365.c[1, ]
  y <- alsfrs.365.c[i, ]
  
  x <- x[!is.na(y)]
  y <- y[!is.na(y)]
  x <- x+1
  fit <- nls(y ~ A*exp(1)^(-(x^b)), start = list(A = 40, b = 0.1))
  sos.wei <- sos.wei + fit$m$deviance()
}
#sos.wei <- 268921.032188645

#linear model fitting and sum of sum of squares
sos.lin <- 0
for (i in 2:nrow(alsfrs.365.c)) {
  x <- alsfrs.365.c[1, ]
  y <- alsfrs.365.c[i, ]
  
  x <- x[!is.na(y)]
  y <- y[!is.na(y)]
  x <- x+1
  fit <- nls(y ~ A*exp(1)^(-(x^b)), start = list(A = 40, b = 0.1))
  sos.wei <- sos.wei + fit$m$deviance()
}

write.csv(alsfrs.365., "alsfrs.365..csv")
write.csv(alsfrs.365.c, "alsfrs.365.c.csv")
