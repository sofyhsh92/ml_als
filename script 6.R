## script 6

#get graphs for ppt
for (i in 97:nrow(alsfrs.w)) {
  
  x <- alsfrs.w[1, ]
  y <- alsfrs.w[i, ]
  
  x <- x[!is.na(y)]
  y <- y[!is.na(y)]
  x <- x+1
  fit.w <- nlsLM(y ~ A*exp(1)^(-( ((c^2) * x) ^ b )), start = list(A = 40, c= 1, b = 0.1), control = nls.lm.control(maxiter = 100))
  fit.l <- lm(y ~ x)
  plot(x = x, y = y, xlab = "ALSFRS_Delta", ylab = "ALSFRS", main = paste("patient ID :", sub("^X", "", row.names(alsfrs.w)[i]) ),  xlim = c(0, 365), ylim = c(0,40) )
  lines(x, fitted(fit.w), col = "red")
  lines(x, fitted(fit.l), col = "blue")
  print(i)
  print(label[i])
}

rmse.w <- sqrt( fit.w$m$deviance() / length(x) )
fit.l <- lm(y ~ x)
res.l <- summary(fit.l)$residuals
rmse.l <- sqrt(sum(res.l^2)/length(x))

rmse.l
rmse.w

#i = 135 for example

