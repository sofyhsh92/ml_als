###script 12
#see the difference of plots of lable E, L, W

alsfrs_label <- mutate(alsfrs.w, label = label)
alsfrs_label[, 1] <- NULL
#---------------------------------------------------------
alsfrs.eq <- filter(alsfrs_label, label == "E")
alsfrs.eq["subject_id"] <- NULL
alsfrs.eq["label"] <- NULL
alsfrs <- alsfrs.eq[1, ]
alsfrs.eq <- alsfrs.eq[-1, ]
alsfrs.eq <- alsfrs.eq[sample(nrow(alsfrs.eq)), ]

for (i in 7:nrow(alsfrs.eq)) {
  x <- alsfrs.w[1, -1]
  y <- alsfrs.eq[i, ]
  
  x <- x[!is.na(y)]
  y <- y[!is.na(y)]
  x <- x+1
  plot(x = x, y = y, xlab = "ALSFRS_Delta", ylab = "ALSFRS",  xlim = c(0, 365), ylim = c(0,40) )
  fit.l <- lm(y ~ x)
  lines(x, fitted(fit.l), col = "blue")
  print(i)
  invisible(readline(prompt="Press [enter] to continue"))
}

#-------------------------------------------------------------
alsfrs.li <- filter(alsfrs_label, label == "L")
alsfrs.li["subject_id"] <- NULL
alsfrs.li["label"] <- NULL
alsfrs <- alsfrs.li[1, ]
alsfrs.li <- alsfrs.li[-1, ]
alsfrs.li <- alsfrs.li[sample(nrow(alsfrs.li)), ]

for (i in 1:nrow(alsfrs.li)) {
  x <- alsfrs.w[1, -1]
  y <- alsfrs.li[i, ]
  
  x <- x[!is.na(y)]
  y <- y[!is.na(y)]
  x <- x+1
  plot(x = x, y = y, xlab = "ALSFRS_Delta", ylab = "ALSFRS",  xlim = c(0, 365), ylim = c(0,40) )
  fit.l <- lm(y ~ x)
  lines(x, fitted(fit.l), col = "blue")
  print(i)
  invisible(readline(prompt="Press [enter] to continue"))
}

#------------------------------------------------------------------------
alsfrs.we <- filter(alsfrs_label, label == "W")
alsfrs.we["subject_id"] <- NULL
alsfrs.we["label"] <- NULL
alsfrs <- alsfrs.w[1, ]
alsfrs.we <- alsfrs.we[-1, ]
alsfrs.we <- alsfrs.we[sample(nrow(alsfrs.we)), ]

for (i in 1:nrow(alsfrs.we)) {
  x <- alsfrs.w[1, -1]
  y <- alsfrs.we[i, ]
  
  x <- x[!is.na(y)]
  y <- y[!is.na(y)]
  x <- x+1
  plot(x = x, y = y, xlab = "ALSFRS_Delta", ylab = "ALSFRS",  xlim = c(0, 365), ylim = c(0,40) )
  fit.w <- nlsLM(y ~ A*exp(1)^(-( ((c^2) * x) ^ b )), start = list(A = 40, c= 1, b = 0.1), control = nls.lm.control(maxiter = 100))
  lines(x, fitted(fit.w), col = "red")
  print(i)
  invisible(readline(prompt="Press [enter] to continue"))
}