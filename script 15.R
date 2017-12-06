#script 15
#see the difference of plots of lable E, W_concave, W_convex

als_label <- mutate(alsfrs, label = c(NA, b_and_rel.err$label))

alsfrs.eq <- filter(als_label, label == "E")
alsfrs.eq["label"] <- NULL
alsfrs.x <- as.numeric(als_label[1, -1])
alsfrs.eq <- alsfrs.eq[-1, ]
alsfrs.eq <- alsfrs.eq[sample(nrow(alsfrs.eq)), ]

for (i in 1:nrow(alsfrs.eq)) {
  x <- alsfrs.x
  y <- alsfrs.eq[i, ]
  
  x <- x[!is.na(y)]
  y <- y[!is.na(y)]
  x <- x+1
  plot(x = x, y = y, xlab = "ALSFRS_Delta", ylab = "ALSFRS", main = paste("subject_id = ", rownames(alsfrs.eq)[i]), xlim = c(0, 548), ylim = c(0,40) )
  fit.l <- lm(y ~ x)
  lines(x, fitted(fit.l), col = "blue")
  print(i)
  invisible(readline(prompt="Press [enter] to continue"))
}

par(mfrow = c(2,2))

#------------------------------------------------------------------------
alsfrs.wcc <- filter(als_label, label == "W_concave")
alsfrs.wcc["label"] <- NULL
alsfrs.x <- as.numeric(als_label[1, -1])
alsfrs.wcc <- alsfrs.wcc[-1, ]
alsfrs.wcc <- alsfrs.wcc[sample(nrow(alsfrs.wcc)), ]

for (i in 1:nrow(alsfrs.wcc)) {
  x <- alsfrs.x
  y <- alsfrs.wcc[i, ]
  
  x <- x[!is.na(y)]
  y <- y[!is.na(y)]
  x <- x+1
  plot(x = x, y = y, xlab = "ALSFRS_Delta", ylab = "ALSFRS", main = paste("subject_id = ", rownames(alsfrs.eq)[i]),  xlim = c(0, 548), ylim = c(0,40) )
  fit.w <- nlsLM(y ~ A*exp(1)^(-( ((c^2) * x) ^ b )), start = list(A = 40, c= 1, b = 0.1), control = nls.lm.control(maxiter = 125))
  lines(x, fitted(fit.w), col = "green")
  print(i)
  fit.w$m$trace()
  invisible(readline(prompt="Press [enter] to continue"))
}

#__________________________________________________________________
alsfrs.wcv <- filter(als_label, label == "W_convex")
alsfrs.wcv["label"] <- NULL
alsfrs.x <- as.numeric(als_label[1, -1])
alsfrs.wcv <- alsfrs.wcv[-1, ]
alsfrs.wcv <- alsfrs.wcv[sample(nrow(alsfrs.wcv)), ]

for (i in 1:nrow(alsfrs.wcv)) {
  x <- alsfrs.x
  y <- alsfrs.wcv[i, ]
  
  x <- x[!is.na(y)]
  y <- y[!is.na(y)]
  x <- x+1
  plot(x = x, y = y, xlab = "ALSFRS_Delta", ylab = "ALSFRS", main = paste("subject_id = ", rownames(alsfrs.eq)[i]),  xlim = c(0, 548), ylim = c(0,40) )
  fit.w <- nlsLM(y ~ A*exp(1)^(-( ((c^2) * x) ^ b )), start = list(A = 40, c= 1, b = 0.1), control = nls.lm.control(maxiter = 125))
  lines(x, fitted(fit.w), col = "red")
  print(i)
  fit.w$m$trace()
  invisible(readline(prompt="Press [enter] to continue"))
}