#script3 : clustering by slopes
alsfrs.tidy2 <- read.csv("alsfrs.tidy2.csv")
alsfrs.tidy2 <- alsfrs.tidy2[, -1]

#see lengths of each patients' trial
c <- numeric(length = nrow(alsfrs.ori))
c[1] <- 0
for(i in 2:nrow(alsfrs.ori)) {
  n <- as.numeric(alsfrs.ori[1, !is.na(alsfrs.ori[i, ])])
  c[i] <- max(range(n)) - min(range(n))
  }

hist(c[-1])
boxplot(c[-1])
plot(density(c[-1]))

#see slopes
d <- numeric(length = nrow(alsfrs.ori))
slope <- numeric(length = nrow(alsfrs.ori))
d[1] <- 0
slope[1] <- 0
for(i in 2:nrow(alsfrs.ori)) {
  n <- !is.na(alsfrs.ori[i, ])
  d[i] <- as.numeric(alsfrs.ori[i, n][1]) - as.numeric(alsfrs.ori[i, n][sum(n)])
  slope[i] <- d[i] / c[i]
}

summary(slope)
hist(slope[-1], breaks = 100, xlim = c(-0.05, 0.2))
boxplot(slope[-1])

#----------------------------------------------------------------------------------------------------------

#see slopes in 0 ~ 365 days
alsfrs.365 <- select(alsfrs.ori, 12:377)
vec <- logical(length = nrow(alsfrs.365))
for(i in 1:nrow(alsfrs.365)){
  if (!(sum(alsfrs.365[i, ], na.rm = TRUE) == 0)){
    vec[i] <- TRUE
  }
}
alsfrs.365 <- alsfrs.365[vec, ]

t <- numeric(length = nrow(alsfrs.365))
t[1] <- 0
for(i in 2:nrow(alsfrs.365)) {
  n <- as.numeric(alsfrs.365[1, !is.na(alsfrs.365[i, ])])
  t[i] <- max(range(n)) - min(range(n))
}

q <- numeric(length = nrow(alsfrs.365))
slo <- numeric(length = nrow(alsfrs.365))
q[1] <- 0
slo[1] <- 0
for(i in 2:nrow(alsfrs.365)) {
  p <- !is.na(alsfrs.365[i, ])
  q[i] <- as.numeric(alsfrs.365[i, p][1]) - as.numeric(alsfrs.365[i, p][sum(p)])
  slo[i] <- q[i] * 28 / t[i]
}

summary(slo[-1])
hist(slo[-1], breaks = 300, xlim = c(-1, 6))
boxplot(slo[-1])
