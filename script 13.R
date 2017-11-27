###script 13
## new data (length : 18 months, ALSFRS_Delta 0 ~ 548 // data points >= 6 // last point alsfrs_delta >= 180)
alsfrs.ori <- read.csv("alsfrs.ori.csv")
colnames(alsfrs.ori) <- alsfrs.ori[1, ]
alsfrs <- cbind(subject_id = alsfrs.ori[, 1], alsfrs.ori[, 13:561])
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

##creating label
alsfrs.save <- alsfrs
write.csv(alsfrs.save, "alsfrs.548.csv")

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
rel.err.diff <- 0
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
  
}

#labeling based on density plot of relative error difference
library(dplyr)
r.e.df <- data.frame(cbind(row.names(alsfrs), rel.err.diff))
r.e.df <- r.e.df[-1, ]
r.e.df[,2] <- rel.err.diff[-1]
r.e.df <- filter(r.e.df, rel.err.diff > -2)
boxplot(r.e.df[,2])
plot(density(r.e.df[,2]), main = "relative error difference (linear - Weibull)")
summary(r.e.df[,2])
abline(v=0.085421, col = "red")

label <- rep("E", nrow(alsfrs))
label[1] <- NA

for(i in 2:nrow(alsfrs)) {
  if (rel.err.diff[i] > 0.085421) {
    label[i] <- "W"
  }
}

table(label)

als_label <- cbind(alsfrs, label)

#see the difference of plots of lable E, W
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
  plot(x = x, y = y, xlab = "ALSFRS_Delta", ylab = "ALSFRS",  xlim = c(0, 548), ylim = c(0,40) )
  fit.l <- lm(y ~ x)
  lines(x, fitted(fit.l), col = "blue")
  print(i)
  invisible(readline(prompt="Press [enter] to continue"))
}

#------------------------------------------------------------------------
alsfrs.we <- filter(als_label, label == "W")
alsfrs.we["label"] <- NULL
alsfrs.x <- as.numeric(als_label[1, -1])
alsfrs.we <- alsfrs.we[-1, ]
alsfrs.we <- alsfrs.we[sample(nrow(alsfrs.we)), ]

for (i in 81:nrow(alsfrs.we)) {
  x <- alsfrs.x
  y <- alsfrs.we[i, ]
  
  x <- x[!is.na(y)]
  y <- y[!is.na(y)]
  x <- x+1
  plot(x = x, y = y, xlab = "ALSFRS_Delta", ylab = "ALSFRS",  xlim = c(0, 548), ylim = c(0,40) )
  fit.w <- nlsLM(y ~ A*exp(1)^(-( ((c^2) * x) ^ b )), start = list(A = 40, c= 1, b = 0.1), control = nls.lm.control(maxiter = 125))
  lines(x, fitted(fit.w), col = "red")
  print(i)
  fit.w$m$trace()
  invisible(readline(prompt="Press [enter] to continue"))
}

##modify alsfrs
alsfrs <- read.csv("alsfrs.csv")
#merge Q5a and Q5b
for(i in 1:nrow(alsfrs)) {
  if(is.na(alsfrs$Q5a_Cutting_without_Gastrostomy[i])) {
    alsfrs$Q5a_Cutting_without_Gastrostomy[i] <- alsfrs$Q5b_Cutting_with_Gastrostomy[i]
  }
}
#drop Q5b
alsfrs$Q5b_Cutting_with_Gastrostomy <- NULL
#merge Q10 and R1
for(i in 1:nrow(alsfrs)) {
  if(is.na(alsfrs$Q10_Respiratory[i])) {
    alsfrs$Q10_Respiratory[i] <- alsfrs$R_1_Dyspnea[i]
  }
}
#drop everything except Q1~Q10 and ALSFRS_Delta
alsfrs[ , 13:19] <- NULL

#filter for 0~90 ALSFRS_Delta
library(dplyr)
alsfrs <- filter(alsfrs, ALSFRS_Delta <= 90 & ALSFRS_Delta >= 0)
alsfrs <- na.omit(alsfrs)
length(unique(alsfrs$subject_id))
#6506 patients
colnames(alsfrs) <- c("subject_id", "Q1", "Q2", "Q3", "Q4", "Q5", "Q6", "Q7", "Q8", "Q9", "Q10", "ALSFRS_Delta")
alsfrs <- alsfrs %>%
  group_by(subject_id) %>%
  mutate(rank = rank(ALSFRS_Delta)) %>%
  filter(rank == 1) %>%
  mutate(alsfrs_total = Q1+Q2+Q3+Q4+Q5+Q6+Q7+Q8+Q9+Q10) %>%
  left_join(alshistory, by = "subject_id") %>%
  mutate(preslope = alsfrs_total * 30 / (-Onset_Delta + ALSFRS_Delta) ) %>%
  mutate(movement = (Q8 <=1 | Q6 <=1),
         swallowing = (Q3 <=1),
         communicating = (Q1 <= 1 | Q4 <= 1),
         breathing = (Q10 <= 1)) %>%
  mutate(mitos = movement + swallowing + communicating + breathing) %>%
  select(subject_id, Q1, Q2, Q3, Q4, Q5, Q6, Q7, Q8, Q9, Q10, alsfrs_total, mitos, preslope)
#done(alsfrs : Q1 ~ Q10, alsfrs_total)

#-----------------------------------------------------------------------------------------------

#try machine learning classification - data merging
alsfrs_label <- als_label[-1, ]
alsfrs_label <- mutate(alsfrs_label, subject_id = rownames(alsfrs_label))
alsfrs_label <- select(alsfrs_label, subject_id, label)
alsfrs_label$subject_id <- as.integer(alsfrs_label$subject_id)

complete_data <- left_join(alsfrs_label, alsfrs, by = "subject_id") %>%
  left_join(alshistory, by = "subject_id") %>%
  left_join(demographics, by = "subject_id") %>%
  left_join(riluzole, by = "subject_id") %>%
  left_join(vital_height, by = "subject_id") %>%
  left_join(vital_pressure, by = "subject_id") %>%
  left_join(vital_respiratory_rate, by = "subject_id") %>%
  left_join(vital_temperature, by = "subject_id") %>%
  left_join(vital_weight, by = "subject_id")

install.packages("caret")
library(caret)

complete_data_some <- complete_data[, -c(23, 27, 28)]

predict(preProcess(complete_data[-1, ], method=c("knnImpute")), complete_data[-1, ])
model <- train(
  label ~.,
  data = predict(preProcess(complete_data_some[-1, ], method=c("medianImpute")), complete_data_some[-1, ]), 
  method = "ranger",
  preProcess = "medianImpute",
  trControl = trainControl(method = "cv", number = 5, verboseIter = TRUE)
)
model
plot(model)