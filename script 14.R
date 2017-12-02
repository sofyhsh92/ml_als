### script 14 : new label

#creating new label

alsfrs <- read.csv("alsfrs.548.csv")
alsfrs[, 1] <- NULL
rownames(alsfrs) <- alsfrs[, 1]
alsfrs[, 1] <- NULL

rel.err.diff <- 0
b <- 0

#calculation of relative errors of Weibull(concave, convex) and linear

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
  b[i] <- as.numeric(fit.w$m$getPars()[3])
  
}

b_and_rel.err <- data.frame(cbind(subject_id = as.numeric(rownames(alsfrs)[-1]), b = b[-1], rel.err.diff = rel.err.diff[-1]))
quantile(rel.err.diff, 0.7)
plot(density(b_and_rel.err[,3]), main = "relative error difference (linear - Weibull)")
abline(v=0.06926119, col = "red")

#labeling based on density plot of relative error difference
label <- rep("E", nrow(alsfrs))
label[1] <- NA

for(i in 2:nrow(alsfrs)) {
  if (rel.err.diff[i] > 0.06926119) {
    label[i] <- "W"
  }
}

table(label)

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

# try classification
alsfrs_label <- b_and_rel.err[, c(1, 4)]

alsfrs_data_mm <- alsfrs_data_mm %>%
  left_join(alsfrs_data, by = "subject_id") %>%
  select(-c(Q1_start, Q2_start, Q3_start, Q4_start, Q5_start, Q6_start, Q7_start, Q8_start, Q9_start, Q10_start, Q1_last, Q2_last, Q3_last, Q4_last, Q5_last, Q6_last, Q7_last, Q8_last, Q9_last, Q10_last))

complete_data <- left_join(alsfrs_label, alsfrs_data_mm, by = "subject_id") %>%
  left_join(alshistory, by = "subject_id") %>%
  left_join(demographics, by = "subject_id") %>%
  left_join(riluzole, by = "subject_id") %>%
  left_join(vital_height, by = "subject_id") %>%
  left_join(vital_pressure, by = "subject_id") %>%
  left_join(vital_respiratory_rate, by = "subject_id") %>%
  left_join(vital_temperature, by = "subject_id") %>%
  left_join(vital_weight, by = "subject_id")

library(caret)

complete_data_conti <- complete_data[, -c(26, 30, 31)]

complete_data_conti_imp <- predict(preProcess(complete_data_conti[, -1], method=c("medianImpute")), complete_data_conti[, -1])

complete_data_imp <- left_join(mutate(complete_data_conti_imp, subject_id = complete_data_conti[, 1]), complete_data[, c(1, 26, 30, 31)], by = "subject_id")

imputed_omitted_data <- na.omit(complete_data_imp)

model <- train(
  label ~.,
  data = imputed_omitted_data, 
  method = "rf",
  tuneLength = 6,
  importance = TRUE,
  trControl = trainControl(method = "cv", number = 5, verboseIter = TRUE)
)
model
plot(model)

write.csv(complete_data, "data.csv")


#_________________________________________________________________________
xgb_grid_1 = expand.grid(
  nrounds = 1000,
  eta = c(0.01, 0.001, 0.0001),
  max_depth = c(2, 4, 6, 8, 10),
  gamma = 1,
  colsample_bytree = 0.5,
  min_child_weight = 5,
  subsample = 1
)

xgb_trcontrol_1 = trainControl(
  method = "cv",
  number = 5,
  verboseIter = TRUE
)

model2 <- train(
  label ~.,
  data = imputed_omitted_data, 
  method = "xgbTree",
  importance = TRUE,
  trControl = xgb_trcontrol_1,
  tuneGrid = xgb_grid_1
)

model2
plot(model2)
