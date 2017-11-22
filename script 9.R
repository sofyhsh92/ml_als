#script 9
#data for machine learning classification

alsfrs.90 <- alsfrs.w[, 1:91]

for(i in 2:nrow(alsfrs.90)) {
  if(sum(!is.na(alsfrs.90[i, ])) == 0) {
    print(i)
  }
}
#row 4446 has to be omitted

alsfrs.90 <- alsfrs.90[-4446, ]

min <- 0
for(i in 2:nrow(alsfrs.90)) {
  min[i] <- min(alsfrs.90[i, ], na.rm = TRUE)
}

max <- 0
for(i in 2:nrow(alsfrs.90)) {
  max[i] <- max(alsfrs.90[i, ], na.rm = TRUE)
}


data <- data.frame(alsfrs_min = min[-1], alsfrs_max = max[-1])
rownames(data) <- rownames(alsfrs.90)[-1]



