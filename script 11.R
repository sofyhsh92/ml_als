#script 11

###Machine Learning Classification

##label
#from script 8, we get label 
alsfrs_label <- cbind(alsfrs.w, label)
alsfrs_label <- mutate(alsfrs_label, subject_id = row.names(alsfrs.w))
alsfrs_label <- alsfrs_label[-1, ]
alsfrs_label <- select(alsfrs_label, subject_id, label)
#deleting "X" from each subject_id  
for (i in 1:nrow(alsfrs_label)) {
  alsfrs_label$subject_id[i] <- sub("^X", "", alsfrs_label$subject_id[i])
  
}
alsfrs_label$subject_id <- as.integer(alsfrs_label$subject_id)
##done

complete_data <- left_join(alsfrs_label, alsfrs, by = "subject_id") %>%
  left_join(alshistory, by = "subject_id") %>%
  left_join(demographics, by = "subject_id") %>%
  left_join(riluzole, by = "subject_id") %>%
  left_join(vital_height, by = "subject_id") %>%
  left_join(vital_pressure, by = "subject_id") %>%
  left_join(vital_respiratory_rate, by = "subject_id") %>%
  left_join(vital_temperature, by = "subject_id") %>%
  left_join(vital_weight, by = "subject_id")

library(randomForest)

imputed_data <- rfImpute(complete_data[, c(-1, -2)], complete_data$label)
imputed_data[, 1] <- NULL
imputed_data <- cbind(imputed_data, label = complete_data$label)

#train / test set
n <- nrow(imputed_data)
shuffled_df <- imputed_data[sample(n), ]
train_indices <- 1:round(0.75 * n)
train <- shuffled_df[train_indices, ]
test_indices <- (round(0.75 * n) + 1):n
test <- shuffled_df[test_indices, ]

RFmodel <- randomForest(label ~ ., data = train)
test$label_pred <- predict(RFmodel, test, type = "class")
mean(test$label_pred == test$label)
