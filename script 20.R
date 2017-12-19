###Script 20
#classification of W_concave, W_convex

Weibull_data <- filter(complete_data, label != "E")

library(caret)

complete_data_conti <- Weibull_data[, -c(17, 21, 22, 23)]
complete_data_conti_imp <- predict(preProcess(complete_data_conti[, -1], method=c("medianImpute")), complete_data_conti[, -1])
complete_data_imp <- left_join(mutate(complete_data_conti_imp, subject_id = complete_data_conti[, 1]), complete_data[, c(1, 17, 21, 22, 23)], by = "subject_id")
imputed_omitted_data <- na.omit(complete_data_imp)
imputed_omitted_data$subject_id <- NULL
imputed_omitted_data$label <- droplevels(imputed_omitted_data$label)


#Random Forest
model1 <- train(
  label ~.,
  data = imputed_omitted_data, 
  method = "ranger",
  tuneLength = 10,
  importance = "permutation",
  trControl = trainControl(method = "cv", number = 5, verboseIter = TRUE, returnResamp = "all")
)
model1
plot(model1)
plot(varImp(model1, scale = F), 20)
varImp(model1)

#Gradient Boosting 1
xgb_grid_1 = expand.grid(
  nrounds = 500,
  eta = c(0.1, 0.01),
  max_depth = c(4, 6, 8),
  gamma = c(0, 5),
  colsample_bytree = c(0.5, 0.9),
  min_child_weight = c(1, 5),
  subsample = c(0.5, 0.9)
)

xgb_trcontrol_1 = trainControl(
  method = "cv",
  number = 5,
  verboseIter = TRUE,
  returnResamp = "all"
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
plot(varImp(model2, scale = FALSE), 20)

#Gradient Boosting 2
xgb_grid_1 = expand.grid(
  nrounds = 500,
  eta = 0.01,
  max_depth = 4,
  gamma = 0,
  colsample_bytree = 0.5,
  min_child_weight = 1,
  subsample = 0.5
)

xgb_trcontrol_1 = trainControl(
  method = "cv",
  number = 5,
  verboseIter = TRUE
)

model3 <- train(
  label ~.,
  data = imputed_omitted_data, 
  method = "xgbTree",
  importance = TRUE,
  trControl = xgb_trcontrol_1,
  tuneGrid = xgb_grid_1
)

model3
plot(model3)
plot(varImp(model3), 20)

#Support Vector Machine (Linear Kernel)
model4 <- train(
  label ~.,
  data = imputed_omitted_data, 
  method = "svmLinear",
  preProcess = c("center", "scale"),
  importance = TRUE,
  trControl = trainControl(method = "cv", number = 5, verboseIter = TRUE)
)
model4
plot(model4)
plot(varImp(model4), 20)

#Support Vector Machine (Radial Kernel)
model5 <- train(
  label ~.,
  data = imputed_omitted_data, 
  method = "svmRadialSigma",
  importance = TRUE,
  trControl = trainControl(method = "cv", number = 5, verboseIter = TRUE)
)
model5
plot(model5)
varImp(model5)

#Linear Discriminant Analysis
model6 <- train(
  label ~.,
  data = imputed_omitted_data, 
  method = "lda",
  importance = TRUE,
  preProcess = c("center", "scale"),
  trControl = trainControl(method = "cv", number = 5, verboseIter = TRUE, returnResamp = "all")
)
model6
plot(model6)
plot(varImp(model6), 20)

#Quadratic Discriminant Analysis
model7 <- train(
  label ~.,
  data = imputed_omitted_data, 
  method = "qda",
  importance = TRUE,
  trControl = trainControl(method = "cv", number = 5, verboseIter = TRUE)
)
model7
plot(model7)
varImp(model7)

#Boxplot of accuracy
boxplot(model1$results$Accuracy, model2$results$Accuracy, c(0.7909406, 0.7746161, 0.7846057, 0.7945393, 0.7836364), c(0.7869601, 0.7746538, 0.7632904, 0.787876, 0.7533937), outline = FALSE, names = c("RF", "GB", "SVM", "LDA"), main = "Accuracy")

#feature selection
feature_selection <- select(imputed_omitted_data, alsfrs_slope, label, Q8_slope, Q4_slope, Q9_slope, Q6_slope, Q5_slope, Q7_slope, Onset_Delta, mitos, alsfrs_total, Q2_slope, preslope, Q1_slope, gender, age, weight_difference_ratio, Q10_slope, height)
detach("package:MASS", unload=TRUE)
#Random Forest
model11 <- train(
  label ~.,
  data = feature_selection, 
  method = "ranger",
  tuneLength = 10,
  importance = "permutation",
  trControl = trainControl(method = "cv", number = 5, verboseIter = TRUE, returnResamp = "all")
)
model11
plot(model11)
plot(varImp(model11, scale = F), 20)
varImp(model11)
boxplot(model1$results$Accuracy, model11$results$Accuracy, outline = FALSE, names = c("RF", "RF with feature selection"), main = "Accuracy")


#ROC curve
#Use this in model1 :  trControl = trainControl(method = "cv", number = 5, verboseIter = TRUE, returnResamp = "all", summaryFunction=twoClassSummary, classProbs=T, savePredictions = T )
rows <- sample(nrow(feature_selection))
feature_selection <- feature_selection[rows, ]
split <- round(nrow(feature_selection) * .80)
train <- feature_selection[1:split, ]
test <- feature_selection[(split + 1):nrow(feature_selection), ]

model10 <- train(
  label ~.,
  data = train, 
  method = "ranger",
  tuneLength = 10,
  importance = "permutation",
  trControl = trainControl(method = "cv", number = 5, verboseIter = TRUE, returnResamp = "all", summaryFunction=twoClassSummary, classProbs=T, savePredictions = T )
)
model10
plot(model1)
plot(varImp(model10, scale = F), 20)
varImp(model10)

library(pROC)
p <- predict(model10, test, type = "prob")
mod_roc <- roc(test$label, p$W_concave)
plot.roc(mod_roc,   # roc를 계산한 value를 입력합니다.
         col="red",   # 선의 색상을 설정합니다.
         print.auc=TRUE,   # auc 값을 출력하도록 설정합니다.
         max.auc.polygon=TRUE,   # auc의 최대 면적을 출력하도록 설정합니다.
         auc.polygon=TRUE, auc.polygon.col="#D1F2EB")   # 선 아래 면적에 대한 출력, 색상을 설정합니다. 
