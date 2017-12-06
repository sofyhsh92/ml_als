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
  importance = "impurity",
  trControl = trainControl(method = "cv", number = 5, verboseIter = TRUE, returnResamp = "all")
)
model1
plot(model1)
plot(varImp(model1), 20)


#Gradient Boosting 1
xgb_grid_1 = expand.grid(
  nrounds = 100,
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
plot(varImp(model2), 20)

#Gradient Boosting 2
xgb_grid_1 = expand.grid(
  nrounds = 1000,
  eta = 0.1,
  max_depth = 2,
  gamma = 0,
  colsample_bytree = 0.9,
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
  method = "svmLinear2",
  preProcess = c("center", "scale"),
  importance = TRUE,
  trControl = trainControl(method = "cv", number = 5, verboseIter = TRUE)
)
model4
plot(model4)
varImp(model4)

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
  method = "lda2",
  importance = TRUE,
  preProcess = c("center", "scale"),
  trControl = trainControl(method = "cv", number = 5, verboseIter = TRUE, returnResamp = "all")
)
model6
plot(model6)
varImp(model6)

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
boxplot(model1$results$Accuracy, model2$results$Accuracy, model4$results$Accuracy, c(0.7845302, 0.7852355, 0.7542733, 0.7692951, 0.7673569), names = c("RF", "GB", "SVM", "LDA"), main = "Accuracy")

#ROC curve
#Use this in model1 :  trControl = trainControl(method = "cv", number = 5, verboseIter = TRUE, returnResamp = "all", summaryFunction=twoClassSummary, classProbs=T, savePredictions = T )
library(pROC)
plot.roc(mod_roc,   # roc를 계산한 value를 입력합니다.
         col="red",   # 선의 색상을 설정합니다.
         print.auc=TRUE,   # auc 값을 출력하도록 설정합니다.
         max.auc.polygon=TRUE,   # auc의 최대 면적을 출력하도록 설정합니다.
         auc.polygon=TRUE, auc.polygon.col="#D1F2EB")   # 선 아래 면적에 대한 출력, 색상을 설정합니다. 
