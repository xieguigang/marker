# 2. LASSO特征选择函数
run_lasso <- function(X, y, lambda = 0.01) {
  # 交叉验证确定最优lambda
  cv_fit <- cv.glmnet(X, y, family = "binomial", alpha = 1)

  # 提取最优模型
  best_model <- glmnet(X, y, family = "binomial", alpha = 1, lambda = lambda)

  # 提取非零系数特征
  coefs <- coef(best_model, s = lambda)
  selected_features <- rownames(coefs)[which(coefs != 0)][-1]  # 排除截距项

  return(list(
    features = selected_features,
    model = best_model,
    cv_error = min(cv_fit$cvm)
  ))
}

# 3. 随机森林特征重要性
run_random_forest <- function(X, y, ntree = 500) {
  set.seed(123)
  rf_model <- randomForest(X, y, ntree = ntree, importance = TRUE)

  # 提取重要性排名
  importance_df <- data.frame(
    feature = colnames(X),
    importance = rf_model$importance[, "MeanDecreaseGini"],
    stringsAsFactors = FALSE
  ) %>%
    arrange(desc(importance))

  return(list(
    features = importance_df$feature[importance_df$importance > 0.5],
    model = rf_model,
    oob_error = rf_model$err.rate[nrow(rf_model$err.rate), "OOB"]
  ))
}

# 4. SVM-RFE特征选择
run_svm_rfe <- function(X, y, n_features = 5, metric = "Accuracy", kernel = "radial",...) {
  # 核方法映射表
  method_map <- list(
    radial = "svmRadial",
    linear = "svmLinear",
    polynomial = "svmPoly",
    sigmoid = "svmSigmoid"
  )

  # 参数模板（扩展默认值）
  kernel_params <- list(
    radial = list(gamma = 1/sqrt(ncol(X)), cost = 1),
    linear = list(cost = 10),
    polynomial = list(degree = 3, scale = TRUE, coef0 = 0, cost = 1),
    sigmoid = list(gamma = 1/sqrt(ncol(X)), coef0 = 0, cost = 1)
  )

  # 参数合并与验证
  svm_params <- modifyList(kernel_params[[kernel]], list(...))

  # 生成调参网格
  tuneGrid <- switch(kernel,
                     "radial" = expand.grid(C = svm_params$cost, sigma = svm_params$gamma),
                     "linear" = expand.grid(C = svm_params$cost),
                     "polynomial" = expand.grid(C = svm_params$cost, degree = svm_params$degree, scale = svm_params$scale),
                     "sigmoid" = expand.grid(C = svm_params$cost, sigma = svm_params$gamma, coef0 = svm_params$coef0)
  )

  # 设置特征筛选控制参数
  ctrl <- rfeControl(
    functions = caretFuncs,
    method = "cv",
    number = 10,
    verbose = FALSE,
    returnResamp = "all",
    # 关键参数：强制遍历所有特征数量
    saveDetails = TRUE
  )

  # 定义特征筛选过程
  rfe_results <- rfe(
    X, y,
    sizes = 1:n_features,
    rfeControl = ctrl,
    method = method_map[[kernel]],
    metric = metric,
    tuneLength = 5,
    tuneGrid = tuneGrid,
    preProcess = c("center", "scale"),
    importance = TRUE
  )

  return(list(
    features = predictors(rfe_results),
    model = rfe_results$fit,
    error = rfe_results$resample$RMSE
  ))
}

# 5. 模型集成与验证
ensemble_model <- function(X, y, selected_features) {
  # 构建列线图模型
  factors = paste("`",paste(selected_features, collapse = "`+`"),"`", sep = "");

  print("view factor list:");
  print(factors);

  model_dir <- "saved_models";
  if (!dir.exists(model_dir)) dir.create(model_dir);

  formula <- as.formula(paste("class ~ ", factors ));
  dX = as.data.frame(X);
  dX = dX[,selected_features];
  dX[,"class"] <- as.numeric( y) -1;
  nomogram_model <- glm(formula, family = "binomial", data = dX)

  xgb_model <- xgboost(
    data = as.matrix(dX[,selected_features]),
    label = dX$class,
    nrounds = 100,
    objective = "binary:logistic",
    eval_metric = "logloss"
  )

  # 训练随机森林
  rf_model <- randomForest(
    x = as.matrix(dX[,selected_features]),
    y = dX$class,
    ntree = 50,
    importance = TRUE
  )

  print(dX);
  print("do cross validation...");

  # 交叉验证评估
  cv_results <- cv.glmnet(as.matrix(dX[,selected_features]), as.numeric( y) -1, family = "binomial", alpha = 0)
  roc_auc <- max(cv_results$glmnet.fit$dev.ratio) * 100

  X = as.data.frame(dX);
  # 添加模型预测概率到数据集
  X$prob_glm <- predict(nomogram_model, X[,selected_features], type = "response");
  X$prob_xgb <- predict(xgb_model, as.matrix(X[,selected_features]));
  X$prob_rf <- predict(rf_model, as.matrix( X[,selected_features]));  # 获取阳性概率
  
  # 计算决策曲线
  thresholds <- seq(0, 0.5, by = 0.01)  # 设置合理的阈值范围
  
  dca_glm <- decision_curve(
    class ~ prob_glm,
    data = X,
    thresholds = thresholds,
    bootstraps = 50
  )
  
  dca_xgb <- decision_curve(
    class ~ prob_xgb,
    data = X,
    thresholds = thresholds,
    bootstraps = 50
  )
  
  dca_rf <- decision_curve(
    class ~ prob_rf,
    data = X,
    thresholds = thresholds,
    bootstraps = 50
  )
  
  pdf("./decision_curve.pdf", width = 12, height = 9)  # 单位：英寸
  # 绘制决策曲线
  plot_decision_curve(
    list(dca_glm, dca_xgb, dca_rf),
    curve.names = c("Logistic Regression", "XGBoost", "Random Forest"),
    col = c("red", "blue", "green"),
    lty = c(1, 2, 3),
    lwd = 6,
    legend.position = "topright",
    xlab = "Threshold Probability",
    ylab = "Net Benefit"
  )
  dev.off();
  
    # 1. 保存逻辑回归模型
  saveRDS(nomogram_model, file = file.path(model_dir, "logistic_model.rds"))
  
  # 2. 保存XGBoost模型（需额外保存特征名）
  xgb.save(xgb_model, file.path(model_dir, "xgb_model.model"))
  writeLines(
    jsonlite::toJSON(list(features = selected_features)),
    file.path(model_dir, "xgb_features.json")
  )
  
  # 3. 保存随机森林模型
  saveRDS(rf_model, file = file.path(model_dir, "rf_model.rds"))
  
  # 4. 保存SHAP解释器（可选）
  shap_data <- list(
    features = selected_features,
    baseline = mean(predict(xgb_model, as.matrix(X[, selected_features]))));
  saveRDS(shap_data, file.path(model_dir, "shap_data.rds"))
  
  # 保存模型元数据
  metadata <- list(
    save_date = Sys.Date(),
    features_used = selected_features,
    dataset_dim = dim(X)
  )
  saveRDS(metadata, file.path(model_dir, "model_metadata.rds"))
  ######################################################
  
  
  # 在ensemble_model函数中添加以下代码（在保存模型之后，返回结果之前）
  
  # 绘制校准曲线
  pdf("./calibration_curves.pdf", width = 10, height = 6)
  par(mfrow = c(1, 3))  # 一行三列的布局
  
  # 1. 逻辑回归校准曲线
  calibration_data_glm <- data.frame(
    predicted = X$prob_glm,
    actual = X$class
  ) %>%
    arrange(predicted) %>%
    mutate(bin = cut(predicted, breaks = seq(0, 1, by = 0.1), include.lowest = TRUE)) %>%
    group_by(bin) %>%
    summarise(
      mean_pred = mean(predicted),
      mean_actual = mean(actual),
      n = n()
    )
  
  plot(calibration_data_glm$mean_pred, calibration_data_glm$mean_actual, 
       xlim = c(0, 1), ylim = c(0, 1), 
       xlab = "预测概率", ylab = "实际比例", 
       main = "逻辑回归校准曲线",
       col = "blue", pch = 19, cex = 1.5)
  abline(0, 1, col = "red", lty = 2, lwd = 2)
  lines(calibration_data_glm$mean_pred, calibration_data_glm$mean_actual, col = "blue", lwd = 2)
  
  # 2. XGBoost校准曲线
  calibration_data_xgb <- data.frame(
    predicted = X$prob_xgb,
    actual = X$class
  ) %>%
    arrange(predicted) %>%
    mutate(bin = cut(predicted, breaks = seq(0, 1, by = 0.1), include.lowest = TRUE)) %>%
    group_by(bin) %>%
    summarise(
      mean_pred = mean(predicted),
      mean_actual = mean(actual),
      n = n()
    )
  
  plot(calibration_data_xgb$mean_pred, calibration_data_xgb$mean_actual, 
       xlim = c(0, 1), ylim = c(0, 1), 
       xlab = "预测概率", ylab = "实际比例", 
       main = "XGBoost校准曲线",
       col = "green", pch = 19, cex = 1.5)
  abline(0, 1, col = "red", lty = 2, lwd = 2)
  lines(calibration_data_xgb$mean_pred, calibration_data_xgb$mean_actual, col = "green", lwd = 2)
  
  # 3. 随机森林校准曲线
  calibration_data_rf <- data.frame(
    predicted = X$prob_rf,
    actual = X$class
  ) %>%
    arrange(predicted) %>%
    mutate(bin = cut(predicted, breaks = seq(0, 1, by = 0.1), include.lowest = TRUE)) %>%
    group_by(bin) %>%
    summarise(
      mean_pred = mean(predicted),
      mean_actual = mean(actual),
      n = n()
    )
  
  plot(calibration_data_rf$mean_pred, calibration_data_rf$mean_actual, 
       xlim = c(0, 1), ylim = c(0, 1), 
       xlab = "预测概率", ylab = "实际比例", 
       main = "随机森林校准曲线",
       col = "purple", pch = 19, cex = 1.5)
  abline(0, 1, col = "red", lty = 2, lwd = 2)
  lines(calibration_data_rf$mean_pred, calibration_data_rf$mean_actual, col = "purple", lwd = 2)
  
  dev.off()
  
  # 使用ggplot2绘制更美观的校准曲线（可选）

  
  # 合并三个模型的校准数据
  calibration_data_all <- bind_rows(
    mutate(calibration_data_glm, Model = "逻辑回归"),
    mutate(calibration_data_xgb, Model = "XGBoost"),
    mutate(calibration_data_rf, Model = "随机森林")
  )
  
  # 绘制组合校准曲线
  p <- ggplot(calibration_data_all, aes(x = mean_pred, y = mean_actual, color = Model)) +
    geom_point(aes(size = n), alpha = 0.7) +
    geom_line(aes(group = Model), size = 1) +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
    scale_color_manual(values = c("逻辑回归" = "blue", "XGBoost" = "green", "随机森林" = "purple")) +
    labs(x = "平均预测概率", y = "实际事件比例", 
         title = "模型校准曲线比较",
         subtitle = "理想情况应接近对角线") +
    xlim(0, 1) + ylim(0, 1) +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  ggsave("./combined_calibration_curve.pdf", plot = p, width = 10, height = 8)
  
  # 将校准数据保存到Excel
  calibration_data_all$bin <- as.character(calibration_data_all$bin)
  write.xlsx(calibration_data_all, file = "./calibration_data.xlsx")
  
  
  return(list(
    nomogram = nomogram_model,
    auc = roc_auc,
    coefficients = coef(nomogram_model),
    formula = formula,
    dX = dX,
    xgb = xgb_model,
    rf_model = rf_model
  ))
}

evaluate_model <- function(class, pred_prob) {
  # 将数值标签转换为因子
  actual_class <- factor(ifelse(class > 0.5, "Yes", "No"), levels = c("No", "Yes"))
  predict_class <- factor(ifelse(pred_prob > 0.5, "Yes", "No"), levels = c("No", "Yes"))
  
  # 计算混淆矩阵
  cm <- confusionMatrix(predict_class, actual_class, positive = "Yes")
  
  # 计算所有指标
  auc_val <- auc(roc(class, pred_prob))
  
  # 计算PPV和NPV
  tp <- cm$table["Yes", "Yes"]
  fp <- cm$table["Yes", "No"]
  tn <- cm$table["No", "No"]
  fn <- cm$table["No", "Yes"]
  
  ppv <- tp / (tp + fp)
  npv <- tn / (tn + fn)
  
  # 计算Brier分数
  brier <- mean((pred_prob - class)^2)
  
  list(
    accuracy = cm$overall["Accuracy"],
    sensitivity = cm$byClass["Sensitivity"],
    specificity = cm$byClass["Specificity"],
    ppv = ppv,
    npv = npv,
    f1 = cm$byClass["F1"],
    kappa = cm$overall["Kappa"],
    auc = auc_val,
    brier = brier
  )
}

# 6. 可视化模块
visualize_results <- function(results, X, y,top_features) {
  nomogram_model <- results$nomogram;
  xgb_model =results$xgb;
  rf_model = results$rf_model;
  auc <- results$auc;
  train_data = results$dX;
  coefficients <- results$coefficients;
  dX = as.data.frame(X);
  dX[,"class"] = as.numeric(y) -1;
  formula = results$formula;

  model_test = data.frame(y_class = y, y_label = dX$class,
                          row.names = rownames(X));

  pdf(file = "./roc_nomogram_model.pdf");

  # 获取预测概率
  # pred_prob <- predict(nomogram_model, type = "response")
  pred_prob <- predict(nomogram_model, X[,top_features], type = "response");
  train_prob <- predict(nomogram_model,train_data[, top_features], type="response");
  # 绘制ROC曲线
  roc_obj <- roc(dX$class, pred_prob)
  train_roc <- roc(train_data$class, train_prob);
  roc_nomogram <- roc_obj;
  evl_nomogram <- evaluate_model(dX$class, pred_prob);
  print(pred_prob);
  model_test = cbind(model_test, nomogram_model = pred_prob);
  # 计算AUC值并设置为标题
  auc_value <- round(auc(roc_obj), 4)  # 保留4位小数
  plot(roc_obj, 
       main = paste("ROC Curve (AUC =", auc_value, ")"),  # 设置标题[1,5](@ref)
       print.auc = TRUE, 
       auc.polygon = TRUE, 
       auc.polygon.col = "white",  # 多边形颜色改为白色[8](@ref)
       col = "#1c61b6",  # 曲线颜色（可选）
       lwd = 2)  # 曲线粗细（可选）
  dev.off();

  pdf(file = "./nomogram_model_train_test.pdf");
  
  plot(roc_nomogram, col = "blue", lwd = 2, main = "Train & Test ROC Curves",
       xlab = "False Positive Rate", ylab = "True Positive Rate")
  lines(train_roc, col = "red", lwd = 2)
  
  legend("bottomright",
         legend = c(paste("Train Data (AUC =", round(auc(train_roc), 3), ")"),
                    paste("Test Data (AUC =", round(auc(roc_nomogram), 3), ")")
         ),
         col = c("blue", "red"), lty = 1, lwd = 2)
  
  dev.off();
  
  
  
  pdf(file = "./roc_xgb_model.pdf");

  # 获取预测概率
  # pred_prob <- predict(nomogram_model, type = "response")
  pred_prob <- predict(xgb_model, as.matrix(X[,top_features]));
  train_prob <- predict(xgb_model,as.matrix(train_data[,top_features]));
  
  # 绘制ROC曲线
  train_roc <- roc(train_data$class, train_prob);
  roc_obj <- roc(dX$class, pred_prob)
  roc_xgb <- roc_obj;
  evl_xgb <- evaluate_model(dX$class, pred_prob);
  print(pred_prob);
  model_test = cbind(model_test, xgb_model = pred_prob);
  # 计算AUC值并设置为标题
  auc_value <- round(auc(roc_obj), 4)  # 保留4位小数
  plot(roc_obj, 
       main = paste("ROC Curve (AUC =", auc_value, ")"),  # 设置标题[1,5](@ref)
       print.auc = TRUE, 
       auc.polygon = TRUE, 
       auc.polygon.col = "white",  # 多边形颜色改为白色[8](@ref)
       col = "#1c61b6",  # 曲线颜色（可选）
       lwd = 2)  # 曲线粗细（可选）
  # 添加交叉验证的AUC值
  # text(0.5, 0.3, paste("CV AUC:", round(auc, 2)), col = "red");
  dev.off();

  pdf(file = "./xgb_model_train_test.pdf");
  
  plot(roc_xgb, col = "blue", lwd = 2, main = "Train & Test ROC Curves",
       xlab = "False Positive Rate", ylab = "True Positive Rate")
  lines(train_roc, col = "red", lwd = 2)
  
  legend("bottomright",
         legend = c(paste("Train Data (AUC =", round(auc(train_roc), 3), ")"),
                    paste("Test Data (AUC =", round(auc(roc_xgb), 3), ")")
         ),
         col = c("blue", "red"), lty = 1, lwd = 2)
  
  dev.off();
  
  
  
  pdf(file = "./roc_rf_model.pdf");

  # 获取预测概率
  # pred_prob <- predict(nomogram_model, type = "response")
  pred_prob <- predict(rf_model, as.matrix( X[,top_features]));
  train_prob <- predict(rf_model, as.matrix( train_data[,top_features]));
  # 绘制ROC曲线
  train_roc <- roc(train_data$class, train_prob);
  roc_obj <- roc(dX$class, pred_prob)
  roc_rf <- roc_obj
  evl_rf <- evaluate_model(dX$class, pred_prob);
  print(pred_prob);
  model_test = cbind(model_test, rf_model = pred_prob);
  # 计算AUC值并设置为标题
  auc_value <- round(auc(roc_obj), 4)  # 保留4位小数
  plot(roc_obj, 
       main = paste("ROC Curve (AUC =", auc_value, ")"),  # 设置标题[1,5](@ref)
       print.auc = TRUE, 
       auc.polygon = TRUE, 
       auc.polygon.col = "white",  # 多边形颜色改为白色[8](@ref)
       col = "#1c61b6",  # 曲线颜色（可选）
       lwd = 2)  # 曲线粗细（可选）
  # 添加交叉验证的AUC值
  # text(0.5, 0.3, paste("CV AUC:", round(auc, 2)), col = "red");
  dev.off();

  pdf(file = "./rf_model_train_test.pdf");
  
  plot(roc_xgb, col = "blue", lwd = 2, main = "Train & Test ROC Curves",
       xlab = "False Positive Rate", ylab = "True Positive Rate")
  lines(train_roc, col = "red", lwd = 2)
  
  legend("bottomright",
         legend = c(paste("Train Data (AUC =", round(auc(train_roc), 3), ")"),
                    paste("Test Data (AUC =", round(auc(roc_rf ), 3), ")")
         ),
         col = c("blue", "red"), lty = 1, lwd = 2)
  
  dev.off();
  
  
  # 计算每个模型在训练集和测试集上的性能指标
  calculate_metrics <- function(model, features, data_type = "test") {
    # 根据数据类型选择数据集
    if (data_type == "train") {
      data <- train_data
      actual <- train_data$class
    } else {
      data <- as.data.frame(X)
      actual <- as.numeric(y) - 1
    }
    
    # 获取预测概率
    if (inherits(model, "glm")) {
      pred_prob <- predict(model, data[, features], type = "response")
    } else if (inherits(model, "xgb.Booster")) {
      pred_prob <- predict(model, as.matrix(data[, features]))
    } else {
      pred_prob <- predict(model, as.matrix(data[, features]))
    }
    
    # 评估模型
    evaluate_model(actual, pred_prob)
  }
  
  # 为每个模型计算训练集和测试集指标
  model_metrics <- list()
  
  models <- list(
    logistic = results$nomogram,
    xgb = results$xgb,
    rf = results$rf_model
  )
  
  for (model_name in names(models)) {
    # 测试集指标
    test_metrics <- calculate_metrics(models[[model_name]], top_features, "test")
    # 训练集指标
    train_metrics <- calculate_metrics(models[[model_name]], top_features, "train")
    
    model_metrics[[paste0(model_name, "_test")]] <- test_metrics
    model_metrics[[paste0(model_name, "_train")]] <- train_metrics
  }
  
  # 创建结果数据框
  results_df <- data.frame(
    Model = rep(c("Logistic", "XGBoost", "Random Forest"), each = 2),
    Dataset = rep(c("Train", "Test"), times = 3),
    AUC = c(
      model_metrics$logistic_train$auc, model_metrics$logistic_test$auc,
      model_metrics$xgb_train$auc, model_metrics$xgb_test$auc,
      model_metrics$rf_train$auc, model_metrics$rf_test$auc
    ),
    ACC = c(
      model_metrics$logistic_train$accuracy, model_metrics$logistic_test$accuracy,
      model_metrics$xgb_train$accuracy, model_metrics$xgb_test$accuracy,
      model_metrics$rf_train$accuracy, model_metrics$rf_test$accuracy
    ),
    Sensitivity = c(
      model_metrics$logistic_train$sensitivity, model_metrics$logistic_test$sensitivity,
      model_metrics$xgb_train$sensitivity, model_metrics$xgb_test$sensitivity,
      model_metrics$rf_train$sensitivity, model_metrics$rf_test$sensitivity
    ),
    Specificity = c(
      model_metrics$logistic_train$specificity, model_metrics$logistic_test$specificity,
      model_metrics$xgb_train$specificity, model_metrics$xgb_test$specificity,
      model_metrics$rf_train$specificity, model_metrics$rf_test$specificity
    ),
    PPV = c(
      model_metrics$logistic_train$ppv, model_metrics$logistic_test$ppv,
      model_metrics$xgb_train$ppv, model_metrics$xgb_test$ppv,
      model_metrics$rf_train$ppv, model_metrics$rf_test$ppv
    ),
    NPV = c(
      model_metrics$logistic_train$npv, model_metrics$logistic_test$npv,
      model_metrics$xgb_train$npv, model_metrics$xgb_test$npv,
      model_metrics$rf_train$npv, model_metrics$rf_test$npv
    ),
    F1 = c(
      model_metrics$logistic_train$f1, model_metrics$logistic_test$f1,
      model_metrics$xgb_train$f1, model_metrics$xgb_test$f1,
      model_metrics$rf_train$f1, model_metrics$rf_test$f1
    ),
    Kappa = c(
      model_metrics$logistic_train$kappa, model_metrics$logistic_test$kappa,
      model_metrics$xgb_train$kappa, model_metrics$xgb_test$kappa,
      model_metrics$rf_train$kappa, model_metrics$rf_test$kappa
    ),
    Brier = c(
      model_metrics$logistic_train$brier, model_metrics$logistic_test$brier,
      model_metrics$xgb_train$brier, model_metrics$xgb_test$brier,
      model_metrics$rf_train$brier, model_metrics$rf_test$brier
    )
  )
  
  # 保存到Excel
  write.xlsx(results_df, file = "./model_compares.xlsx")

  # 模型在测试集上的预测测试效果
  write.xlsx(model_test, file = "./model_test.xlsx");
  #write.xlsx(data.frame(
  #  模型 = c("XGBoost", "RF", "logistic"),
  #  准确率 = sapply(results, function(x) round(x$accuracy, 3)),
  #  AUC = sapply(results, function(x) round(x$auc, 3)),
  #  敏感度 = sapply(results, function(x) round(x$sensitivity, 3)),
  #  特异度 = sapply(results, function(x) round(x$specificity, 3)),
  #  F1得分 = sapply(results, function(x) round(x$f1, 3))
  #), file = "./model_compares.xlsx");

  pdf(file = "./ROC.pdf");

  plot(roc_nomogram, col = "blue", lwd = 2, main = "ROC Curves Comparison",
       xlab = "False Positive Rate", ylab = "True Positive Rate")
  lines(roc_xgb, col = "red", lwd = 2)
  lines(roc_rf, col = "green", lwd = 2)

  legend("bottomright",
         legend = c(paste("Model Nomogram (AUC =", round(auc(roc_nomogram), 3), ")"),
                    paste("Model xgboost (AUC =", round(auc(roc_xgb), 3), ")"),
                    paste("Model Random Forest (AUC =", round(auc(roc_rf), 3), ")")
         ),
         col = c("blue", "red", "green"), lty = 1, lwd = 2)

  dev.off();

  # library(rms)
  # 转换为rms包数据格式
  # ddist <- datadist(as.data.frame(X))
  # options(datadist = "ddist")
  # 绘制校准曲线
  # cal <- calibrate(nomogram_model, method = "boot", B = 1000)
  # plot(cal)

  pdf(file = "./feature_importance.pdf");

  coef_df <- data.frame(
    Feature = names(coefficients(nomogram_model))[-1],
    Importance = abs(coefficients(nomogram_model)[-1])
  )
  p = ggplot(coef_df, aes(x = reorder(Feature, Importance), y = Importance)) +
    geom_col(fill = "#87CEEB") +
    coord_flip() +
    labs(title = "Logistic Regression Feature Importance", x = "")
  print(p);
  dev.off();

  write.xlsx(coef_df, file = "./model_features.xlsx");

  library(fastshap)
  library(shapviz)

  # 若需输出类别预测（0/1）
  pred_wrapper_class <- function(model, newdata) {
    as.numeric(predict(model, newdata = newdata, type = "response") > 0.5)
  }
  shap_values <- explain(nomogram_model, X = as.data.frame( X[, top_features]),
                         pred_wrapper =pred_wrapper_class,  # 必须指定
                         nsim = 100,  # 蒙特卡洛模拟次数，建议 >= 100
                         shap_only = FALSE
  );
  shap_viz <- shapviz(shap_values)

  # 提取核心结果
  shap_matrix <- shap_values$shapley_values
  baseline <- shap_values$baseline

  # 导出CSV
  write.csv(shap_matrix, "SHAP_Values.csv", row.names = TRUE, quote = FALSE)

  # 导出Excel
  wb <- createWorkbook()
  addWorksheet(wb, "SHAP_Values")
  writeData(wb, 1, shap_matrix, rowNames = TRUE)
  addWorksheet(wb, "Baseline")
  writeData(wb, 2, data.frame(Baseline = baseline))
  saveWorkbook(wb, "shap_results.xlsx", overwrite = TRUE)


  # 3. 可视化
  pdf(file = "./shap.pdf");
  print(sv_importance(shap_viz))  # 全局特征重要性
  # 蜂群图（Beeswarm plot）
  print(sv_waterfall(shap_viz, row_id = 1))  # 单个样本解释
  dev.off();


  pdf(file = "./nomogram.pdf", width = 24,height = 8, family = "GB1");
  library(rms)

  ddist <<- datadist(dX);
  options(datadist = "ddist");

  # 重构模型为rms格式
  lrm_model <- lrm(formula, data = dX, x = TRUE, y = TRUE)
  nom <- nomogram(lrm_model, fun = plogis, funlabel = "Risk Probability")
  plot(nom,xfrac = 0.1,cex.var = 1.5, cex.axis = 0.85   )
  dev.off();

  # library(rmda)
  # dca_data <- decision_curve(formula, data = dX, family = binomial)
  # plot_decision_curve(dca_data, curve.names = "Our Model")
}

# 新增模型加载函数
load_ensemble_models <- function(model_dir = "saved_models") {
  models <- list()
  
  # 1. 加载逻辑回归模型
  if (file.exists(file.path(model_dir, "logistic_model.rds"))) {
    models$nomogram <- readRDS(file.path(model_dir, "logistic_model.rds"))
  }
  
  # 2. 加载XGBoost模型及特征
  if (file.exists(file.path(model_dir, "xgb_model.model"))) {
    models$xgb <- xgb.load(file.path(model_dir, "xgb_model.model"))
    feature_json <- jsonlite::fromJSON(file.path(model_dir, "xgb_features.json"))
    models$xgb_features <- feature_json$features
  }
  
  # 3. 加载随机森林模型
  if (file.exists(file.path(model_dir, "rf_model.rds"))) {
    models$rf <- readRDS(file.path(model_dir, "rf_model.rds"))
  }
  
  # 4. 加载元数据
  if (file.exists(file.path(model_dir, "model_metadata.rds"))) {
    models$metadata <- readRDS(file.path(model_dir, "model_metadata.rds"))
  }
  
  return(models)
}

# 使用保存的模型进行预测
predict_with_saved_model <- function(new_data, model_dir = "saved_models") {
  models <- load_ensemble_models(model_dir)
  
  # 确保新数据特征与训练时一致
  new_data <- new_data[, models$metadata$features_used]
  
  # XGBoost预测
  xgb_pred <- predict(models$xgb, as.matrix(new_data))
  
  # 逻辑回归预测
  lr_pred <- predict(models$nomogram, as.data.frame(new_data), type = "response")
  
  # 随机森林预测
  rf_pred <- predict(models$rf, as.matrix( new_data));
  
  data.frame(
    xgb = xgb_pred, 
	lr = lr_pred, 
	rf = rf_pred, 
	row.names = rownames(new_data)
  );
}

main_manual = function(file_path, class, sel_features = NULL, save_dir=NULL) {
  # 1. 加载数据
  data <- read.csv(file_path,  row.names = 1, check.names = FALSE);
  preprocessed <- preprocess_data(data[data$class %in% class,]);
  X <- preprocessed$X
  y <- preprocessed$y
  name = save_dir;

  if (is.null(save_dir)) {
    name = dirname(file_path); # paste(class, collapse = " vs ");
  }  

  print(name);
  print(sel_features);

  dir.create(name);
  setwd(name);

  if (length(sel_features) == 0) {
    # 3. 特征选择
    lasso_result <- run_lasso(X, y)
    rf_result <- run_random_forest(X, y)
    svm_result <- run_svm_rfe(X, y)
    
    combined <- c(as.character(lasso_result$features), as.character(rf_result$features), as.character(svm_result$features))
    # 统计次数并降序排序
    counts <- sort(table(combined), decreasing = TRUE)
    # 提取前三个字符串名称
    top_features <- names(head(counts, 6))
  } else {
    top_features <- sel_features;
  }
  
  split_idx <- createDataPartition(y, p = 0.7, list = FALSE)[,1];
  X = as.data.frame(X);

  print(split_idx);

  train = X[split_idx,];
  train_y = y[split_idx];
  test = X[-split_idx,];
  test_y = y[-split_idx];

  # 4. 模型集成
  ensemble_result <- ensemble_model(train, train_y, top_features)

  # 5. 可视化
  visualize_results(ensemble_result, test, test_y,top_features)

  setwd("..");
}

# main("./factors.csv");
main_manual("./data.csv", c( "阴性","阳性"), save_dir="机器学习");
