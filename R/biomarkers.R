


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
