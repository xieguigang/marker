
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