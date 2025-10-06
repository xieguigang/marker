

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
