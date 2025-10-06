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
