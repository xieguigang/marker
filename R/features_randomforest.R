

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