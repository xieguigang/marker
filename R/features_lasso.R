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