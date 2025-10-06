

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
