
#' SVM-RFE Feature Selection
#'
#' Performs feature selection using Support Vector Machine Recursive Feature Elimination (SVM-RFE).
#' This function implements a wrapper feature selection method that recursively removes the least
#' important features based on SVM weights, ultimately selecting the optimal feature subset.
#'
#' @param X A numeric matrix or data frame where rows represent samples and columns represent
#'   features/genes. This is the predictor variable matrix.
#' @param y A factor or numeric vector containing the outcome/target variable corresponding
#'   to the samples in `X`.
#' @param n_features An integer specifying the maximum number of top features to select.
#'   The function will evaluate feature subsets from 1 to `n_features`. Default is 5.
#' @param metric A character string indicating the performance metric to use for evaluating
#'   feature subsets. Common metrics include "Accuracy" for classification or "RMSE" for
#'   regression. Default is "Accuracy".
#' @param kernel A character string specifying the SVM kernel type. Must be one of:
#'   "radial" (default), "linear", "polynomial", or "sigmoid". Each kernel has different
#'   characteristics for handling non-linear decision boundaries.
#' @param ... Additional arguments passed to the SVM model, allowing customization of
#'   parameters such as `cost`, `gamma`, `degree`, etc. These will override default
#'   kernel parameters.
#'
#' @return A list containing the following components:
#'   \itemize{
#'     \item \code{features} - A character vector of the names of the selected features
#'       (if `X` has column names) or indices of the selected features.
#'     \item \code{model} - The final SVM model trained on the selected features.
#'     \item \code{error} - A vector of performance metric values (e.g., RMSE) from the
#'       resampling process for each feature subset size.
#'   }
#'
#' @details
#' This function utilizes the \code{caret} package's \code{rfe} function to implement
#' SVM-RFE. The algorithm works as follows:
#' \enumerate{
#'   \item Train an SVM model on all features and rank features by their importance
#'     (based on weight magnitude in linear SVM or variable importance for non-linear kernels).
#'   \item Remove the least important feature(s).
#'   \item Repeat the process recursively on the remaining feature set.
#'   \item Evaluate model performance at each step using cross-validation.
#'   \item Select the feature subset that yields optimal performance based on the specified metric.
#' }
#'
#' The function includes predefined parameter templates for different SVM kernels and
#' uses 10-fold cross-validation by default for robust feature evaluation.
#'
#' @examples
#' \donttest{
#' # Load required library
#' library(caret)
#'
#' # Example with iris dataset (classification)
#' data(iris)
#' X <- iris[, 1:4]  # Feature matrix
#' y <- iris[, 5]    # Target variable
#'
#' # Perform SVM-RFE feature selection with radial kernel
#' result <- run_svm_rfe(X, y, n_features = 3, kernel = "radial")
#'
#' # Print selected features
#' print(result$features)
#'
#' # Plot feature selection results (if available)
#' plot(result$model)
#' }
#'
#' @seealso
#' \code{\link[caret]{rfe}} for the underlying recursive feature elimination implementation,
#' \code{\link[e1071]{svm}} for SVM model details,
#' \code{\link[kernlab]{ksvm}} for alternative SVM implementation.
#'
#' @references
#' For more information on SVM-RFE, see:
#' Guyon, I., Weston, J., Barnhill, S., & Vapnik, V. (2002). Gene selection for cancer
#' classification using support vector machines. Machine Learning, 46(1-3), 389-422.
#'
#' @export
run_svm_rfe <- function(X, y, n_features = 5, metric = "Accuracy", kernel = "radial",...) {
    message("Starting SVM-RFE feature selection...")

    # 加载必要包（静默加载）
    suppressMessages({
        if (!require("caret", quietly = TRUE)) install.packages("caret")
        if (!require("e1071", quietly = TRUE)) install.packages("e1071")
        if (!require("doParallel", quietly = TRUE)) install.packages("doParallel")
        library(caret)
        library(doParallel)
    })

    result <- tryCatch(
        expr = {
            n_total <- ncol(X)  # 总特征数

            # 1. 自适应参数调整基于特征数量
            if (n_total > 5000) {
                # 极大数据集：激进优化
                max_sizes <- 20                    # 仅测试20个特征子集大小
                cv_number <- 3                     # 3折交叉验证
                halve_step <- TRUE                 # 启用大步长消除
                tune_len <- 3                      # 减少参数调优组合
            } else if (n_total > 1000) {
                # 大数据集：中等优化
                max_sizes <- 30
                cv_number <- 5
                halve_step <- TRUE
                tune_len <- 5
            } else {
                # 小数据集：保持原样
                max_sizes <- min(n_features, 50)   # 最多50个点
                cv_number <- 10
                halve_step <- FALSE
                tune_len <- 5
            }

            # 2. 自适应设置特征子集大小（sizes）
            if (halve_step && n_total > 100) {
                # 使用halve.above策略：特征数>100时每次消除一半特征
                sizes <- unique(round(2^(seq(0, log2(n_features), length.out = max_sizes))));
            } else {
                # 线性步长：适用于小数据集
                sizes <- unique(round(seq(1, n_features, length.out = max_sizes)))
            }
            sizes <- sort(sizes[sizes <= n_features & sizes >= 1])  # 确保有效性

            # 3. 并行计算设置
            cl <- makeCluster(detectCores() - 1)
            registerDoParallel(cl)
            on.exit(stopCluster(cl))  # 确保退出时关闭集群

            # 核方法映射与参数设置（保持原逻辑）
            method_map <- list(
                radial = "svmRadial",
                linear = "svmLinear",
                polynomial = "svmPoly",
                sigmoid = "svmSigmoid"
            )
            kernel_params <- list(
                radial = list(gamma = 1/sqrt(n_total), cost = 1),
                linear = list(cost = 10),
                polynomial = list(degree = 3, scale = TRUE, coef0 = 0, cost = 1),
                sigmoid = list(gamma = 1/sqrt(n_total), coef0 = 0, cost = 1)
            )
            svm_params <- modifyList(kernel_params[[kernel]], list(...))
            tuneGrid <- switch(kernel,
                               "radial" = expand.grid(C = svm_params$cost, sigma = svm_params$gamma),
                               "linear" = expand.grid(C = svm_params$cost),
                               "polynomial" = expand.grid(C = svm_params$cost, degree = svm_params$degree, scale = svm_params$scale),
                               "sigmoid" = expand.grid(C = svm_params$cost, sigma = svm_params$gamma, coef0 = svm_params$coef0)
            )

            # 4. 配置递归特征消除控制参数
            ctrl <- rfeControl(
                functions = caretFuncs,
                method = "cv",
                number = cv_number,           # 自适应交叉验证折数
                verbose = FALSE,
                allowParallel = TRUE,         # 启用并行
                returnResamp = "final"
            )

            # 5. 执行SVM-RFE
            rfe_results <- rfe(
                X, y,
                sizes = sizes,                # 自适应特征子集大小
                rfeControl = ctrl,
                method = method_map[[kernel]],
                metric = metric,
                tuneGrid = tuneGrid,          # 固定参数网格（避免调优开销）
                preProcess = c("center", "scale"),
                importance = TRUE
            )

            list(
                features = predictors(rfe_results),
                model = toString(rfe_results$fit),
                optimal_size = rfe_results$optsize,
                performance = rfe_results$results
            )
        },
        error = function(e) {
            message("SVM-RFE failed: ", conditionMessage(e))
            return(NULL)
        }
    );

    return(result)
}
