
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
    result <- tryCatch(
        expr = {
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

            list(
                features = predictors(rfe_results),
                model = toString( rfe_results$fit),
                error = rfe_results$resample$RMSE
            )
        },
        error = function(e) {
            message("svm feature selection error: ", conditionMessage(e));
            return(NULL) # 出错时返回NULL
        }
    )

    return(result);
}
