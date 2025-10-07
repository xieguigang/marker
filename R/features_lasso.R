#' LASSO Feature Selection
#'
#' This function performs feature selection using LASSO (Least Absolute Shrinkage and Selection Operator) regression.
#' It is particularly useful for high-dimensional data where the number of predictors is large.
#' The function leverages cross-validation to determine the optimal penalty parameter (lambda) and returns the selected features along with the fitted model.
#'
#' @param X A numeric matrix or data.frame containing the predictor variables. Each row represents an observation, and each column represents a feature.
#' @param y A numeric vector representing the response variable. For binary classification, this should be a factor or numeric vector with two levels.
#' @param lambda A numeric value specifying the penalty parameter. If not provided, the function will use the optimal lambda found by cross-validation. Default is `0.01`.
#'
#' @return A list containing the following components:
#'   \item{features}{A character vector of the names of features (predictors) with non-zero coefficients in the LASSO model.}
#'   \item{model}{The fitted glmnet model object of class `glmnet`. This can be used for further inspection or prediction.}
#'   \item{cv_error}{The minimum mean cross-validated error (a numeric value) from the cross-validation process.}
#'
#' @examples
#' \dontrun{
#' # Load required library
#' library(glmnet)
#'
#' # Create sample data
#' set.seed(123)
#' X <- matrix(rnorm(100 * 10), nrow = 100, ncol = 10)
#' colnames(X) <- paste0("Feature", 1:10)
#' y <- sample(0:1, 100, replace = TRUE)
#'
#' # Run LASSO feature selection
#' result <- run_lasso(X, y)
#'
#' # View selected features
#' print(result$features)
#'
#' # Inspect the model
#' summary(result$model)
#'
#' # Check the cross-validation error
#' print(result$cv_error)
#' }
#'
#' @importFrom glmnet cv.glmnet glmnet coef.glmnet
#' @export
#'
#' @seealso
#' * The `glmnet` package documentation: \url{https://glmnet.stanford.edu/}
#' * Useful resources on LASSO regression: \url{https://web.stanford.edu/~hastie/Papers/glmnet.pdf}
run_lasso <- function(X, y, lambda = 0.01) {
    result <- tryCatch(
        expr = {
            # 交叉验证确定最优lambda
            cv_fit <- cv.glmnet(X, y, family = "binomial", alpha = 1)

            # 提取最优模型
            best_model <- glmnet(X, y, family = "binomial", alpha = 1, lambda = lambda)

            # 提取非零系数特征
            coefs <- coef(best_model, s = lambda)
            selected_features <- rownames(coefs)[which(coefs != 0)][-1]  # 排除截距项

            list(
                features = selected_features,
                model = toString( best_model),
                cv_error = min(cv_fit$cvm)
            );
        },
        error = function(e) {
            message("lasso feature selection error: ", conditionMessage(e));
            return(NULL) # 出错时返回NULL
        }
    )

    return(result);
}
