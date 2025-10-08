#' Run Random Forest for Feature Importance Analysis
#'
#' This function trains a random forest model using the provided feature matrix and response variable.
#' It calculates feature importance based on the Mean Decrease Gini index and returns a filtered list of significant features,
#' the trained model object, and the Out-of-Bag (OOB) error rate. The function is particularly useful for
#' preliminary feature selection in high-dimensional data analysis.
#'
#' @param X A numeric matrix or data frame containing the predictor variables (features).
#'   Rows represent observations/samples, and columns represent features. This is the fixed effects matrix.
#' @param y A numeric vector representing the response (output) variable. Its length must equal the number of rows in `X`.
#' @param ntree An integer specifying the number of trees to grow in the random forest. A larger number increases
#'   stability but also computation time. The default value is 500.
#'
#' @return A list containing the following components:
#'   \itemize{
#'     \item `features` - A character vector containing the names of features whose importance (MeanDecreaseGini) is greater than 0.5. If no features exceed this threshold, returns an empty vector.
#'     \item `model` - The full fitted random forest model object of class `randomForest`. This can be used for further inspection, prediction, or plotting.
#'     \item `oob_error` - The final Out-of-Bag (OOB) error rate of the model, extracted from the last row of the `err.rate` matrix of the model. This provides an estimate of the prediction error.
#'   }
#'
#' @examples
#' \donttest{
#' # Example using the iris dataset (modifying for a regression problem)
#' data(iris)
#' # Use petal dimensions to predict sepal length (create a continuous `y`)
#' X <- iris[, c("Petal.Length", "Petal.Width", "Sepal.Width")]
#' y <- iris$Sepal.Length
#'
#' # Run the random forest function
#' result <- run_random_forest(X, y, ntree = 100) # Smaller ntree for quick example
#'
#' # Inspect the significant features
#' print(result$features)
#'
#' # Check the OOB error
#' print(result$oob_error)
#'
#' # Plot the importance from the full model (if needed)
#' # randomForest::varImpPlot(result$model)
#' }
#'
#' @seealso
#' \code{\link[randomForest]{randomForest}} for the underlying function used to build the model.
#' \code{\link[randomForest]{importance}} for more details on how variable importance is calculated.
#' \code{\link[randomForest]{varImpPlot}} for plotting variable importance.
#'
#' @export
#'
#' @importFrom randomForest randomForest
#' @importFrom dplyr arrange desc
run_random_forest <- function(X, y) {
    message("run random forest feature selection...");

    # 获取特征数量
    n_features <- ncol(X)
    message("Number of features detected: ", n_features)

    # 动态计算 mtry，并对高维数据设置上限
    mtry_value <- round(sqrt(n_features))
    mtry_value <- min(100, mtry_value) # 即使sqrt(n_features)很大，也限制在100以内
    ntree_auto = TRUE;

    # 自适应调整树的数量和节点大小
    if(n_features > 1000) {
        ntree_value <- 100  # 特征非常多时，减少树的数量
        node_size <- 10     # 增大节点大小，简化树
        max_nodes <- 50     # 限制最大节点数
    } else if(n_features > 500) {
        ntree_value <- 150
        node_size <- 5
        max_nodes <- 100
    } else {
        ntree_value <- ifelse(ntree_auto, 200, 500) # 特征少时可用更多树
        node_size <- 1
        max_nodes <- NULL
    }

    message("Adaptive parameters - mtry: ", mtry_value,
            ", ntree: ", ntree_value,
            ", node_size: ", node_size)

    result <- tryCatch(
        expr = {
            # 2. 模型训练
            rf_model <- randomForest::randomForest(
                x = X,
                y = y,
                ntree = ntree_value,
                mtry = mtry_value,
                nodesize = node_size,
                maxnodes = max_nodes, # 可选参数，强力加速
                importance = TRUE,    # 必须为TRUE才能计算重要性
                do.trace = 50,       # 每50棵树显示一次进度，方便监控
                keep.forest = FALSE   # 若只需特征重要性，可设为FALSE节省内存
            )

            # 提取重要性排名
            importance_df <- data.frame(
                feature = colnames(X),
                importance = rf_model$importance[, "MeanDecreaseGini"],
                stringsAsFactors = FALSE
            ) %>%
                arrange(desc(importance))

            list(
                features = importance_df$feature[importance_df$importance > 0.5],
                model = toString( rf_model),
                oob_error = rf_model$err.rate[nrow(rf_model$err.rate), "OOB"]
            );
        },
        error = function(e) {
            message("random forest feature selection error: ", conditionMessage(e));
            return(NULL) # 出错时返回NULL
        }
    )

    return(result);
}
