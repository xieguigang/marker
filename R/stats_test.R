#' Perform statistical test based on feature type
#'
#' This function automatically determines the appropriate statistical test (Fisher's exact test for binary features, 
#' Chi-squared test for continuous features) to compare the distribution of a feature between two groups (e.g., negative vs. positive classes).
#'
#' @param feature_vector A numeric vector representing the feature values to be tested. Can contain binary (0/1) or continuous values.
#' @param class_vector A factor or character vector indicating the group membership (e.g., "Negative", "Positive") for each sample. Must be the same length as `feature_vector`.
#' @param CON A scalar value (typically 0, or a reference level in `class_vector`) denoting the control or reference group (e.g., negative class).
#' @param treatment A scalar value (typically 1, or a test level in `class_vector`) denoting the experimental or test group (e.g., positive class).
#'
#' @return A list containing the following elements:
#' \itemize{
#'   \item \code{method} - Character. Name of the statistical test performed ("Fisher exact test" for binary features, "Chi-squared" for continuous features).
#'   \item \code{statistic} - Numeric. The test statistic (Odds Ratio for Fisher's exact test, Chi-squared statistic for Chi-squared test).
#'   \item \code{pvalue} - Numeric. The p-value from the statistical test.
#'   \item \code{feature_type} - Character. Type of the input feature ("binary" or "continuous").
#'   \item **If the feature is binary**, the list additionally contains:
#'     \itemize{
#'       \item \code{negative_0} - Numeric. Count of reference group (class = `CON`) samples where the feature value is 0.
#'       \item \code{negative_1} - Numeric. Count of reference group samples where the feature value is 1.
#'       \item \code{positive_0} - Numeric. Count of test group (class = `treatment`) samples where the feature value is 0.
#'       \item \code{positive_1} - Numeric. Count of test group samples where the feature value is 1.
#'     }
#'   \item **If the feature is continuous**, the list additionally contains:
#'     \itemize{
#'       \item \code{negative_mean} - Numeric. Mean of the feature values in the reference group.
#'       \item \code{negative_sd} - Numeric. Standard deviation of the feature values in the reference group.
#'       \item \code{positive_mean} - Numeric. Mean of the feature values in the test group.
#'       \item \code{positive_sd} - Numeric. Standard deviation of the feature values in the test group.
#'     }
#' }
#'
#' @examples
#' \dontrun{
#' # Example 1: Binary feature test
#' feature_binary <- c(0, 1, 0, 1, 1, 0, 1, 1)
#' class_labels <- c(0, 0, 0, 0, 1, 1, 1, 1) # 0: Negative, 1: Positive
#' result_bin <- perform_statistical_test(feature_binary, class_labels, CON = 0, treatment = 1)
#' print(result_bin)
#'
#' # Example 2: Continuous feature test
#' feature_cont <- rnorm(30, mean = 10, sd = 2)
#' class_labels_cont <- rep(c(0, 1), each = 15)
#' result_cont <- perform_statistical_test(feature_cont, class_labels_cont, CON = 0, treatment = 1)
#' print(result_cont)
#' }
#'
#' @seealso
#' \code{\link[stats]{fisher.test}} for Fisher's exact test, \code{\link[stats]{chisq.test}} for Chi-squared test.
#' For alternative tests for continuous data (e.g., t-test, Wilcoxon), consider \code{\link[stats]{t.test}} or \code{\link[stats]{wilcox.test}}.
#'
#' @export
perform_statistical_test <- function(feature_vector, class_vector, CON, treatment) {
    # 检查特征类型
    unique_vals <- unique(feature_vector)

    # 二值特征处理 (只有0和1)
    if (all(unique_vals %in% c(0, 1))) {
        # 构建列联表
        contingency_table <- table(class_vector, feature_vector)

        # 计算各组计数
        negative_counts <- as.numeric(contingency_table[CON, ])
        positive_counts <- as.numeric(contingency_table[treatment, ])

        # Fisher精确检验
        test_result <- fisher.test(contingency_table)

        return(list(
            method = "Fisher exact test",
            statistic = test_result$estimate,  # 使用OR值作为统计量
            pvalue = test_result$p.value,
            negative_0 = negative_counts[1],   # 阴性组中特征=0的样本数
            negative_1 = negative_counts[2],   # 阴性组中特征=1的样本数
            positive_0 = positive_counts[1],   # 阳性组中特征=0的样本数
            positive_1 = positive_counts[2],   # 阳性组中特征=1的样本数
            feature_type = "binary"
        ))
    }
    # 连续特征处理
    else {
        # 按组计算均值和标准差
        negative_data <- feature_vector[class_vector == CON]
        positive_data <- feature_vector[class_vector == treatment]

        negative_mean <- mean(negative_data, na.rm = TRUE)
        negative_sd <- sd(negative_data, na.rm = TRUE)
        positive_mean <- mean(positive_data, na.rm = TRUE)
        positive_sd <- sd(positive_data, na.rm = TRUE)

        # 卡方检验（适用于连续变量的组间比较）
        test_result <- chisq.test(feature_vector, class_vector)

        return(list(
            method = "Chi-squared",
            statistic = test_result$statistic,
            pvalue = test_result$p.value,
            negative_mean = negative_mean,   # 阴性组均值
            negative_sd = negative_sd,       # 阴性组标准差
            positive_mean = positive_mean,    # 阳性组均值
            positive_sd = positive_sd,        # 阳性组标准差
            feature_type = "continuous"
        ))
    }
}

