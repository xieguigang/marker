#' Perform comprehensive statistical analysis on dataset features
#'
#' This function conducts statistical tests on all feature columns within a provided dataset,
#' comparing distributions between negative (class 0) and positive (class 1) groups. It returns
#' and saves detailed results including test statistics, p-values, descriptive statistics,
#' and group-specific metrics.
#'
#' @param data A data frame containing the dataset to be analyzed. Must include a class column
#'   indicating negative/positive groups (coded as 0/1) and feature columns to be tested.
#' @param CON A parameter (specific meaning depends on the implementation of 
#'   `perform_statistical_test`). Could represent a threshold, configuration object, or 
#'   constant value used in the statistical testing procedure.
#' @param treatment A parameter (specific meaning depends on the implementation of 
#'   `perform_statistical_test`). Might indicate treatment groups, experimental conditions, 
#'   or a variable modifying the test behavior.
#' @param save_dir A character string specifying the directory path where the results file 
#'   "statistical_test_results.csv" will be saved.
#'
#' @return A data frame (invisibly) with one row per feature and the following columns:
#' \itemize{
#'   \item \code{Feature}: Character. Name of the feature/column from the input data.
#'   \item \code{Test_Method}: Character. Name of the statistical test used (determined by `perform_statistical_test`).
#'   \item \code{Statistic_Value}: Numeric. The value of the test statistic (e.g., t-value, chi-squared).
#'   \item \code{P_Value}: Numeric. The p-value associated with the statistical test.
#'   \item \code{Feature_Type}: Character. Type of the feature (e.g., continuous, categorical), as determined by `perform_statistical_test`.
#'   \item \code{Negative_0}: Numeric. Metric for the negative group (class 0), specific to the test (e.g., count for a level).
#'   \item \code{Negative_1}: Numeric. Additional metric for the negative group (class 0).
#'   \item \code{Positive_0}: Numeric. Metric for the positive group (class 1), specific to the test.
#'   \item \code{Positive_1}: Numeric. Additional metric for the positive group (class 1).
#'   \item \code{Negative_Mean}: Numeric. Mean of the feature values in the negative group (class 0).
#'   \item \code{Negative_SD}: Numeric. Standard deviation of the feature values in the negative group (class 0).
#'   \item \code{Positive_Mean}: Numeric. Mean of the feature values in the positive group (class 1).
#'   \item \code{Positive_SD}: Numeric. Standard deviation of the feature values in the positive group (class 1).
#' }
#' The full results are also saved to a CSV file named "statistical_test_results.csv" in the directory specified by `save_dir`. The returned data frame is sorted by ascending P_Value.
#'
#' @examples
#' \dontrun{
#' # Assuming `my_df` has a 'class' column and other feature columns
#' results <- stats_data(data = my_df, 
#'                      CON = 0.05, 
#'                      treatment = "drug_A", 
#'                      save_dir = "/path/to/results")
#' 
#' # View top 5 most significant results
#' head(results, 5)
#' }
#'
#' @seealso
#' The internal function \code{perform_statistical_test} which is responsible for the actual statistical testing.
#'
#' @export
stats_data = function(data, CON, treatment, save_dir) {
    # 初始化结果数据框（增加新列）
    results <- data.frame(
        Feature = character(),
        Test_Method = character(),
        Statistic_Value = numeric(),
        P_Value = numeric(),
        Feature_Type = character(),
        Negative_0 = numeric(),
        Negative_1 = numeric(),
        Positive_0 = numeric(),
        Positive_1 = numeric(),
        Negative_Mean = numeric(),
        Negative_SD = numeric(),
        Positive_Mean = numeric(),
        Positive_SD = numeric(),
        stringsAsFactors = FALSE
    )

    # 对每个特征进行检验
    feature_columns <- colnames(data)
    feature_columns <- feature_columns[!(feature_columns %in% c("id","ID","class","Class","Id"))];

    for (feature in feature_columns) {
        # 执行统计检验
        test_result <- perform_statistical_test(data[[feature]], data$class,CON, treatment);

        # 将结果添加到数据框
        results <- rbind(results, data.frame(
            Feature = feature,
            Test_Method = test_result$method,
            Statistic_Value = ifelse(is.null(test_result$statistic), NA,
                                     as.numeric(test_result$statistic)),
            P_Value = test_result$pvalue,
            Feature_Type = test_result$feature_type,
            Negative_0 = ifelse(!is.null(test_result$negative_0), test_result$negative_0, NA),
            Negative_1 = ifelse(!is.null(test_result$negative_1), test_result$negative_1, NA),
            Positive_0 = ifelse(!is.null(test_result$positive_0), test_result$positive_0, NA),
            Positive_1 = ifelse(!is.null(test_result$positive_1), test_result$positive_1, NA),
            Negative_Mean = ifelse(!is.null(test_result$negative_mean), test_result$negative_mean, NA),
            Negative_SD = ifelse(!is.null(test_result$negative_sd), test_result$negative_sd, NA),
            Positive_Mean = ifelse(!is.null(test_result$positive_mean), test_result$positive_mean, NA),
            Positive_SD = ifelse(!is.null(test_result$positive_sd), test_result$positive_sd, NA),
            stringsAsFactors = FALSE
        ))
    }

    # 4. 结果输出
    # 按P值排序
    results <- results[order(results$P_Value), ]

    # 输出结果预览
    cat("\n统计分析完成! 结果已保存到 statistical_test_results.csv")
    cat("\n\n预览结果:\n")
    print(head(results, 5))

    # 保存结果到CSV
    write.csv(results, file.path(save_dir, "statistical_test_results.csv") , row.names = FALSE);
}
