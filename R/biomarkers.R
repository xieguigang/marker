
#' Marker Identification for Classification Models
#'
#' This function performs comprehensive marker identification and validation
#' through a multi-step process including data loading, feature selection,
#' model ensemble and result visualization. It integrates LASSO, Random Forest
#' and SVM-RFE algorithms for robust feature selection.
#'
#' @param file_path Character string specifying the path to the input data file.
#'    The file should contain both features and class labels.
#' @param class Vector specifying the class labels to be included in the analysis.
#'    Only samples with these class labels will be used.
#' @param sel_features Optional character vector of pre-selected features.
#'    If provided (default is NULL), the function skips automated feature
#'    selection and uses these features directly.
#' @param training_size Numeric value between 0 and 1 specifying the proportion
#'    of data to be used for training. Default is 0.7 (70% training, 30% testing).
#' @param top_features Integer specifying the number of top features to select
#'    when \code{sel_features} is NULL. Default is 6.
#' @param save_dir Character string specifying the directory path for saving
#'    results. Default is current directory ("./").
#'
#' @return Invisibly returns \code{NULL}. The function primarily generates
#'    visualization outputs and prints diagnostic information to the console.
#'    Key side effects include:
#'    \itemize{
#'      \item Feature selection results when \code{sel_features} is NULL
#'      \item Model ensemble performance metrics
#'      \item Visualization plots in the specified directory
#'    }
#'
#' @details
#' The function executes the following pipeline:
#' \enumerate{
#'   \item Data loading and preprocessing using \code{preprocess_data()}
#'   \item Feature selection (if \code{sel_features} not provided) using three
#'         methods: LASSO, Random Forest, and SVM-RFE
#'   \item Data splitting into training and testing sets
#'   \item Model ensemble training on selected features
#'   \item Result visualization and performance assessment
#' }
#'
#' When \code{sel_features} is NULL, the feature selection process combines
#' results from multiple algorithms and selects the most frequently identified
#' features based on \code{top_features} parameter.
#'
#' @examples
#' \dontrun{
#' # Basic usage with automated feature selection
#' marker("data.csv", class = c("A", "B"), top_features = 10)
#'
#' # Using pre-selected features
#' marker("data.csv", class = "A", sel_features = c("gene1", "gene2"))
#'
#' # Custom training size and save directory
#' marker("data.csv", class = c("A", "B"),
#'        training_size = 0.8, save_dir = "./results/")
#' }
#'
#' @seealso
#' Related functions:
#' \code{\link{preprocess_data}}, \code{\link{run_lasso}},
#' \code{\link{ensemble_model}}, \code{\link{visualize_results}}
#'
#' @export
marker = function(file_path, class, sel_features = NULL, training_size = 0.7, top_features = 6, save_dir= "./") {
    # 1. 加载数据
    data <- dataframe(data = file_path);
    preprocessed <- preprocess_data(data[data$class %in% class,]);
    X <- preprocessed$X
    y <- preprocessed$y

    plot_pca(df = data, dirsave = save_dir);
    single_linear(data, NC = class[1], Treatment = class[2], save_dir = save_dir);
    stats_data(data, CON = class[1], treatment = class[2], save_dir = save_dir);

    message("result data files will be save at location:");
    message(save_dir);
    message("do we havee the selected features for run analysis?");
    message(sel_features);

    if (length(sel_features) == 0) {
        # 3. 特征选择
        lasso_result <- run_lasso(X, y)
        rf_result <- run_random_forest(X, y)
        svm_result <- run_svm_rfe(X, y)

        combined <- c(as.character(lasso_result$features),
                      as.character(rf_result$features),
                      as.character(svm_result$features));
        # 统计次数并降序排序
        counts <- sort(table(combined), decreasing = TRUE)
        # 提取前三个字符串名称
        top_features <- names(head(counts, top_features));
    } else {
        top_features <- sel_features;
    }

    split_idx <- createDataPartition(y, p = training_size, list = FALSE)[,1];
    X = as.data.frame(X);

    print(split_idx);

    train = X[split_idx,];
    train_y = y[split_idx];
    test = X[-split_idx,];
    test_y = y[-split_idx];

    # 4. 模型集成
    ensemble_result <- ensemble_model(train, train_y, top_features)

    # 5. 可视化
    visualize_results(ensemble_result, test, test_y,top_features)

    invisible(NULL);
}
