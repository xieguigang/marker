marker = function(file_path, class, sel_features = NULL, save_dir=NULL) {
    # 1. 加载数据
    data <- read.csv(file_path,  row.names = 1, check.names = FALSE);
    preprocessed <- preprocess_data(data[data$class %in% class,]);
    X <- preprocessed$X
    y <- preprocessed$y
    name = save_dir;

    if (is.null(save_dir)) {
        name = dirname(file_path); # paste(class, collapse = " vs ");
    }

    print(name);
    print(sel_features);

    dir.create(name);
    setwd(name);

    if (length(sel_features) == 0) {
        # 3. 特征选择
        lasso_result <- run_lasso(X, y)
        rf_result <- run_random_forest(X, y)
        svm_result <- run_svm_rfe(X, y)

        combined <- c(as.character(lasso_result$features), as.character(rf_result$features), as.character(svm_result$features))
        # 统计次数并降序排序
        counts <- sort(table(combined), decreasing = TRUE)
        # 提取前三个字符串名称
        top_features <- names(head(counts, 6))
    } else {
        top_features <- sel_features;
    }

    split_idx <- createDataPartition(y, p = 0.7, list = FALSE)[,1];
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

    setwd("..");
}
