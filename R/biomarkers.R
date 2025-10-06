marker = function(file_path, class, sel_features = NULL, training_size = 0.7, top_features = 6, save_dir= "./") {
    # 1. 加载数据
    data <- dataframe(data = file_path);
    preprocessed <- preprocess_data(data[data$class %in% class,]);
    X <- preprocessed$X
    y <- preprocessed$y
    name = save_dir;

    print(name);
    print(sel_features);

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
}
