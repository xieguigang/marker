# 1. 数据预处理函数
preprocess_data <- function(data, remove_na = TRUE, normalize = TRUE) {
    # 数据清洗
    data_clean <- data %>%
        filter(!is.na(class));

    colnames(data_clean) <- trimws(colnames(data_clean));

    for(name in colnames(data_clean)) {
        if (name != "class") {
            v <- as.numeric(data_clean[,name]);
            v[is.na(v)] <- mean(v[!is.na(v)])/2;

            if(normalize) {
                v = v / max(v);
            }

            data_clean[,name] = v;
        }
    }

    # 分离特征和标签
    X <- as.matrix(data_clean %>% select(-class))
    y <- factor(data_clean$class, levels = c("阴性", "阳性"));

    return(list(X = X, y = y))
}
