#' Perform single metabolite linear regression analysis with comprehensive evaluation
#'
#' This function conducts linear regression analysis for each metabolite in the dataset,
#' evaluating the relationship between metabolite concentration and treatment classification.
#' It generates comprehensive performance metrics, ROC curves, and visualization plots.
#'
#' @param data A data frame containing the metabolomics data. The first column should be 
#'   the class labels (as factor), and subsequent columns represent metabolite concentrations.
#' @param NC Character string specifying the name of the negative control/normal group 
#'   in the class column (e.g., "Control" or "Normal").
#' @param Treatment Character string specifying the name of the treatment group 
#'   in the class column (e.g., "Case" or "Disease").
#'
#' @return A data frame with one row per metabolite containing the following columns:
#' \itemize{
#'   \item Metabolite: Character, name of the metabolite
#'   \item Model_Equation: Character, linear regression equation
#'   \item R2: Numeric, R-squared value of the linear model
#'   \item AUC: Numeric, area under the ROC curve
#'   \item Accuracy: Numeric, classification accuracy at optimal threshold
#'   \item FPR: Numeric, false positive rate
#'   \item Sensitivity: Numeric, true positive rate
#'   \item Specificity: Numeric, true negative rate
#'   \item F1score: Numeric, F1 score (harmonic mean of precision and sensitivity)
#'   \item log2FC: Numeric, log2 fold change between treatment and control groups
#'   \item pvalue: Numeric, p-value from t-test between groups
#' }
#'
#' @details
#' This function performs the following analysis for each metabolite:
#' \enumerate{
#'   \item Fits a linear regression model: class ~ metabolite concentration
#'   \item Calculates ROC curve and AUC using predictions from the linear model
#'   \item Determines optimal classification threshold maximizing accuracy
#'   \item Computes comprehensive performance metrics (accuracy, sensitivity, specificity, F1)
#'   \item Calculates log2 fold change and statistical significance between groups
#'   \item Generates individual ROC plots and grouped bar plots with statistical annotations
#'   \item Saves top 10 metabolites by AUC in a combined ROC plot
#' }
#'
#' The function creates two directories: "Individual_ROCs" for ROC curves and 
#' "Barplots" for concentration comparison plots. It also exports an Excel file 
#' with complete results and displays top performers in the console.
#'
#' @examples
#' \dontrun{
#' # Load example metabolomics data
#' data <- read.csv("metabolite_data.csv")
#' 
#' # Ensure class column is factor
#' data$class <- as.factor(data$class)
#' 
#' # Run analysis comparing "Control" vs "Treatment" groups
#' results <- single_linear(data, NC = "Control", Treatment = "Treatment")
#' 
#' # View top 10 metabolites by AUC
#' head(results, 10)
#' }
#'
#' @seealso
#' Useful links:
#' \itemize{
#'   \item \code{\link[stats]{lm}} for linear regression
#'   \item \code{\link[pROC]{roc}} for ROC curve calculation
#'   \item \code{\link[ggplot2]{ggplot}} for visualization
#' }
#'
#' @export
single_linear = function(data, NC, Treatment) {
    # 初始化结果存储
    results_df <- data.frame(
        Metabolite = character(0),
        Model_Equation = character(0),
        R2 = numeric(0),
        AUC = numeric(0),
        Accuracy = numeric(0),
        FPR = numeric(0),
        Sensitivity = numeric(0),
        Specificity = numeric(0),
        F1score = numeric(0),
        log2FC = numeric(0),      # 新增log2FC列
        pvalue = numeric(0),      # 新增p值列
        stringsAsFactors = FALSE
    )

    # 创建目录保存各种图表
    dir.create("Individual_ROCs", showWarnings = FALSE)
    dir.create("Barplots", showWarnings = FALSE)  # 新增条形图目录

    roc_plots <- list()

    # 遍历每个代谢物
    for (i in 2:ncol(data)) {
        metab_name <- colnames(data)[i]

        # 构建线性回归模型
        model <- lm(as.numeric(data$class == Treatment) ~ data[, i], data = data)
        model_summary <- summary(model)
        r2_value <- model_summary$r.squared

        # 预测概率
        predictions <- predict(model)

        # 计算ROC和AUC
        roc_obj <- roc(data$class, predictions, levels = c(NC, Treatment), direction = "<")
        auc_value <- auc(roc_obj)

        # 计算最佳阈值和混淆矩阵
        best_threshold <- coords(roc_obj, "best", ret = "threshold")$threshold
        predicted_class <- ifelse(predictions > best_threshold, Treatment, NC)

        # 确保因子水平一致
        actual_class <- factor(data$class, levels = c(NC, Treatment))
        predicted_class <- factor(predicted_class, levels = c(NC, Treatment))

        conf_matrix <- table(Predicted = predicted_class, Actual = actual_class)

        # 提取混淆矩阵值
        TP <- conf_matrix[Treatment, Treatment]
        TN <- conf_matrix[NC, NC]
        FP <- conf_matrix[Treatment, NC]
        FN <- conf_matrix[NC, Treatment]

        # 计算性能指标
        accuracy <- (TP + TN) / sum(conf_matrix)
        fpr <- FP / (FP + TN)
        sensitivity <- TP / (TP + FN)
        specificity <- TN / (TN + FP)
        precision <- TP / (TP + FP)
        f1_score <- ifelse((precision + sensitivity) > 0,
                           2 * (precision * sensitivity) / (precision + sensitivity),
                           0)

        # === 新增部分：统计检验和log2FC计算 ===
        # 提取两组数据
        group1 <- data[data$class == NC, metab_name]
        group2 <- data[data$class == Treatment, metab_name]

        # 计算log2FC
        log2fc <- log2(mean(group2) / mean(group1))

        # t检验
        t_test <- t.test(group2, group1, var.equal = FALSE)
        p_val <- t_test$p.value

        # 创建绘图数据
        plot_data <- data.frame(
            group = factor(c(rep(NC, length(group1)), rep(Treatment, length(group2)))),
            value = c(group1, group2)
        )

        model_eq <- sprintf("%s_Prob = %.4f + %.4f * %s",
                            Treatment, coef(model)[1], coef(model)[2], metab_name)

        # 创建条形图（带误差线和jitter点）
        y_max <- max(plot_data$value) * 1.2
        p_bar <- ggplot(plot_data, aes(x = group, y = value, fill = group)) +
            stat_summary(fun = mean, geom = "bar", width = 0.6) +
            stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2) +
            geom_jitter(width = 0.1, size = 2, alpha = 0.5) +
            geom_signif(
                comparisons = list(c(NC, Treatment)),
                map_signif_level = function(p) sprintf("p = %.2g", p),
                y_position = y_max,
                textsize = 3
            ) +
            labs(title = metab_name,
                 x = "Group",
                 y = "Concentration",
                 subtitle = sprintf("log2FC = %.2f\nAUC     = %s\n%s", log2fc,round(auc_value, 3), model_eq)) +
            theme_minimal() +
            theme(legend.position = "none")

        # 保存条形图
        ggsave(paste0("Barplots/", metab_name, "_barplot.pdf"), p_bar, width = 6, height = 5)
        # === 结束新增部分 ===

        # 存储结果

        results_df <- rbind(results_df, data.frame(
            Metabolite = metab_name,
            Model_Equation = model_eq,
            R2 = r2_value,
            AUC = auc_value,
            Accuracy = accuracy,
            FPR = fpr,
            Sensitivity = sensitivity,
            Specificity = specificity,
            F1score = f1_score,
            log2FC = log2fc,    # 新增值
            pvalue = p_val      # 新增值
        ))

        # 生成单个ROC图
        p <- ggroc(roc_obj, legacy.axes = TRUE) +
            geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray") +
            labs(title = paste0(metab_name, " (AUC = ", round(auc_value, 3), ")"),
                 x = "False Positive Rate", y = "True Positive Rate") +
            theme_minimal()

        ggsave(paste0("Individual_ROCs/", metab_name, "_ROC.pdf"), p, width = 6, height = 4)

        # 存储前10名候选
        if (length(roc_plots) < 10) {
            roc_plots[[metab_name]] <- list(roc = roc_obj, plot = p)
        } else {
            current_min <- min(sapply(roc_plots, function(x) auc(x$roc)))
            if (auc_value > current_min) {
                # 移除当前最小AUC
                min_index <- which.min(sapply(roc_plots, function(x) auc(x$roc)))
                roc_plots[min_index] <- NULL
                roc_plots[[metab_name]] <- list(roc = roc_obj, plot = p)
            }
        }
    }

    # 按AUC降序排序
    results_df <- results_df[order(results_df$AUC, decreasing = TRUE), ]

    # 绘制前10名ROC叠加图
    if (length(roc_plots) > 0) {
        top_rocs <- lapply(roc_plots, function(x) x$roc)
        ggroc(top_rocs, legacy.axes = TRUE) +
            geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray") +
            labs(title = "Top 10 Metabolites by AUC",
                 x = "False Positive Rate", y = "True Positive Rate",
                 color = "Metabolite") +
            theme_minimal() +
            scale_color_discrete(labels = paste0(names(top_rocs), " (AUC=",
                                                 round(sapply(top_rocs, auc), 3), ")"))
        ggsave("Top10_ROCs_Combined.pdf", width = 8, height = 6)
    }

    # 输出前10名代谢物
    cat("Top 10 Metabolites by AUC:\n")
    print(head(results_df, 10));

    # 导出结果到Excel
    write.xlsx(results_df, "Metabolite_Regression_Results.xlsx");
}
