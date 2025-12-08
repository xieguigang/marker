#' Perform single metabolite linear regression analysis with comprehensive evaluation
#'
#' This function conducts linear regression analysis for each metabolite in a metabolomics dataset,
#' evaluating the relationship between metabolite concentration and treatment classification.
#' It generates comprehensive performance metrics, ROC curves, and visualization plots
#' for each metabolite, facilitating biomarker discovery and analysis.
#'
#' @param data A data frame containing the metabolomics data. The dataset must include
#'   a column named 'class' containing the group labels as a factor, with subsequent
#'   columns representing metabolite concentrations. The 'class' column should contain
#'   exactly two levels corresponding to the CON and Treatment groups.
#' @param CON Character string specifying the name of the control/reference group
#'   in the 'class' column (e.g., "Control" or "Normal").
#' @param Treatment Character string specifying the name of the treatment/experimental group
#'   in the 'class' column (e.g., "Case" or "Disease").
#' @param save_dir Character string specifying the directory path where all output
#'   files (results, plots) will be saved. The directory will be created if it doesn't exist.
#' @param top_plots Integer specifying the number of top-performing metabolites
#'   (ranked by AUC) for which individual plots will be generated. Default is 50.
#'
#' @return A data frame with one row per metabolite, sorted in descending order by AUC value.
#'   The data frame contains the following columns:
#' \itemize{
#'   \item \code{Metabolite}: Character, name of the metabolite (column name from input data)
#'   \item \code{Model_Equation}: Character, linear regression equation in the format "Treatment_Prob = intercept + slope * metabolite"
#'   \item \code{R2}: Numeric, R-squared value of the linear model indicating goodness-of-fit
#'   \item \code{AUC}: Numeric, area under the ROC curve ranging from 0 to 1 (higher values indicate better classification)
#'   \item \code{Accuracy}: Numeric, classification accuracy at optimal threshold determined by ROC analysis
#'   \item \code{FPR}: Numeric, false positive rate (1 - specificity) at optimal threshold
#'   \item \code{Sensitivity}: Numeric, true positive rate at optimal threshold
#'   \item \code{Specificity}: Numeric, true negative rate at optimal threshold
#'   \item \code{F1score}: Numeric, F1 score (harmonic mean of precision and sensitivity)
#'   \item \code{log2FC}: Numeric, log2 fold change between treatment and control groups
#'   \item \code{pvalue}: Numeric, p-value from Welch's t-test comparing group means
#' }
#'
#' @details
#' This function performs comprehensive analysis for each metabolite in the dataset:
#' \enumerate{
#'   \item Fits a linear regression model: class ~ metabolite concentration
#'   \item Calculates ROC curve and AUC using predictions from the linear model
#'   \item Determines optimal classification threshold maximizing accuracy
#'   \item Computes performance metrics (accuracy, sensitivity, specificity, F1 score)
#'   \item Calculates log2 fold change and statistical significance between groups
#'   \item Generates individual ROC plots and grouped bar plots with statistical annotations
#'   \item Saves top 10 metabolites by AUC in a combined ROC plot
#' }
#'
#' The function creates two subdirectories within \code{save_dir}: "Individual_ROCs" for
#' ROC curves and "Barplots" for concentration comparison plots. It also exports an Excel file
#' with complete results and displays top performers in the console.
#'
#' @examples
#' \donttest{
#' # Load example metabolomics data
#' data <- read.csv("metabolite_data.csv")
#'
#' # Ensure class column is factor with correct levels
#' data$class <- as.factor(data$class)
#'
#' # Create output directory
#' output_dir <- "analysis_results"
#'
#' # Run analysis comparing "Control" vs "Treatment" groups
#' results <- single_linear(data, CON = "Control", Treatment = "Treatment",
#'                         save_dir = output_dir, top_plots = 20)
#'
#' # View top 10 metabolites by AUC
#' head(results, 10)
#' }
#'
#' @seealso
#' Useful links:
#' \itemize{
#'   \item \code{\link[stats]{lm}} for linear regression modeling
#'   \item \code{\link[pROC]{roc}} for ROC curve calculation
#'   \item \code{\link[ggplot2]{ggplot}} for visualization
#'   \item \code{\link[stats]{t.test}} for statistical testing between groups
#' }
#'
#' @export
single_linear = function(data, CON, Treatment, save_dir, top_plots = 50) {
    message("use dir path for export result files of the single linears:");
    message(save_dir);
    message("class label for CON type is:");
    message(CON);
    message("class label for treatment type is:");
    message(Treatment);

    # 初始化结果存储
    results_df <- data.frame(
        Metabolite = character(0),
        Model_Equation = character(0),
        R2 = numeric(0),
        AUC = numeric(0),
        Accuracy = numeric(0),
        Threshold = numeric(0),
        FPR = numeric(0),
        Sensitivity = numeric(0),
        Specificity = numeric(0),
        F1score = numeric(0),
        log2FC = numeric(0),
        pvalue = numeric(0),
        stringsAsFactors = FALSE
    )

    # 创建目录保存各种图表
    dir.create(file.path(save_dir, "Individual_ROCs"), showWarnings = FALSE)
    dir.create(file.path(save_dir, "Barplots"), showWarnings = FALSE)

    # 初始化存储所有ROC对象的列表
    all_rocs <- list()

    # 遍历每个代谢物进行计算（不立即绘图）
    for (i in 2:ncol(data)) {
        metab_name <- colnames(data)[i]

        # 构建线性回归模型
        model <- lm(as.numeric(data$class == Treatment) ~ data[, i], data = data)
        model_summary <- summary(model)
        r2_value <- model_summary$r.squared

        # 预测概率
        predictions <- predict(model)
        roc_obj = NULL;
        auc_value = 0.0;
        best_threshold = 1.0;

        if (all(predictions == 0.0)) {
            message("invalid model of ", metab_name);
        } else {
            # 计算ROC和AUC
            roc_obj <- roc(data$class, predictions, levels = c(CON, Treatment), direction = "<")
            auc_value <- auc(roc_obj)

            # 计算最佳阈值和混淆矩阵
            best_threshold <- coords(roc_obj, "best", ret = "threshold")$threshold
        }

        predicted_class <- ifelse(predictions > best_threshold, Treatment, CON)

        # 确保因子水平一致
        actual_class <- factor(data$class, levels = c(CON, Treatment))
        predicted_class <- factor(predicted_class, levels = c(CON, Treatment))

        conf_matrix <- table(Predicted = predicted_class, Actual = actual_class)

        # 提取混淆矩阵值
        TP <- conf_matrix[Treatment, Treatment]
        TN <- conf_matrix[CON, CON]
        FP <- conf_matrix[Treatment, CON]
        FN <- conf_matrix[CON, Treatment]

        # 计算性能指标
        accuracy <- (TP + TN) / sum(conf_matrix)
        fpr <- FP / (FP + TN)
        sensitivity <- TP / (TP + FN)
        specificity <- TN / (TN + FP)
        precision <- TP / (TP + FP)
        f1_score <- ifelse((precision + sensitivity) > 0,
                           2 * (precision * sensitivity) / (precision + sensitivity),
                           0)

        # 统计检验和log2FC计算
        group1 <- data[data$class == CON, metab_name]
        group2 <- data[data$class == Treatment, metab_name]

        # 计算log2FC
        log2fc <- log2(mean(group2) / mean(group1))

        # t检验
        t_test <- t.test(group2, group1, var.equal = FALSE)
        p_val <- t_test$p.value

        model_eq <- sprintf("%s_Prob = %.4f + %.4f * %s",
                            Treatment, coef(model)[1], coef(model)[2], metab_name)

        # 存储结果
        results_df <- rbind(results_df, data.frame(
            Metabolite = metab_name,
            Model_Equation = model_eq,
            R2 = r2_value,
            AUC = auc_value,
            Accuracy = accuracy,
            Threshold = best_threshold,
            FPR = fpr,
            Sensitivity = sensitivity,
            Specificity = specificity,
            F1score = f1_score,
            log2FC = log2fc,
            pvalue = p_val
        ))

        # 存储ROC对象供后续使用
        all_rocs[[metab_name]] <- list(
            roc = roc_obj,
            auc = auc_value,
            metab_name = metab_name,
            group1 = group1,
            group2 = group2,
            log2fc = log2fc,
            model_eq = model_eq,
            p_val = p_val
        )
    }

    # 按AUC降序排序
    results_df <- results_df[order(results_df$AUC, decreasing = TRUE), ]

    # 仅对AUC排名前top_plots的feature进行绘图
    top_metabolites <- head(results_df$Metabolite, top_plots)

    # 绘制条形图和单个ROC图（仅前top_plots个）
    for (metab_name in top_metabolites) {
        roc_info <- all_rocs[[metab_name]]

        # 创建条形图数据
        plot_data <- data.frame(
            group = factor(c(rep(CON, length(roc_info$group1)),
                          rep(Treatment, length(roc_info$group2)))),
            value = c(roc_info$group1, roc_info$group2)
        )

        # 创建条形图
        y_max <- max(plot_data$value) * 1.2
        p_bar <- ggplot(plot_data, aes(x = group, y = value, fill = group)) +
            stat_summary(fun = mean, geom = "bar", width = 0.6) +
            stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2) +
            geom_jitter(width = 0.1, size = 2, alpha = 0.5) +
            geom_signif(
                comparisons = list(c(CON, Treatment)),
                map_signif_level = function(p) sprintf("p = %.2g", p),
                y_position = y_max,
                textsize = 3
            ) +
            labs(title = metab_name,
                 x = "Group",
                 y = "Concentration",
                 subtitle = sprintf("log2FC = %.2f\nAUC = %.3f\n%s",
                                   roc_info$log2fc, roc_info$auc, roc_info$model_eq)) +
            theme_minimal() +
            theme(legend.position = "none")

        dir.create(save_dir);

        # 保存条形图
        ggsave(file.path(save_dir, "Barplots", paste0(metab_name, "_barplot.pdf")),
               p_bar, width = 6, height = 5)

        # 生成单个ROC图
        p_roc <- ggroc(roc_info$roc, legacy.axes = TRUE) +
            geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray") +
            labs(title = paste0(metab_name, " (AUC = ", round(roc_info$auc, 3), ")"),
                 x = "False Positive Rate", y = "True Positive Rate") +
            theme_minimal() # + 
             # 添加以下行以实现平滑，并隐藏原始锯齿状路径
            # geom_smooth(aes(ymax = after_scale(y), ymin = after_scale(y)), # 确保按分组平滑
            #             se = FALSE,         # 不绘制置信区间
            #             method = "loess",   # 使用loess方法进行平滑
            #             size = 1.2) +       # 设置平滑曲线的粗细
            # geom_path(size = 0)             # 将原始路径的粗细设为0，使其不可见

        ggsave(file.path(save_dir, "Individual_ROCs", paste0(metab_name, "_ROC.pdf")),
               p_roc, width = 6, height = 4)
    }

    # 绘制前10个AUC最好的feature的ROC叠加图
    top10_metabolites <- head(results_df$Metabolite, 10)
    top10_rocs <- lapply(top10_metabolites, function(name) all_rocs[[name]]$roc)
    names(top10_rocs) <- top10_metabolites

    if (length(top10_rocs) > 0) {
        p_combined <- ggroc(top10_rocs, legacy.axes = TRUE) +
            geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray") +
            labs(title = "Top 10 Metabolites by AUC",
                 x = "False Positive Rate", y = "True Positive Rate",
                 color = "Metabolite") +
            theme_minimal() +
            scale_color_discrete(labels = paste0(names(top10_rocs), " (AUC=",
                                                 round(sapply(top10_rocs, auc), 3), ")")) # + 
            # 添加以下行以实现平滑，并隐藏原始锯齿状路径
            # geom_smooth(aes(ymax = after_scale(y), ymin = after_scale(y), group = group), # 确保按分组平滑
            #             se = FALSE,         # 不绘制置信区间
            #             method = "loess",   # 使用loess方法进行平滑
            #             size = 1.2) +       # 设置平滑曲线的粗细
            # geom_path(size = 0)             # 将原始路径的粗细设为0，使其不可见

        ggsave(file.path(save_dir, "Top10_ROCs_Combined.pdf"), p_combined, width = 8, height = 6)
    }

    # 输出前10名代谢物
    cat("Top 10 Metabolites by AUC:\n")
    print(head(results_df, 10))

    # 使用openxlsx包导出结果到Excel
    write.xlsx(results_df, file.path(save_dir, "Metabolite_Regression_Results.xlsx"))

    message("分析完成！结果已保存到: ", save_dir)
    message("总计分析了 ", nrow(results_df), " 个代谢物")
    message("绘制了前 ", top_plots, " 个代谢物的图表")
    message("生成了前10个AUC最佳代谢物的ROC叠加图")

    invisible(NULL);
}
