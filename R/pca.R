#' @title PCA Analysis and Visualization
#'
#' @description This function performs Principal Component Analysis (PCA) on a numeric data matrix,
#' generates a score plot visualizing the first two principal components, and saves the plot to a specified directory.
#' The function includes data standardization, PCA computation using singular value decomposition,
#' and visualization with ggplot2 including confidence ellipses and variance explained labels.
#'
#' importFrom stats prcomp scale
#'
#' @param df A data frame containing the data for PCA analysis. Must include a column named 'class'
#'        containing categorical labels for grouping, with all other columns being numeric variables.
#' @param dirsave A character string specifying the directory path where the PCA plot will be saved.
#'
#' @return Invisibly returns NULL. The primary output is a PNG file saved to the specified directory
#'         containing the PCA score plot. The plot displays samples colored by their class membership
#'         with 95% confidence ellipses and variance explained percentages on the axes.
#'
#' @examples
#' \dontrun{
#' # Using the iris dataset as an example (after renaming Species to class)
#' iris_data <- iris
#' colnames(iris_data)[5] <- "class"  # Rename Species to class
#' plot_pca(iris_data, tempdir())
#' }
#'
#' @importFrom dplyr %>% select
#' @importFrom ggplot2 ggplot aes geom_point stat_ellipse labs scale_color_manual
#' @importFrom ggplot2 theme_minimal theme geom_hline geom_vline ggsave
#' @export
plot_pca = function(df, dirsave) {
    # ====== PCA计算核心步骤 ======
    # 1. 数据预处理：分离分类标签和数值数据
    class_labels <- df$class  # 保存分类标签
    pca_data <- df %>% select(-class)  # 移除分类标签列

    # 2. 数据标准化（重要步骤）
    scaled_data <- scale(pca_data)  # 中心化+标准化

    # 3. 执行PCA分析
    pca_result <- prcomp(scaled_data, center = TRUE, scale. = TRUE)

    # 4. 提取主成分得分和方差解释率
    pc_scores <- as.data.frame(pca_result$x[, 1:2])  # 取前两个主成分
    pc_scores$class <- class_labels  # 添加分类标签
    variance <- summary(pca_result)$importance[2, 1:2] * 100  # 计算方差贡献率

    # ====== 颜色设置 (修改部分) ======
    # 计算唯一分类的数量
    n_classes <- length(unique(pc_scores$class))
    
    # 动态生成颜色 (使用 hcl.colors，支持任意数量的分类)
    # "Set3" 是一个配色方案，也可以尝试 "Dynamic", "Pastel1" 等
    my_colors <- hcl.colors(n_classes, palette = "Set3")

    # ====== 可视化 ======
    # 5. 绘制PCA得分图
    ggplot(pc_scores, aes(x = PC1, y = PC2, color = class)) +
        geom_point(size = 3, alpha = 0.8) +  # 绘制样本点
        stat_ellipse(level = 0.95, linewidth = 1, show.legend = FALSE) +  # 添加95%置信椭圆 (注意：类别太多时椭圆可能会重叠严重，可选择去掉)
        labs(title = "PCA Score Plot",
            x = paste0("PC1 (", round(variance[1], 1), "%)"),
            y = paste0("PC2 (", round(variance[2], 1), "%)")) +
        # 关键修改：使用动态生成的 my_colors 替代原本的固定 3 个颜色
        scale_color_manual(values = my_colors) +  
        theme_minimal(base_size = 12) +
        theme(
        panel.grid.major = element_line(color = "grey90", linewidth = 0.25),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(fill = NA, color = "black", linewidth = 0.8),
        legend.position = "right",
        # 如果分类太多，图例可能会挡住图，可以尝试将 legend.position 设为 "none"
        plot.title = element_text(hjust = 0.5, face = "bold")
        ) +
        geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
        geom_vline(xintercept = 0, linetype = "dashed", color = "grey50")

    dir.create(dirsave);

    # 6. 保存结果（可选）
    ggsave(file.path(dirsave, "PCA_plot.png"), width = 8, height = 6, dpi = 300);
    ggsave(file.path(dirsave, "PCA_plot.pdf"), width = 8, height = 6);

    writeLines(as.character( variance), con = file.path(dirsave,"PCA_importance.txt"));
    write.csv(pc_scores, file = file.path(dirsave,"PCA_scores.csv"));
}
