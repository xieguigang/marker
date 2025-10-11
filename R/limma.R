limma_filter = function(data, CON = "NC", treatment = "treatment", top = 2000) {
    library(limma);

    # 从dataframe中提取表达矩阵和分组信息
    # 移除class列得到基因表达矩阵
    expr_matrix <- as.matrix(data[, -which(colnames(data) == "class")]);
    # 获取分组信息
    group <- data$class;
    # 确保分组为因子类型，并设置对照水平（CON为对照组）
    group <- factor(group, levels = c(CON, treatment));
    # 构建设计矩阵
    design <- model.matrix(~0 + group);

    colnames(design) <- levels(group)  # 将设计矩阵列名设置为组别名称

    # 设置对比矩阵（比较Treatment组与CON组）
    contrast.matrix <- makeContrasts(Treatment - CON, levels = design)

    # 拟合线性模型
    fit <- lmFit(expr_matrix, design)
    fit2 <- contrasts.fit(fit, contrast.matrix)
    fit2 <- eBayes(fit2)
    # 提取top1000差异基因（按p值排序）
    top1000_genes <- topTable(fit2, coef = 1, number = top, sort.by = "p")

    # 查看结果
    print(head(top1000_genes));
    summary(top1000_genes);

    return(top1000_genes);
}