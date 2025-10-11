#' data - the gene expression data frame, the row is the samples and column is the gene features. there is a column which is named `class` is the class group of the sample rows.
#' CON - the sample class label for sample group CON
#' treatment - the sample class label for sample group Treatment
#' top - select top n genes to returns from this function. top result is sorted via the pvalue
limma_filter = function(data, CON = "NC", treatment = "treatment", top = 2000) {
    library(limma)
    
    # 1. 提取表达矩阵和分组信息
    # 确保数据框中有一个名为'class'的分组列
    expr_matrix <- as.matrix(data[, -which(colnames(data) == "class")])
    group <- data$class
    
    # 2. 将分组信息转换为因子，并明确指定对照水平（CON）在前
    group <- factor(group, levels = c(CON, treatment))
    
    # 3. 构建无截距的设计矩阵
    design <- model.matrix(~0 + group)
    # 显式设置设计矩阵的列名，确保顺序与levels(group)一致
    colnames(design) <- levels(group) # 或直接写为 c(CON, treatment)
    
    # 4. 构建对比矩阵，动态使用函数参数CON和treatment
    # 注意：makeContrasts中的比较顺序是 实验组 - 对照组
    contrast_formula <- paste0(treatment, " - ", CON)
    contrast.matrix <- makeContrasts(contrasts = contrast_formula, levels = design)
    
    # 5. 拟合线性模型
    fit <- lmFit(expr_matrix, design)
    fit2 <- contrasts.fit(fit, contrast.matrix)
    fit2 <- eBayes(fit2)
    
    # 6. 提取所有基因的结果（number = Inf），而不仅仅是top个
    # 这样便于后续自定义筛选
    all_results <- topTable(fit2, coef = 1, number = Inf, adjust.method = "BH", sort.by = "P")
    
    # 7. 筛选显著差异基因（通常使用校正后P值adj.P.Val和logFC阈值）
    # 常用阈值：adj.P.Val < 0.05 且 |logFC| > 1 (或0.585对应1.5倍变化)
    # 此处先按P值排序并取前top个，然后可根据需求进一步筛选
    top_genes <- all_results[order(all_results$P.Value), ][1:top, ]
    # 示例：添加显著性标记列
    # top_genes$significant <- (top_genes$adj.P.Val < 0.05 & abs(top_genes$logFC) > 1)
    
    # 8. 查看结果概览
    print(paste("Top table for contrast:", contrast_formula))
    print(head(top_genes))
    cat("\nResults summary (for top", top, "genes ordered by P.Value):\n")
    print(summary(top_genes))
    
    return(top_genes)
}