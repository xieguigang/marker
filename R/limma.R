#' Perform Differential Expression Analysis using Limma
#'
#' This function conducts differential expression analysis between two specified groups (e.g., control vs. treatment)
#' in a gene expression dataset using the limma package. It handles data preprocessing, model fitting, and result
#' extraction, returning a list of top differentially expressed genes based on statistical significance and fold change.
#'
#' @param data A data.frame containing gene expression data. Rows should represent samples, and columns should
#'   represent gene features. Must include a column named `class` indicating the group membership of each sample.
#' @param CON A character string specifying the control group label in the `class` column. Default is "NC".
#' @param treatment A character string specifying the treatment group label in the `class` column. Default is "treatment".
#' @param top An integer specifying the maximum number of top differentially expressed genes to return. Genes are
#'   sorted by adjusted p-value. Default is 2000.
#' @param adj.P.Val A numeric value specifying the significance threshold for adjusted p-values (FDR). Default is 0.05.
#' @param logFC A numeric value specifying the minimum absolute log2 fold change threshold for significance. Default is 1.
#'
#' @return A data.frame containing the results for the top differentially expressed genes. The data.frame includes
#'   columns for log2 fold change (`logFC`), average expression (`AveExpr`), t-statistic (`t`), p-value (`P.Value`),
#'   adjusted p-value (`adj.P.Val`), and other statistics. Only genes passing the `adj.P.Val` and `logFC` thresholds
#'   are included, up to the number specified by `top`.
#'
#' @examples
#' \dontrun{
#' # Load example dataset (replace with actual data)
#' data <- read.csv("gene_expression_data.csv")
#'
#' # Perform differential expression analysis
#' deg_results <- limma_filter(
#'   data = data,
#'   CON = "NC",
#'   treatment = "treatment",
#'   top = 2000,
#'   adj.P.Val = 0.05,
#'   logFC = 1
#' )
#'
#' # View top results
#' head(deg_results)
#' }
#'
#' @references
#' For more information on the limma package and its methods, see:
#' - Smyth, G. K. (2004). Linear models and empirical bayes methods for assessing differential expression in microarray experiments. *Statistical Applications in Genetics and Molecular Biology*, 3(1), Article 3. 
#' - Ritchie, M. E., Phipson, B., Wu, D., Hu, Y., Law, C. W., Shi, W., & Smyth, G. K. (2015). limma powers differential expression analyses for RNA-sequencing and microarray studies. *Nucleic Acids Research*, 43(7), e47. 
#'
#' @seealso
#' [`limma::lmFit()`], [`limma::eBayes()`], [`limma::topTable()`], [`limma::makeContrasts()`]
#'
#' @export
limma_filter = function(data, CON = "NC", treatment = "treatment", top = 2000, 
                       adj.P.Val = 0.05, logFC = 1) {
    library(limma)
    library(dplyr)
    
    # 1. 检查数据方向并转置（如果行是样本，列是基因）
    # 假设输入数据行为样本，列为基因，需要转置为limma标准格式
    if("class" %in% colnames(data)) {
        # 提取分组信息
        group_info <- data$class
        # 移除分组列，保留表达数据
        expr_data <- data[, -which(colnames(data) == "class")]
        # 转置：行为基因，列为样本（limma标准格式）
        expr_matrix <- as.matrix(t(expr_data))
        # 设置基因名作为行名
        rownames(expr_matrix) <- colnames(expr_data)
        colnames(expr_matrix) <- rownames(data)
    } else {
        stop("数据框中必须包含名为'class'的分组列")
    }
    
    # 2. 过滤样本，只保留CON和treatment组的样本
    keep_samples <- group_info %in% c(CON, treatment)
    expr_matrix <- expr_matrix[, keep_samples]
    group <- factor(group_info[keep_samples], levels = c(CON, treatment))
    
    # 3. 构建设计矩阵
    design <- model.matrix(~0 + group)
    colnames(design) <- levels(group)
    
    # 4. 构建对比矩阵
    contrast_formula <- paste0(treatment, " - ", CON)
    contrast.matrix <- makeContrasts(contrasts = contrast_formula, levels = design)
    
    # 5. 拟合线性模型
    fit <- lmFit(expr_matrix, design)
    fit2 <- contrasts.fit(fit, contrast.matrix)
    fit2 <- eBayes(fit2)
    
    # 6. 提取所有基因结果
    all_results <- topTable(fit2, coef = 1, number = Inf, adjust.method = "BH")
    
    # 7. 筛选显著差异基因
    # 首先按调整后P值排序，然后应用阈值筛选
    sig_genes <- all_results %>%
        arrange(adj.P.Val) %>%
        filter(adj.P.Val < adj.P.Val & abs(logFC) > logFC)
    
    # 8. 如果显著基因数量多于top参数，取前top个
    if(nrow(sig_genes) > top) {
        top_genes <- sig_genes[1:top, ]
    } else {
        top_genes <- sig_genes
    }
    
    # 9. 结果摘要
    cat("差异分析结果摘要:\n")
    cat("对比组:", contrast_formula, "\n")
    cat("总基因数:", nrow(all_results), "\n")
    cat("显著差异基因数 (adj.P.Val <", adj.P.Val, "& |logFC| >", logFC, "):", nrow(sig_genes), "\n")
    cat("返回基因数:", nrow(top_genes), "\n")
    
    print(head(top_genes));

    return(top_genes)
}