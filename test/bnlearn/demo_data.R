# =================================================================
# 动态贝叶斯网络（DBN）测试数据生成脚本
# =================================================================

set.seed(123)  # 设置随机种子以确保结果可重复

# ==================== 参数设置 ====================
n_genes <- 10           # 基因数量
n_timepoints <- 15      # 时间点数量
sample_size <- n_timepoints - 1  # 转换后的样本数（t->t+1）
n_edges <- 12           # 文献网络中边的数量

# ==================== 生成基因名称 ====================
gene_names <- paste0("Gene", 1:n_genes)

# ==================== 生成文献先验网络（白名单）====================
# 创建一个有向图结构
set.seed(456)
# 确保至少有一些真实的因果关系
true_causal_edges <- matrix(0, nrow = n_genes, ncol = n_genes)
true_causal_edges[1, 2] <- 1   # Gene1 -> Gene2
true_causal_edges[2, 3] <- 1   # Gene2 -> Gene3
true_causal_edges[1, 3] <- 1   # Gene1 -> Gene3 (间接)
true_causal_edges[4, 5] <- 1   # Gene4 -> Gene5
true_causal_edges[5, 6] <- 1   # Gene5 -> Gene6
true_causal_edges[7, 8] <- 1   # Gene7 -> Gene8
true_causal_edges[8, 9] <- 1   # Gene8 -> Gene9
true_causal_edges[9, 10] <- 1  # Gene9 -> Gene10
true_causal_edges[2, 5] <- 1   # Gene2 -> Gene5 (跨模块)
true_causal_edges[3, 7] <- 1   # Gene3 -> Gene7 (跨模块)

# 添加一些随机边（文献中的假阳性或潜在关系）
for (i in (n_edges - sum(true_causal_edges) + 1):n_edges) {
  edge_added <- FALSE
  while (!edge_added) {
    from <- sample(1:n_genes, 1)
    to <- sample(1:n_genes, 1)
    if (from != to && true_causal_edges[from, to] == 0) {
      true_causal_edges[from, to] <- 1
      edge_added <- TRUE
    }
  }
}

# 创建先验边列表
prior_edges <- data.frame(
  Source = character(0),
  Target = character(0)
)

for (i in 1:nrow(true_causal_edges)) {
  for (j in 1:ncol(true_causal_edges)) {
    if (true_causal_edges[i, j] == 1) {
      prior_edges <- rbind(prior_edges, data.frame(
        Source = gene_names[i],
        Target = gene_names[j]
      ))
    }
  }
}

cat("生成的文献先验网络包含", nrow(prior_edges), "条边:\n")
print(prior_edges)
cat("\n")

# ==================== 生成时间序列表达数据 ====================
# 初始化表达矩阵 (基因 x 时间点)
expr_matrix <- matrix(0, nrow = n_genes, ncol = n_timepoints)
rownames(expr_matrix) <- gene_names
colnames(expr_matrix) <- paste0("T", 1:n_timepoints)

# 为每个基因生成初始值（T0）
expr_matrix[, 1] <- rnorm(n_genes, mean = 5, sd = 1)

# 生成后续时间点的数据，基于真实的因果关系
for (t in 2:n_timepoints) {
  for (gene in 1:n_genes) {
    # 基因自身的影响（自回归）
    self_influence <- 0.6 * expr_matrix[gene, t-1]
    
    # 来自父基因的影响
    parent_influence <- 0
    parents <- which(true_causal_edges[, gene] == 1)
    if (length(parents) > 0) {
      parent_influence <- sum(0.4 * expr_matrix[parents, t-1] / length(parents))
    }
    
    # 添加噪声
    noise <- rnorm(1, mean = 0, sd = 0.5)
    
    # 计算当前表达值
    expr_matrix[gene, t] <- self_influence + parent_influence + noise
    
    # 确保表达值为正（模拟真实的基因表达数据）
    if (expr_matrix[gene, t] < 0) {
      expr_matrix[gene, t] <- 0.1
    }
  }
}

# 标准化数据（Z-score）
expr_matrix <- t(scale(t(expr_matrix)))

cat("生成的表达矩阵维度:", dim(expr_matrix), "\n")
cat("前6个时间点的表达值（前5个基因）:\n")
print(round(expr_matrix[1:5, 1:6], 3))
cat("\n")

# ==================== 数据准备用于DBN分析 ====================
# 转换数据为 t -> t+1 格式
data_t <- expr_matrix[, 1:(n_timepoints-1)]
data_t_next <- expr_matrix[, 2:n_timepoints]
colnames(data_t_next) <- paste0(colnames(data_t_next), ".next")

# 纵向拼接，此时每一行代表一个时间步的转换
dbn_data <- cbind(as.data.frame(t(data_t)), as.data.frame(t(data_t_next)))

cat("DBN分析数据格式:\n")
cat("维度:", dim(dbn_data), "\n")
cat("前3行示例:\n")
print(round(head(dbn_data, 3), 3))
cat("\n")

# ==================== 创建白名单 ====================
# 格式必须匹配 dbn_data 的列名
whitelist <- data.frame(
  from = prior_edges$Source,
  to = paste0(prior_edges$Target, ".next")
)

cat("白名单（用于DBN分析）:\n")
print(whitelist)
cat("\n")

# ==================== 保存测试数据 ====================
# 保存为RDS格式方便后续使用
saveRDS(list(
  expr_matrix = expr_matrix,
  prior_edges = prior_edges,
  dbn_data = dbn_data,
  whitelist = whitelist,
  true_causal_edges = true_causal_edges
), file = "dbn_test_data.rds")

cat("测试数据已保存为 'dbn_test_data.rds'\n")

# ==================== 可视化生成的数据 ====================
# 1. 绘制基因表达时间序列图
par(mfrow = c(2, 2))
for (i in 1:min(4, n_genes)) {
  plot(1:n_timepoints, expr_matrix[i, ], 
       type = "b", pch = 19, col = i+1,
       main = gene_names[i],
       xlab = "时间点", ylab = "表达值",
       ylim = c(min(expr_matrix), max(expr_matrix)))
}
par(mfrow = c(1, 1))

# 2. 绘制先验网络图
if (require(igraph, quietly = TRUE)) {
  library(igraph)
  
  # 创建图对象
  g <- graph_from_data_frame(prior_edges, directed = TRUE)
  
  # 设置可视化参数
  V(g)$color <- "lightblue"
  V(g)$size <- 25
  V(g)$label.cex <- 0.8
  E(g)$arrow.size <- 0.5
  E(g)$color <- "gray50"
  
  # 绘制网络
  plot(g, layout = layout_with_fr(g),
       main = "文献先验网络")
  
} else {
  cat("igraph包未安装，跳过网络可视化\n")
  cat("安装igraph: install.packages('igraph')\n")
}

# ==================== 数据摘要统计 ====================
cat("\n========== 数据摘要统计 ==========\n")
cat("基因数量:", n_genes, "\n")
cat("时间点数量:", n_timepoints, "\n")
cat("文献网络边数:", nrow(prior_edges), "\n")
cat("真实因果边数:", sum(true_causal_edges), "\n")
cat("DBN样本数:", nrow(dbn_data), "\n")
cat("DBN特征数:", ncol(dbn_data), "\n")

cat("\n表达值统计:\n")
print(summary(as.vector(expr_matrix)))

cat("\n数据生成完成！可以使用以下代码加载数据进行DBN分析:\n")
cat("test_data <- readRDS('dbn_test_data.rds')\n")
cat("expr_matrix <- test_data$expr_matrix\n")
cat("prior_edges <- test_data$prior_edges\n")
cat("dbn_data <- test_data$dbn_data\n")
cat("whitelist <- test_data$whitelist\n")
