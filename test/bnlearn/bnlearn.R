# =================================================================
# 1. 环境准备与数据加载
# =================================================================

# 加载 bnlearn 包
if (!requireNamespace("bnlearn", quietly = TRUE)) install.packages("bnlearn")
library(bnlearn)

# 读取刚才生成的测试数据文件
# 请确保 'dbn_test_data.rds' 文件在当前的工作目录 中
test_data <- readRDS("dbn_test_data.rds")

# 关键步骤：将生成的数据赋值给你分析脚本中的变量名
# test_data$expr_matrix -> 你的 data_matrix
# test_data$prior_edges -> 你的 prior_edges
data_matrix <- test_data$expr_matrix
prior_edges <- test_data$prior_edges

cat("数据加载完成！\n")
cat("表达矩阵维度:", dim(data_matrix), "\n")
cat("文献边数量:", nrow(prior_edges), "\n\n")

# =================================================================
# 2. 执行你的分析脚本 (数据预处理)
# =================================================================

# 1. 准备数据 (假设 data_matrix 是 行=基因, 列=时间点 的矩阵)
# 为了使用 bnlearn 做 DBN，我们需要将数据重构为 t -> t+1 的格式
# 即 t时刻的所有基因 和 t+1时刻的所有基因 拼接在一起
data_t <- data_matrix[, 1:(ncol(data_matrix)-1)]
data_t_next <- data_matrix[, 2:ncol(data_matrix)]
rownames(data_t_next) <- paste0(rownames(data_t_next), ".next")

# 纵向拼接，此时每一行代表一个时间步的转换
dbn_data <- cbind(as.data.frame(t(data_t)), as.data.frame(t(data_t_next)))

# =================================================================
# 3. 执行你的分析脚本 (构建白名单)
# =================================================================

# 2. 构建先验网络（白名单）
# 假设 prior_edges 是一个两列的数据框，包含文献挖掘出的基因对
# 格式必须匹配 dbn_data 的列名，即 Source 是 t时刻, Target 是 t+1时刻
whitelist <- data.frame(
  from = prior_edges$Source,
  to = paste0(prior_edges$Target, ".next")
)

cat("白名单构建完成，共", nrow(whitelist), "条先验约束。\n\n")

message("inspect of the dbn_data dataset:");
print(dbn_data);
message("inspect of the whitelist:")
print(whitelist);

# =================================================================
# 4. 执行你的分析脚本 (结构学习)
# =================================================================

# 3. 结构学习 (使用 Tabu 搜索，并限制在白名单范围内)
# 注意：由于demo数据量较小，Tabu搜索非常快。
# 这里的 score 选择 "bic-g" 适用于连续高斯数据
cat("正在进行贝叶斯网络结构学习，请稍候...\n")
fitted_bn <- tabu(dbn_data, score = "bic-g", whitelist = whitelist, blacklist = NULL)

# =================================================================
# 5. 结果查看与可视化
# =================================================================

# 4. 查看结果
cat("\n========== 分析结果 ==========\n")

# 打印学习到的边
learned_arcs <- arcs(fitted_bn)
cat("学习到的有向边数量:", nrow(learned_arcs), "\n")
print(learned_arcs)

# 绘制网络图
plot(fitted_bn, main = "DBN Inferred Network")

# --- 可选：对比真实结果 (因为这是我们生成的假数据，所以有标准答案) ---
true_edges <- test_data$true_causal_edges
# 将真实矩阵转换为边列表
true_list <- which(true_edges == 1, arr.ind = TRUE)
true_df <- data.frame(
  from = rownames(true_edges)[true_list[,1]],
  to = paste0(rownames(true_edges)[true_list[,2]], ".next")
)

# 检查召回率
matched <- merge(learned_arcs, true_df)
cat("\n========== 验证结果 (与生成数据的真实结构对比) ==========\n")
cat("算法成功推断出的真实因果边数量:", nrow(matched), "/", nrow(true_df), "\n")
cat("成功推断的边:\n")
print(matched)
