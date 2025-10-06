# 加载必要的包
library(tidyverse)

# 1. 数据准备与检验函数定义
# 读取数据（假设CSV格式）
data <- read.csv("./data.csv", row.names = 1, check.names = FALSE, stringsAsFactors = TRUE)  # 替换为实际文件路径

# 定义统计检验函数
perform_statistical_test <- function(feature_vector, class_vector) {
  # 检查特征类型
  unique_vals <- unique(feature_vector)
  
  # 二值特征处理 (只有0和1)
  if (all(unique_vals %in% c(0, 1))) {
    # 构建列联表
    contingency_table <- table(class_vector, feature_vector)
    
    # 计算各组计数
    negative_counts <- as.numeric(contingency_table["阴性", ])
    positive_counts <- as.numeric(contingency_table["阳性", ])
    
    # Fisher精确检验
    test_result <- fisher.test(contingency_table)
    
    return(list(
      method = "Fisher exact test",
      statistic = test_result$estimate,  # 使用OR值作为统计量
      pvalue = test_result$p.value,
      negative_0 = negative_counts[1],   # 阴性组中特征=0的样本数
      negative_1 = negative_counts[2],   # 阴性组中特征=1的样本数
      positive_0 = positive_counts[1],   # 阳性组中特征=0的样本数
      positive_1 = positive_counts[2],   # 阳性组中特征=1的样本数
      feature_type = "binary"
    ))
  } 
  # 连续特征处理
  else {
    # 按组计算均值和标准差
    negative_data <- feature_vector[class_vector == "阴性"]
    positive_data <- feature_vector[class_vector == "阳性"]
    
    negative_mean <- mean(negative_data, na.rm = TRUE)
    negative_sd <- sd(negative_data, na.rm = TRUE)
    positive_mean <- mean(positive_data, na.rm = TRUE)
    positive_sd <- sd(positive_data, na.rm = TRUE)
    
    # 卡方检验（适用于连续变量的组间比较）
    test_result <- chisq.test(feature_vector, class_vector)
    
    return(list(
      method = "Chi-squared",
      statistic = test_result$statistic,
      pvalue = test_result$p.value,
      negative_mean = negative_mean,   # 阴性组均值
      negative_sd = negative_sd,       # 阴性组标准差
      positive_mean = positive_mean,    # 阳性组均值
      positive_sd = positive_sd,        # 阳性组标准差
      feature_type = "continuous"
    ))
  }
}

# 2. 数据处理与检验执行
# 确保分类列正确编码
if (!all(levels(data$class) %in% c("阴性", "阳性"))) {
  stop("第二列必须包含'阴性'和'阳性'分类")
}

# 初始化结果数据框（增加新列）
results <- data.frame(
  Feature = character(),
  Test_Method = character(),
  Statistic_Value = numeric(),
  P_Value = numeric(),
  Feature_Type = character(),
  Negative_0 = numeric(),
  Negative_1 = numeric(),
  Positive_0 = numeric(),
  Positive_1 = numeric(),
  Negative_Mean = numeric(),
  Negative_SD = numeric(),
  Positive_Mean = numeric(),
  Positive_SD = numeric(),
  stringsAsFactors = FALSE
)

# 对每个特征进行检验
feature_columns <- colnames(data)
feature_columns <- feature_columns[!(feature_columns %in% c("id","ID","class","Class","Id"))];

for (feature in feature_columns) {
  # 执行统计检验
  test_result <- perform_statistical_test(data[[feature]], data$class)
  
  # 将结果添加到数据框
  results <- rbind(results, data.frame(
    Feature = feature,
    Test_Method = test_result$method,
    Statistic_Value = ifelse(is.null(test_result$statistic), NA, 
                             as.numeric(test_result$statistic)),
    P_Value = test_result$pvalue,
    Feature_Type = test_result$feature_type,
    Negative_0 = ifelse(!is.null(test_result$negative_0), test_result$negative_0, NA),
    Negative_1 = ifelse(!is.null(test_result$negative_1), test_result$negative_1, NA),
    Positive_0 = ifelse(!is.null(test_result$positive_0), test_result$positive_0, NA),
    Positive_1 = ifelse(!is.null(test_result$positive_1), test_result$positive_1, NA),
    Negative_Mean = ifelse(!is.null(test_result$negative_mean), test_result$negative_mean, NA),
    Negative_SD = ifelse(!is.null(test_result$negative_sd), test_result$negative_sd, NA),
    Positive_Mean = ifelse(!is.null(test_result$positive_mean), test_result$positive_mean, NA),
    Positive_SD = ifelse(!is.null(test_result$positive_sd), test_result$positive_sd, NA),
    stringsAsFactors = FALSE
  ))
}

# 4. 结果输出
# 按P值排序
results <- results[order(results$P_Value), ]

# 保存结果到CSV
write.csv(results, "./机器学习/statistical_test_results.csv", row.names = FALSE)

# 输出结果预览
cat("\n统计分析完成! 结果已保存到 statistical_test_results.csv")
cat("\n\n预览结果:\n")
print(head(results, 5))