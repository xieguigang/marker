# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   https://r-pkgs.org
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

# 使用5种机器学习方法进行分类问题建模
#
# 1. lasso, random forest, svm 三种机器学习方法筛选出最重要的5个特征
# 2. 使用筛选出的5个重要特征建立逻辑回归模型, xgboost, random forest三种机器学习模型用于样本预测
# 3. 预测模型都显示出非常高的准确度

.onLoad <- function(libname, pkgname) {
    message("Employing multiple machine learning methods for biomarker discovery and disease prediction model construction.");
    
    .requires();
}
