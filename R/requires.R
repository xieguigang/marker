.requires = function() {
    library(ggplot2)
    library(glmnet)
    library(randomForest)
    library(e1071)
    library(caret)
    library(pheatmap)
    library(ggcorrplot)
    library(dplyr)
    library(tidyverse)
    library(randomForest)
    library(xgboost)
    library(SHAPforxgboost)
    library(pROC)
    library(caret)
    library(ggthemes)
    library(xlsx);
    library(openxlsx)
    library(rmda) 

    library(showtext)

    # fix for the zh-cn chars display problem in pdf drawing
    showtext_auto(enable = TRUE)
}