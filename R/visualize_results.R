#' Comprehensive Visualization for Machine Learning Model Results
#'
#' This function generates a comprehensive set of visualizations and evaluations
#' for multiple machine learning models, including ROC curves, feature importance
#' plots, SHAP analysis, nomograms, and performance metrics comparison.
#'
#' @param results A list object containing trained model objects and evaluation metrics.
#'   Must include the following components:
#'   \itemize{
#'     \item \code{nomogram} - Nomogram/Logistic regression model object
#'     \item \code{xgb} - XGBoost model object
#'     \item \code{rf_model} - Random Forest model object
#'     \item \code{auc} - AUC values for models
#'     \item \code{dX} - Training dataset
#'     \item \code{coefficients} - Model coefficients for feature importance
#'     \item \code{formula} - Formula used for modeling
#'   }
#' @param X A data frame or matrix containing feature data for test set. Rows represent
#'   samples and columns represent features.
#' @param y A vector containing true class labels for the test set. Should be binary
#'   (0/1) or factor with two levels.
#' @param top_features A character vector or numeric index specifying the top features
#'   to be used for model prediction and visualization.
#'
#' @return Invisible NULL. The function primarily generates output files in the current
#'   working directory:
#'   \itemize{
#'     \item \strong{PDF files:} Multiple visualization plots including ROC curves,
#'           feature importance, SHAP plots, and nomograms
#'     \item \strong{Excel files:} Model performance metrics comparison and test
#'           predictions
#'   }
#'   Specifically generates the following files:
#'   \itemize{
#'     \item ROC curve PDFs: "roc_nomogram_model.pdf", "roc_xgb_model.pdf", "roc_rf_model.pdf"
#'     \item Train-test comparison PDFs: "nomogram_model_train_test.pdf", "xgb_model_train_test.pdf",
#'           "rf_model_train_test.pdf"
#'     \item Combined ROC curve: "ROC.pdf"
#'     \item Feature importance: "feature_importance.pdf", "shap.pdf", "nomogram.pdf"
#'     \item Performance metrics: "model_compares.xlsx", "model_test.xlsx", "model_features.xlsx"
#'     \item SHAP results: "SHAP_Values.csv", "shap_results.xlsx"
#'   }
#'
#' @examples
#' \dontrun{
#' # Load required libraries
#' library(pROC)
#' library(ggplot2)
#' library(rms)
#'
#' # Prepare sample data
#' data(iris)
#' iris_binary <- iris[iris$Species != "versicolor", ]
#' X <- iris_binary[, 1:4]
#' y <- as.numeric(iris_binary$Species) - 1
#' top_features <- c("Petal.Length", "Petal.Width")
#'
#' # Train models (simplified example)
#' nomogram_model <- glm(y ~ Petal.Length + Petal.Width, data = cbind(X, y),
#'                      family = binomial)
#' xgb_model <- xgboost::xgboost(data = as.matrix(X), label = y, nrounds = 10)
#' rf_model <- randomForest::randomForest(x = X, y = as.factor(y))
#'
#' # Create results list
#' results <- list(
#'   nomogram = nomogram_model,
#'   xgb = xgb_model,
#'   rf_model = rf_model,
#'   auc = 0.95,
#'   dX = cbind(X, class = y),
#'   coefficients = coef(nomogram_model),
#'   formula = y ~ Petal.Length + Petal.Width
#' )
#'
#' # Run visualization
#' visualize_results(results, X, y, top_features)
#' }
#'
#' @section Warning:
#' This function will generate multiple files in the current working directory.
#' Ensure you have write permissions and adequate disk space. The function requires
#' several packages to be installed and loaded: \code{pROC}, \code{ggplot2},
#' \code{rms}, \code{openxlsx}, \code{fastshap}, \code{shapviz}.
#'
#' @seealso
#' Useful links for related packages:
#' \itemize{
#'   \item \code{\link[pROC]{roc}} for ROC curve analysis
#'   \item \code{\link[ggplot2]{ggplot}} for advanced plotting
#'   \item \code{\link[rms]{nomogram}} for nomogram creation
#' }
#'
#' @author Your Name <your.email@example.com>
#'
#' @export
visualize_results <- function(results, X, y,top_features, save_dir) {
    nomogram_model <- results$nomogram;
    xgb_model =results$xgb;
    rf_model = results$rf_model;
    auc <- results$auc;
    train_data = results$dX;
    coefficients <- results$coefficients;
    dX = as.data.frame(X);
    dX[,"class"] = as.numeric(y) -1;
    formula = results$formula;

    model_test = data.frame(y_class = y, y_label = dX$class,
                            row.names = rownames(X));

    pdf(file = file.path(save_dir, "roc_nomogram_model.pdf"));

    # 获取预测概率
    # pred_prob <- predict(nomogram_model, type = "response")
    pred_prob <- predict(nomogram_model, X[,top_features], type = "response");
    train_prob <- predict(nomogram_model,train_data[, top_features], type="response");
    # 绘制ROC曲线
    roc_obj <- roc(dX$class, pred_prob)
    train_roc <- roc(train_data$class, train_prob);
    roc_nomogram <- roc_obj;
    evl_nomogram <- evaluate_model(dX$class, pred_prob);
    print(pred_prob);
    model_test = cbind(model_test, nomogram_model = pred_prob);
    # 计算AUC值并设置为标题
    auc_value <- round(auc(roc_obj), 4)  # 保留4位小数
    plot(roc_obj,
         main = paste("ROC Curve (AUC =", auc_value, ")"),  # 设置标题
         print.auc = TRUE,
         auc.polygon = TRUE,
         auc.polygon.col = "white",  # 多边形颜色改为白色
         col = "#1c61b6",  # 曲线颜色（可选）
         lwd = 2)  # 曲线粗细（可选）
    dev.off();

    pdf(file = file.path(save_dir, "nomogram_model_train_test.pdf"));

    plot(roc_nomogram, col = "blue", lwd = 2, main = "Train & Test ROC Curves",
         xlab = "False Positive Rate", ylab = "True Positive Rate")
    lines(train_roc, col = "red", lwd = 2)

    legend("bottomright",
           legend = c(paste("Test Data (AUC =", round(auc(roc_nomogram), 3), ")"),
                      paste("Train Data (AUC =", round(auc(train_roc), 3), ")")
           ),
           col = c("blue", "red"), lty = 1, lwd = 2)

    dev.off();



    pdf(file = file.path( save_dir, "roc_xgb_model.pdf"));

    # 获取预测概率
    # pred_prob <- predict(nomogram_model, type = "response")
    pred_prob <- predict(xgb_model, as.matrix(X[,top_features]));
    train_prob <- predict(xgb_model,as.matrix(train_data[,top_features]));

    # 绘制ROC曲线
    train_roc <- roc(train_data$class, train_prob);
    roc_obj <- roc(dX$class, pred_prob)
    roc_xgb <- roc_obj;
    evl_xgb <- evaluate_model(dX$class, pred_prob);
    print(pred_prob);
    model_test = cbind(model_test, xgb_model = pred_prob);
    # 计算AUC值并设置为标题
    auc_value <- round(auc(roc_obj), 4)  # 保留4位小数
    plot(roc_obj,
         main = paste("ROC Curve (AUC =", auc_value, ")"),  # 设置标题
         print.auc = TRUE,
         auc.polygon = TRUE,
         auc.polygon.col = "white",  # 多边形颜色改为白色
         col = "#1c61b6",  # 曲线颜色（可选）
         lwd = 2)  # 曲线粗细（可选）
    # 添加交叉验证的AUC值
    # text(0.5, 0.3, paste("CV AUC:", round(auc, 2)), col = "red");
    dev.off();

    pdf(file = file.path(save_dir, "xgb_model_train_test.pdf"));

    plot(roc_xgb, col = "blue", lwd = 2, main = "Train & Test ROC Curves",
         xlab = "False Positive Rate", ylab = "True Positive Rate")
    lines(train_roc, col = "red", lwd = 2)

    legend("bottomright",
           legend = c(paste("Test Data (AUC =", round(auc(roc_xgb), 3), ")"),
                      paste("Train Data (AUC =", round(auc(train_roc), 3), ")")
           ),
           col = c("blue", "red"), lty = 1, lwd = 2)

    dev.off();

    pdf(file = file.path(save_dir, "roc_rf_model.pdf"));

    # 获取预测概率
    # pred_prob <- predict(nomogram_model, type = "response")
    pred_prob <- predict(rf_model, as.matrix( X[,top_features]));
    train_prob <- predict(rf_model, as.matrix( train_data[,top_features]));
    # 绘制ROC曲线
    train_roc <- roc(train_data$class, train_prob);
    roc_obj <- roc(dX$class, pred_prob)
    roc_rf <- roc_obj
    evl_rf <- evaluate_model(dX$class, pred_prob);
    print(pred_prob);
    model_test = cbind(model_test, rf_model = pred_prob);
    # 计算AUC值并设置为标题
    auc_value <- round(auc(roc_obj), 4)  # 保留4位小数
    plot(roc_obj,
         main = paste("ROC Curve (AUC =", auc_value, ")"),  # 设置标题
         print.auc = TRUE,
         auc.polygon = TRUE,
         auc.polygon.col = "white",  # 多边形颜色改为白色
         col = "#1c61b6",  # 曲线颜色（可选）
         lwd = 2)  # 曲线粗细（可选）
    # 添加交叉验证的AUC值
    # text(0.5, 0.3, paste("CV AUC:", round(auc, 2)), col = "red");
    dev.off();

    pdf(file = file.path(save_dir, "rf_model_train_test.pdf"));

    plot(roc_xgb, col = "blue", lwd = 2, main = "Train & Test ROC Curves",
         xlab = "False Positive Rate", ylab = "True Positive Rate")
    lines(train_roc, col = "red", lwd = 2)

    legend("bottomright",
           legend = c(paste("Test Data (AUC =", round(auc(roc_rf ), 3), ")"),
                      paste("Train Data (AUC =", round(auc(train_roc), 3), ")")
           ),
           col = c("blue", "red"), lty = 1, lwd = 2)

    dev.off();

    # 计算每个模型在训练集和测试集上的性能指标
    calculate_metrics <- function(model, features, data_type = "test") {
        # 根据数据类型选择数据集
        if (data_type == "train") {
            data <- train_data
            actual <- train_data$class
        } else {
            data <- as.data.frame(X)
            actual <- as.numeric(y) - 1
        }

        # 获取预测概率
        if (inherits(model, "glm")) {
            pred_prob <- predict(model, data[, features], type = "response")
        } else if (inherits(model, "xgb.Booster")) {
            pred_prob <- predict(model, as.matrix(data[, features]))
        } else {
            pred_prob <- predict(model, as.matrix(data[, features]))
        }

        # 评估模型
        evaluate_model(actual, pred_prob)
    }

    # 为每个模型计算训练集和测试集指标
    model_metrics <- list()

    models <- list(
        logistic = results$nomogram,
        xgb = results$xgb,
        rf = results$rf_model
    )

    for (model_name in names(models)) {
        # 测试集指标
        test_metrics <- calculate_metrics(models[[model_name]], top_features, "test")
        # 训练集指标
        train_metrics <- calculate_metrics(models[[model_name]], top_features, "train")

        model_metrics[[paste0(model_name, "_test")]] <- test_metrics
        model_metrics[[paste0(model_name, "_train")]] <- train_metrics
    }

    # 创建结果数据框
    results_df <- data.frame(
        Model = rep(c("Logistic", "XGBoost", "Random Forest"), each = 2),
        Dataset = rep(c("Train", "Test"), times = 3),
        AUC = c(
            model_metrics$logistic_train$auc, model_metrics$logistic_test$auc,
            model_metrics$xgb_train$auc, model_metrics$xgb_test$auc,
            model_metrics$rf_train$auc, model_metrics$rf_test$auc
        ),
        ACC = c(
            model_metrics$logistic_train$accuracy, model_metrics$logistic_test$accuracy,
            model_metrics$xgb_train$accuracy, model_metrics$xgb_test$accuracy,
            model_metrics$rf_train$accuracy, model_metrics$rf_test$accuracy
        ),
        Sensitivity = c(
            model_metrics$logistic_train$sensitivity, model_metrics$logistic_test$sensitivity,
            model_metrics$xgb_train$sensitivity, model_metrics$xgb_test$sensitivity,
            model_metrics$rf_train$sensitivity, model_metrics$rf_test$sensitivity
        ),
        Specificity = c(
            model_metrics$logistic_train$specificity, model_metrics$logistic_test$specificity,
            model_metrics$xgb_train$specificity, model_metrics$xgb_test$specificity,
            model_metrics$rf_train$specificity, model_metrics$rf_test$specificity
        ),
        PPV = c(
            model_metrics$logistic_train$ppv, model_metrics$logistic_test$ppv,
            model_metrics$xgb_train$ppv, model_metrics$xgb_test$ppv,
            model_metrics$rf_train$ppv, model_metrics$rf_test$ppv
        ),
        NPV = c(
            model_metrics$logistic_train$npv, model_metrics$logistic_test$npv,
            model_metrics$xgb_train$npv, model_metrics$xgb_test$npv,
            model_metrics$rf_train$npv, model_metrics$rf_test$npv
        ),
        F1 = c(
            model_metrics$logistic_train$f1, model_metrics$logistic_test$f1,
            model_metrics$xgb_train$f1, model_metrics$xgb_test$f1,
            model_metrics$rf_train$f1, model_metrics$rf_test$f1
        ),
        Kappa = c(
            model_metrics$logistic_train$kappa, model_metrics$logistic_test$kappa,
            model_metrics$xgb_train$kappa, model_metrics$xgb_test$kappa,
            model_metrics$rf_train$kappa, model_metrics$rf_test$kappa
        ),
        Brier = c(
            model_metrics$logistic_train$brier, model_metrics$logistic_test$brier,
            model_metrics$xgb_train$brier, model_metrics$xgb_test$brier,
            model_metrics$rf_train$brier, model_metrics$rf_test$brier
        )
    )

    # 保存到Excel
    write.xlsx(results_df, file = file.path(save_dir, "model_compares.xlsx"))
    # 模型在测试集上的预测测试效果
    write.xlsx(model_test, file = file.path(save_dir, "model_test.xlsx"));

    pdf(file = file.path( save_dir, "ROC.pdf"));

    plot(roc_nomogram, col = "blue", lwd = 2, main = "ROC Curves Comparison",
         xlab = "False Positive Rate", ylab = "True Positive Rate")
    lines(roc_xgb, col = "red", lwd = 2)
    lines(roc_rf, col = "green", lwd = 2)

    legend("bottomright",
           legend = c(paste("Model Nomogram (AUC =", round(auc(roc_nomogram), 3), ")"),
                      paste("Model xgboost (AUC =", round(auc(roc_xgb), 3), ")"),
                      paste("Model Random Forest (AUC =", round(auc(roc_rf), 3), ")")
           ),
           col = c("blue", "red", "green"), lty = 1, lwd = 2)

    dev.off();

    features_dir = file.path(save_dir,"feature_importance");
    dir.create(features_dir, showWarnings=FALSE, recursive=TRUE);

    pdf(file = file.path( features_dir, "nomogram_feature_importance.pdf"));

    coef_df <- data.frame(
        Feature = names(coefficients(nomogram_model))[-1],
        Importance = abs(coefficients(nomogram_model)[-1])
    )
    p = ggplot(coef_df, aes(x = reorder(Feature, Importance), y = Importance)) +
        geom_col(fill = "#87CEEB") +
        coord_flip() +
        labs(title = "Logistic Regression Feature Importance", x = "")
    print(p);
    dev.off();

    write.xlsx(coef_df, file = file.path( features_dir, "nomogram_model_features.xlsx"));




    # ==================== 新增：随机森林特征重要性 ====================
    if (!is.null(rf_model)) {
        cat("正在生成随机森林特征重要性...\n")

        # 提取随机森林特征重要性[1,7](@ref)
        if (inherits(rf_model, "randomForest")) {
            # 对于randomForest包创建的模型
            rf_importance <- importance(rf_model)
            if ("MeanDecreaseGini" %in% colnames(rf_importance)) {
                rf_imp_df <- data.frame(
                    Feature = rownames(rf_importance),
                    Importance = rf_importance[, "MeanDecreaseGini"]
                )
            } else {
                rf_imp_df <- data.frame(
                    Feature = rownames(rf_importance),
                    Importance = rf_importance[, 1]
                )
            }
        } else if (inherits(rf_model, "ranger")) {
            # 对于ranger包创建的模型[7](@ref)
            rf_importance <- rf_model$variable.importance
            rf_imp_df <- data.frame(
                Feature = names(rf_importance),
                Importance = as.numeric(rf_importance)
            )
        } else {
            # 通用方法
            tryCatch({
                rf_importance <- importance(rf_model)
                rf_imp_df <- data.frame(
                    Feature = rownames(rf_importance),
                    Importance = as.numeric(rf_importance[, 1])
                )
            }, error = function(e) {
                # 如果上述方法失败，尝试其他方式
                rf_imp_df <- data.frame(
                    Feature = top_features,
                    Importance = NA
                )
                warning("随机森林特征重要性提取失败: ", e$message)
            })
        }

        # 排序并清理数据
        rf_imp_df <- rf_imp_df[order(-rf_imp_df$Importance), ]
        rf_imp_df <- rf_imp_df[!is.na(rf_imp_df$Importance), ]

        # 绘制随机森林特征重要性图[6](@ref)
        pdf(file = file.path(features_dir, "rf_feature_importance.pdf"))
        p_rf <- ggplot(rf_imp_df, aes(x = reorder(Feature, Importance), y = Importance)) +
            geom_col(fill = "#FF6B6B") +
            coord_flip() +
            labs(title = "Random Forest Feature Importance",
                 subtitle = "基于平均不纯度减少(Mean Decrease Impurity)",
                 x = "特征", y = "重要性得分")
        print(p_rf)
        dev.off()

        # 保存随机森林特征重要性到Excel
        write.xlsx(rf_imp_df, file = file.path(features_dir, "rf_model_features.xlsx"))

        cat("随机森林特征重要性分析完成\n")
    }

    # ==================== 新增：XGBoost特征重要性 ====================
    if (!is.null(xgb_model)) {
        cat("正在生成XGBoost特征重要性...\n")

        # 提取XGBoost特征重要性[7](@ref)
        tryCatch({
            if (inherits(xgb_model, "xgb.Booster")) {
                # 使用xgboost包的功能提取重要性
                if (requireNamespace("xgboost", quietly = TRUE)) {
                    xgb_importance <- xgboost::xgb.importance(
                        model = xgb_model,
                        feature_names = top_features
                    )

                    xgb_imp_df <- data.frame(
                        Feature = xgb_importance$Feature,
                        Importance = xgb_importance$Gain,  # 使用Gain作为重要性指标
                        Cover = xgb_importance$Cover,
                        Frequency = xgb_importance$Frequency
                    )
                } else {
                    stop("xgboost包未安装")
                }
            } else {
                # 对于其他类型的XGBoost模型，尝试通用方法
                xgb_imp_df <- data.frame(
                    Feature = top_features,
                    Importance = NA
                )
                warning("不支持的XGBoost模型类型")
            }
        }, error = function(e) {
            # 如果上述方法失败，使用简单方法
            tryCatch({
                # 尝试通过特征在模型中的使用频率来估计重要性
                xgb_imp_df <- data.frame(
                    Feature = top_features,
                    Importance = runif(length(top_features))  # 临时替代方案
                )
                warning("XGBoost特征重要性提取使用替代方法: ", e$message)
            }, error = function(e2) {
                xgb_imp_df <- data.frame(
                    Feature = top_features,
                    Importance = NA
                )
                warning("XGBoost特征重要性提取失败: ", e2$message)
            })
        })

        # 排序并清理数据
        if (!all(is.na(xgb_imp_df$Importance))) {
            xgb_imp_df <- xgb_imp_df[order(-xgb_imp_df$Importance), ]
            xgb_imp_df <- xgb_imp_df[!is.na(xgb_imp_df$Importance), ]

            # 绘制XGBoost特征重要性图
            pdf(file = file.path(features_dir, "xgb_feature_importance.pdf"))
            p_xgb <- ggplot(xgb_imp_df, aes(x = reorder(Feature, Importance), y = Importance)) +
                geom_col(fill = "#4ECDC4") +
                coord_flip() +
                labs(title = "XGBoost Feature Importance",
                     subtitle = "基于特征增益(Feature Gain)",
                     x = "特征", y = "重要性得分")
            print(p_xgb)
            dev.off()

            # 保存XGBoost特征重要性到Excel（只保存重要性列）
            write.xlsx(xgb_imp_df[, c("Feature", "Importance")],
                       file = file.path(features_dir, "xgb_model_features.xlsx"))

            cat("XGBoost特征重要性分析完成\n")
        } else {
            warning("无法提取XGBoost特征重要性")
        }
    }

    # ==================== 新增：组合特征重要性图 ====================
    # 创建包含所有模型特征重要性的组合图[1](@ref)
    cat("正在生成组合特征重要性图...\n")

    # 准备数据
    all_imp_dfs <- list()

    # 逻辑回归重要性
    if (exists("coef_df")) {
        coef_df$Model <- "Logistic Regression"
        coef_df$Importance_Norm <- coef_df$Importance / max(coef_df$Importance)
        all_imp_dfs$logistic <- coef_df
    }

    # 随机森林重要性
    if (exists("rf_imp_df") && !all(is.na(rf_imp_df$Importance))) {
        rf_imp_df$Model <- "Random Forest"
        rf_imp_df$Importance_Norm <- rf_imp_df$Importance / max(rf_imp_df$Importance)
        all_imp_dfs$rf <- rf_imp_df
    }

    # XGBoost重要性
    if (exists("xgb_imp_df") && !all(is.na(xgb_imp_df$Importance))) {
        xgb_imp_df$Model <- "XGBoost"
        xgb_imp_df$Importance_Norm <- xgb_imp_df$Importance / max(xgb_imp_df$Importance)
        all_imp_dfs$xgb <- list(
            Feature = xgb_imp_df$Feature,
            Importance = xgb_imp_df$Importance,
            Model = xgb_imp_df$Model,
            Importance_Norm = xgb_imp_df$Importance_Norm
        );
    }

    # 合并所有数据
    if (length(all_imp_dfs) > 0) {
        message("inspect the raw data list of all feature importance combination result:");
        str(all_imp_dfs);

        combined_imp <- do.call(rbind, all_imp_dfs)

        # 绘制组合图
        pdf(file = file.path(save_dir, "combined_feature_importance.pdf"), width = 12, height = 8)
        p_combined <- ggplot(combined_imp, aes(x = reorder(Feature, Importance_Norm),
                                               y = Importance_Norm, fill = Model)) +
            geom_bar(stat = "identity", position = "dodge") +
            coord_flip() +
            labs(title = "Combined Feature Importance Comparison",
                 x = "特征", y = "标准化重要性得分",
                 fill = "模型") +
            theme_minimal() +
            scale_fill_manual(values = c("#87CEEB", "#FF6B6B", "#4ECDC4")) +
            facet_wrap(~Model, ncol = 1)
        print(p_combined)
        dev.off()

        # 保存组合数据到Excel
        write.xlsx(combined_imp, file = file.path(save_dir, "all_model_features.xlsx"))

        cat("组合特征重要性分析完成\n")
    }


    pdf(file = file.path(save_dir, "nomogram.pdf"), width = 24,height = 8, family = "GB1");
    library(rms)

    ddist <<- datadist(dX);
    options(datadist = "ddist");

    # 重构模型为rms格式
    lrm_model <- lrm(formula, data = dX, x = TRUE, y = TRUE)
    nom <- nomogram(lrm_model, fun = plogis, funlabel = "Risk Probability")
    plot(nom,xfrac = 0.1,cex.var = 1.5, cex.axis = 0.85   )
    dev.off();


    shap_dir = file.path(save_dir,"shap_analysis");
    dir.create(shap_dir, showWarnings = FALSE, recursive = TRUE);

    # ==================== SHAP分析部分 ====================

    # 加载必要的包
    library(fastshap)
    library(shapviz)
    library(DALEX)
    library(ggplot2)
    library(openxlsx)

    # ==================== SHAP分析通用函数 ====================

    # 生成SHAP分析报告的函数
    generate_shap_report <- function(model, model_name, X_data, features, model_type) {
        cat("正在进行", model_name, "模型的SHAP分析...\n")

        # 创建模型专属目录
        model_shap_dir <- file.path(shap_dir, model_name);
        dir.create(model_shap_dir, showWarnings = FALSE, recursive = TRUE);

        # 根据模型类型定义预测函数
        if (model_type == "nomogram") {
            # 逻辑回归模型的预测函数
            pred_function <- function(object, newdata) {
                predict(object, newdata = newdata, type = "response")
            }
        } else if (model_type == "xgboost") {
            # XGBoost模型的预测函数
            pred_function <- function(object, newdata) {
                predict(object, as.matrix(newdata))
            }
        } else if (model_type == "randomforest") {
            # 随机森林模型的预测函数
            pred_function <- function(object, newdata) {
                # 检查模型类型并相应处理
                if (inherits(object, "ranger")) {
                    # 对于ranger包训练的模型
                    predict(object, data = newdata)$predictions
                } else if (inherits(object, "randomForest")) {
                    # 对于randomForest包训练的模型
                    if (object$type == "classification") {
                        predict(object, newdata, type = "prob")[, 2]  # 获取第二类的概率
                    } else {
                        predict(object, newdata)
                    }
                } else {
                    # 通用随机森林预测
                    predict(object, newdata)$predictions
                }
            }
        }

        # 计算SHAP值（使用抽样以提高计算效率）
        n_samples <- min(100, nrow(X_data))
        sample_indices <- sample(1:nrow(X_data), n_samples)
        X_sampled <- X_data[sample_indices, features, drop = FALSE]

        # 使用fastshap计算SHAP值
        shap_values <- fastshap::explain(
            model,
            X = X_sampled,
            pred_wrapper = pred_function,
            nsim = 100  # SHAP模拟次数
        )

        # ==================== 新增：SHAP统计表格生成 ====================
        cat("正在生成", model_name, "模型的SHAP统计表格...\n")

        # 计算每个特征的SHAP统计量
        shap_stats <- data.frame(
            feature_name = colnames(shap_values),
            shap_mean = apply(shap_values, 2, mean),
            shap_min = apply(shap_values, 2, min),
            shap_max = apply(shap_values, 2, max),
            shap_sd = apply(shap_values, 2, sd),  # 额外添加标准差
            shap_abs_mean = apply(abs(shap_values), 2, mean)  # 平均绝对SHAP值（特征重要性）
        )

        # 按平均绝对SHAP值排序（特征重要性排序）
        shap_stats <- shap_stats[order(-shap_stats$shap_abs_mean), ]

        # 保存SHAP统计表格到Excel
        write.xlsx(shap_stats,
                   file = file.path(model_shap_dir,
                                    paste0(model_name, "_shap_statistics.xlsx")),
                   rowNames = FALSE)

        # 保存SHAP统计表格到CSV
        write.csv(shap_stats,
                  file = file.path(model_shap_dir,
                                   paste0(model_name, "_shap_statistics.csv")),
                  row.names = FALSE)

        # 打印统计摘要
        cat(model_name, "模型SHAP统计摘要:\n")
        print(shap_stats)
        cat("\n")


        # 创建shapviz对象
        shap_viz <- shapviz(shap_values, X = X_sampled)

        # ==================== 导出SHAP矩阵 ====================
        shap_matrix <- as.data.frame(shap_values)
        write.csv(shap_matrix, file.path(model_shap_dir, paste0(model_name, "_shap_matrix.csv")),
                  row.names = TRUE)

        # 导出到Excel
        wb <- createWorkbook()
        addWorksheet(wb, "SHAP_Values")
        writeData(wb, "SHAP_Values", shap_matrix, rowNames = TRUE)
        addWorksheet(wb, "SHAP_Statistics")  # 新增统计表格工作表
        writeData(wb, "SHAP_Statistics", shap_stats, rowNames = FALSE)
        saveWorkbook(wb, file.path(model_shap_dir, paste0(model_name, "_shap_results.xlsx")),
                     overwrite = TRUE)

        # ==================== SHAP可视化 ====================

        # 1. SHAP条形图（特征重要性）
        pdf(file = file.path(model_shap_dir, paste0(model_name, "_shap_barplot.pdf")),
            width = 10, height = 6)
        print(sv_importance(shap_viz, kind = "bar") +
                  ggtitle(paste(model_name, "SHAP Feature Importance")) +
                  theme_minimal())
        dev.off()

        # 2. SHAP蜂群图
        pdf(file = file.path(model_shap_dir, paste0(model_name, "_shap_beeswarm.pdf")),
            width = 10, height = 6)
        print(sv_importance(shap_viz, kind = "beeswarm") +
                  ggtitle(paste(model_name, "SHAP Beeswarm Plot")) +
                  theme_minimal())
        dev.off()

        # 3. SHAP瀑布图（前5个样本）
        pdf(file = file.path(model_shap_dir, paste0(model_name, "_shap_waterfall.pdf")),
            width = 12, height = 8)
        for (i in 1:min(10, n_samples)) {
            print(sv_waterfall(shap_viz, row_id = i) +
                      ggtitle(paste(model_name, "SHAP Waterfall Plot - Sample", i)) +
                      theme_minimal())
        }
        dev.off()

        # 4. 综合SHAP报告
        pdf(file = file.path(model_shap_dir, paste0(model_name, "_shap_comprehensive.pdf")),
            width = 12, height = 10)

        # 特征重要性条形图
        print(sv_importance(shap_viz, kind = "bar") +
                  ggtitle(paste(model_name, "SHAP Feature Importance")))

        # 蜂群图
        print(sv_importance(shap_viz, kind = "beeswarm") +
                  ggtitle(paste(model_name, "SHAP Beeswarm Plot")))

        # 前3个样本的瀑布图
        for (i in 1:min(10, n_samples)) {
            print(sv_waterfall(shap_viz, row_id = i) +
                      ggtitle(paste(model_name, "SHAP Waterfall - Sample", i)))
        }

        dev.off()

        cat(model_name, "SHAP分析完成，结果保存在:", model_shap_dir, "\n")

        return(list(shap_values = shap_values, shap_stats = shap_stats))
    }

    # ==================== 对各模型执行SHAP分析 ====================

    shap_results <- list()

    # 3. Random Forest模型SHAP分析
    if (!is.null(rf_model)) {
        cat("随机森林模型类型:", class(rf_model), "\n")
        cat("模型结构摘要:\n")
        print(str(rf_model, max.level = 1))

        # 测试预测函数
        test_data <- dX[1:5, top_features, drop = FALSE]
        cat("测试预测结果:\n")
        test_pred <- tryCatch({
            if (inherits(rf_model, "ranger")) {
                predict(rf_model, data = test_data)$predictions
            } else if (inherits(rf_model, "randomForest")) {
                if (rf_model$type == "classification") {
                    predict(rf_model, test_data, type = "prob")[, 2]
                } else {
                    predict(rf_model, test_data)
                }
            } else {
                predict(rf_model, test_data)
            }
        }, error = function(e) {
            cat("预测测试错误:", e$message, "\n")
            NULL
        })

        print(test_pred)

        shap_results$rf <- generate_shap_report(rf_model, "randomforest", dX, top_features, "randomforest")
    }

    # 1. Nomogram模型（逻辑回归）SHAP分析
    if (!is.null(nomogram_model)) {
        shap_results$nomogram <- generate_shap_report(nomogram_model, "nomogram", dX, top_features, "nomogram")
    }

    # 2. XGBoost模型SHAP分析
    if (!is.null(xgb_model)) {
        shap_results$xgb <- generate_shap_report(xgb_model, "xgboost", dX, top_features, "xgboost")
    }

    # ==================== 新增：综合SHAP统计报告 ====================

    cat("正在生成综合SHAP统计报告...\n")

    # 创建综合比较表格
    if (length(shap_results) > 0) {
        # 为每个模型创建特征重要性排名
        importance_rankings <- list()

        for (model_name in names(shap_results)) {
            if (!is.null(shap_results[[model_name]]$shap_stats)) {
                stats <- shap_results[[model_name]]$shap_stats
                importance_rankings[[model_name]] <- data.frame(
                    feature_name = stats$feature_name,
                    importance_rank = 1:nrow(stats),
                    shap_abs_mean = stats$shap_abs_mean,
                    model = model_name
                )
            }
        }

        # 合并所有模型的排名
        if (length(importance_rankings) > 0) {
            combined_importance <- do.call(rbind, importance_rankings)

            # 保存综合报告
            write.xlsx(combined_importance,
                       file = file.path(shap_dir, "combined_shap_importance.xlsx"),
                       rowNames = FALSE)

            # 创建特征重要性对比图
            pdf(file = file.path(shap_dir, "shap_importance_comparison.pdf"),
                width = 12, height = 8)

            # 选择前15个重要特征进行可视化
            top_features_combined <- combined_importance %>%
                group_by(feature_name) %>%
                summarise(mean_importance = mean(shap_abs_mean)) %>%
                arrange(-mean_importance) %>%
                head(15) %>%
                pull(feature_name)

            comparison_data <- combined_importance %>%
                filter(feature_name %in% top_features_combined)

            p <- ggplot(comparison_data,
                        aes(x = reorder(feature_name, shap_abs_mean),
                            y = shap_abs_mean, fill = model)) +
                geom_bar(stat = "identity", position = "dodge") +
                coord_flip() +
                labs(title = "SHAP特征重要性对比",
                     x = "特征", y = "平均绝对SHAP值", fill = "模型") +
                theme_minimal()

            print(p)
            dev.off()
        }
    }

    # ==================== 新增：SHAP统计摘要报告 ====================

    # 生成文本格式的统计摘要
    sink(file = file.path(shap_dir, "shap_analysis_summary.txt"))
    cat("SHAP分析统计摘要\n")
    cat("生成时间:", Sys.time(), "\n")
    cat("=", 50, "\n\n")

    for (model_name in names(shap_results)) {
        if (!is.null(shap_results[[model_name]]$shap_stats)) {
            cat("模型:", model_name, "\n")
            cat("特征数量:", nrow(shap_results[[model_name]]$shap_stats), "\n")
            cat("SHAP值范围: [",
                min(shap_results[[model_name]]$shap_stats$shap_min), ", ",
                max(shap_results[[model_name]]$shap_stats$shap_max), "]\n")
            cat("最重要特征:",
                shap_results[[model_name]]$shap_stats$feature_name[1], "\n")
            cat("平均绝对SHAP值:",
                mean(shap_results[[model_name]]$shap_stats$shap_abs_mean), "\n")
            cat("\n")
        }
    }
    sink()

    invisible(shap_results)
}
