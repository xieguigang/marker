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

    pdf(file = file.path( save_dir, "feature_importance.pdf"));

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

    write.xlsx(coef_df, file = file.path( save_dir, "model_features.xlsx"));

    library(fastshap)
    library(shapviz)

    # 若需输出类别预测（0/1）
    pred_wrapper_class <- function(model, newdata) {
        as.numeric(predict(model, newdata = newdata, type = "response") > 0.5)
    }
    shap_values <- explain(nomogram_model, X = as.data.frame( X[, top_features]),
                           pred_wrapper =pred_wrapper_class,  # 必须指定
                           nsim = 100,  # 蒙特卡洛模拟次数，建议 >= 100
                           shap_only = FALSE
    );
    shap_viz <- shapviz(shap_values)

    # 提取核心结果
    shap_matrix <- shap_values$shapley_values
    baseline <- shap_values$baseline

    # 导出CSV
    write.csv(shap_matrix, file.path( save_dir, "SHAP_Values.csv"), row.names = TRUE, quote = FALSE)

    # 导出Excel
    wb <- createWorkbook()
    addWorksheet(wb, "SHAP_Values")
    writeData(wb, 1, shap_matrix, rowNames = TRUE)
    addWorksheet(wb, "Baseline")
    writeData(wb, 2, data.frame(Baseline = baseline))
    saveWorkbook(wb, file.path( save_dir, "shap_results.xlsx"), overwrite = TRUE)


    # 3. 可视化
    pdf(file = file.path(save_dir, "./shap.pdf"));
    print(sv_importance(shap_viz))  # 全局特征重要性
    # 蜂群图（Beeswarm plot）
    print(sv_waterfall(shap_viz, row_id = 1))  # 单个样本解释
    dev.off();


    pdf(file = file.path(save_dir, "nomogram.pdf"), width = 24,height = 8, family = "GB1");
    library(rms)

    ddist <<- datadist(dX);
    options(datadist = "ddist");

    # 重构模型为rms格式
    lrm_model <- lrm(formula, data = dX, x = TRUE, y = TRUE)
    nom <- nomogram(lrm_model, fun = plogis, funlabel = "Risk Probability")
    plot(nom,xfrac = 0.1,cex.var = 1.5, cex.axis = 0.85   )
    dev.off();

 library(fastshap)
    library(shapviz)
    library(DALEX)
    library(iBreakDown)
    library(openxlsx)

    shap_dir = file.path(save_dir,"shap_analysis");
    dir.create(shap_dir, showWarnings = FALSE, recursive = TRUE);

    # ==================== SHAP分析部分 ====================
    
    # 1. 为XGBoost模型添加SHAP分析
    if (!is.null(xgb_model)) {
        cat("正在进行XGBoost模型的SHAP分析...\n")
        
        # 方法1: 使用shapviz包（推荐）
        tryCatch({
            # 准备数据
            xgb_data <- as.matrix(X[, top_features])
            
            # 计算SHAP值
            xgb_shap <- shapviz(xgb_model, X_pred = xgb_data)
            
            # 导出SHAP值表格
            shap_values_df <- as.data.frame(xgb_shap$S)
            write.xlsx(shap_values_df, file = file.path(shap_dir, "xgb_shap_values.xlsx"))
            
            # 绘制SHAP可视化图
            pdf(file = file.path(shap_dir, "xgb_shap_plots.pdf"))
            print(sv_importance(xgb_shap, kind = "both"))  # 重要性图
            print(sv_importance(xgb_shap, kind = "beeswarm"))  # 蜂群图
            print(sv_dependence(xgb_shap, v = top_features[1]))  # 依赖图
            dev.off()
            
            cat("XGBoost SHAP分析完成\n")
        }, error = function(e) {
            cat("XGBoost SHAP分析出错:", e$message, "\n")
        })
    }
    
    # 2. 为Random Forest模型添加SHAP分析
    if (!is.null(rf_model)) {
        cat("正在进行Random Forest模型的SHAP分析...\n")
        
        tryCatch({
            # 定义预测函数[1](@ref)
            p_function <- function(model, data) {
                preds <- predict(model, newdata = data)
                if ("predicted" %in% names(preds)) {
                    return(preds$predicted)
                } else {
                    return(preds)
                }
            }
            
            # 构建解释器[1](@ref)
            rf_exp <- DALEX::explain(rf_model,
                                   data = X[, top_features],
                                   y = y,
                                   predict_function = p_function,
                                   label = "Random Forest")
            
            # 计算SHAP值（使用前100个样本以节省时间）
            n_samples <- min(100, nrow(X))
            shap_values <- matrix(nrow = n_samples, ncol = length(top_features))
            colnames(shap_values) <- top_features
            
            for(i in 1:n_samples) {
                shap_rf <- shap(rf_exp, X[i, top_features])
                shap_values[i, ] <- shap_rf$contribution
            }
            
            # 导出SHAP值表格
            shap_df <- as.data.frame(shap_values)
            write.xlsx(shap_df, file = file.path(shap_dir, "rf_shap_values.xlsx"))
            
            # 创建shapviz对象进行可视化[1](@ref)
            sv_data <- as.matrix(X[1:n_samples, top_features])
            baseline <- mean(predict(rf_model, X[, top_features])$predicted)
            
            # 创建shapviz对象
            rf_shap <- list(
                S = shap_values,
                X = sv_data,
                baseline = baseline
            )
            class(rf_shap) <- "shapviz"
            
            # 绘制SHAP图
            pdf(file = file.path(shap_dir, "rf_shap_plots.pdf"))
            if (n_samples > 1) {
                # 重要性图
                sv_importance(rf_shap, kind = "beeswarm")
                # 瀑布图示例
                sv_waterfall(rf_shap, row_id = 1)
            }
            dev.off()
            
            cat("Random Forest SHAP分析完成\n")
        }, error = function(e) {
            cat("Random Forest SHAP分析出错:", e$message, "\n")
        })
    }
    
    # 3. 为Nomogram模型添加SHAP分析
    if (!is.null(nomogram_model)) {
        cat("正在进行Nomogram模型的SHAP分析...\n")
        
        tryCatch({
            # 对于逻辑回归模型，使用fastshap包
            # 定义预测函数
            pred_wrapper <- function(model, newdata) {
                predict(model, newdata = newdata, type = "response")
            }
            
            # 计算SHAP值（使用子集以提高计算效率）
            n_samples <- min(50, nrow(X))
            sample_indices <- sample(1:nrow(X), n_samples)
            
            nomogram_shap <- fastshap::explain(
                nomogram_model,
                X = X[sample_indices, top_features],
                pred_wrapper = pred_wrapper,
                nsim = 50  # 减少模拟次数以提高速度
            )
            
            # 导出SHAP值表格
            shap_df <- as.data.frame(nomogram_shap)
            write.xlsx(shap_df, file = file.path(shap_dir, "nomogram_shap_values.xlsx"))
            
            # 创建shapviz对象进行可视化
            nomogram_shap_viz <- shapviz(nomogram_shap, X = X[sample_indices, top_features])
            
            # 绘制SHAP图
            pdf(file = file.path(shap_dir, "nomogram_shap_plots.pdf"))
            sv_importance(nomogram_shap_viz, kind = "bar")  # 条形图
            if (n_samples > 10) {
                sv_importance(nomogram_shap_viz, kind = "beeswarm")  # 蜂群图
            }
            dev.off()
            
            cat("Nomogram SHAP分析完成\n")
        }, error = function(e) {
            cat("Nomogram SHAP分析出错:", e$message, "\n")
        })
    }
    
    # ==================== 生成综合SHAP报告 ====================
    
    cat("生成综合SHAP报告...\n")
    
    # 创建综合报告PDF
    pdf(file = file.path(shap_dir, "comprehensive_shap_report.pdf"), width = 12, height = 8)
    
    # 为每个模型添加SHAP重要性比较
    if (exists("xgb_shap") && exists("rf_shap")) {
        # 可以添加模型间比较代码
        par(mfrow = c(1, 2))
        if (exists("xgb_shap")) {
            sv_importance(xgb_shap, kind = "bar")
            title("XGBoost特征重要性")
        }
        if (exists("rf_shap")) {
            sv_importance(rf_shap, kind = "bar")
            title("Random Forest特征重要性")
        }
    }
    
    dev.off()

    invisible(NULL);
}
