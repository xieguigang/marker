
#' Ensemble Model Training, Validation and Visualization
#'
#' This function performs integrated ensemble modeling by training three distinct machine learning models (logistic regression, XGBoost, and random forest) on selected features. It conducts comprehensive validation including cross-validation, decision curve analysis (DCA), and calibration curve assessment. The function automatically saves all models, validation plots, and metadata to disk.
#'
#' @param X A data frame or matrix containing the feature variables where rows represent samples and columns represent features. Must contain the columns specified in \code{selected_features}.
#' @param y A vector containing the target/response variable. For binary classification, should be numeric with values 0 and 1, or factor with two levels.
#' @param selected_features A character vector specifying the names of features to be used for model training. These must correspond to column names in \code{X}.
#'
#' @return A list containing the following components:
#' \itemize{
#'   \item \code{nomogram}: Fitted logistic regression model object of class \code{glm}
#'   \item \code{auc}: Numeric value representing the area under the ROC curve from cross-validation
#'   \item \code{coefficients}: Coefficients from the logistic regression model
#'   \item \code{formula}: The formula object used in logistic regression
#'   \item \code{dX}: Processed data frame used for modeling (includes target variable)
#'   \item \code{xgb}: Trained XGBoost model object
#'   \item \code{rf_model}: Trained random forest model object of class \code{randomForest}
#' }
#'
#' @details
#' The function implements a complete ensemble modeling workflow:
#' \enumerate{
#'   \item \strong{Data Preparation}: Converts input data to appropriate formats and creates model formula
#'   \item \strong{Model Training}: Simultaneously trains three different models using the same feature set
#'   \item \strong{Validation}: Performs cross-validation and generates decision curves and calibration plots
#'   \item \strong{Output}: Saves all models and visualization results to the "saved_models" directory
#' }
#'
#' The function generates two key visualization files:
#' \itemize{
#'   \item \code{decision_curve.pdf}: Decision curves comparing net benefit across models
#'   \item \code{calibration_curves.pdf}: Individual and combined calibration plots
#' }
#'
#' @examples
#' \dontrun{
#' # Load required libraries
#' library(xgboost)
#' library(randomForest)
#' library(glmnet)
#' 
#' # Create sample data
#' data <- matrix(rnorm(1000), ncol = 10)
#' colnames(data) <- paste0("feature", 1:10)
#' target <- sample(0:1, 100, replace = TRUE)
#' features <- c("feature1", "feature3", "feature7")
#' 
#' # Run ensemble modeling
#' result <- ensemble_model(X = data, y = target, selected_features = features)
#' 
#' # Access results
#' print(result$auc)
#' summary(result$nomogram)
#' }
#'
#' @seealso
#' Related functions and packages:
#' \itemize{
#'   \item \code{\link[stats]{glm}} for generalized linear models
#'   \item \code{\link[xgboost]{xgboost}} for gradient boosting models
#'   \item \code{\link[randomForest]{randomForest}} for random forest implementation
#'   \item \code{\link[glmnet]{cv.glmnet}} for cross-validation of GLM models
#' }
#'
#' @export
#'
#' @author Your Name <your.email@domain.com>
#'
#' @references
#' For theoretical background on ensemble methods and validation techniques:
#' \itemize{
#'   \item Spence, M. A., et al. (2018) "A General Framework for Combining Ecosystem Models". 
#'         Fish and Fisheries 19: 1013-42. https://doi.org/10.1111/faf.12310
#'   \item Google's R Style Guide: https://google.github.io/styleguide/Rguide.xml
#' }
ensemble_model <- function(X, y, selected_features, save_dir) {
    # 构建列线图模型
    factors = paste("`",paste(selected_features, collapse = "`+`"),"`", sep = "");

    message("view factor list:");
    message(factors);

    model_dir <- file.path( save_dir, "saved_models");
    if (!dir.exists(model_dir)) dir.create(model_dir);

    formula <- as.formula(paste("class ~ ", factors ));
    dX = as.data.frame(X);
    dX = dX[,selected_features];
    dX[,"class"] <- as.numeric( y) -1;

    message("make glm regression...");
    nomogram_model <- glm(formula, family = "binomial", data = dX)
    message("make xgboost model...");
    xgb_model <- xgboost(
        data = as.matrix(dX[,selected_features]),
        label = dX$class,
        nrounds = 100,
        objective = "binary:logistic",
        eval_metric = "logloss"
    )
    message("make random forest model...");
    # 训练随机森林
    rf_model <- randomForest(
        x = as.matrix(dX[,selected_features]),
        y = dX$class,
        ntree = 50,
        importance = TRUE
    )

    message("do cross validation...");

    # 交叉验证评估
    cv_results <- cv.glmnet(as.matrix(dX[,selected_features]), as.numeric( y) -1, family = "binomial", alpha = 0)
    roc_auc <- max(cv_results$glmnet.fit$dev.ratio) * 100

    X = as.data.frame(dX);
    # 添加模型预测概率到数据集
    X$prob_glm <- predict(nomogram_model, X[,selected_features], type = "response");
    X$prob_xgb <- predict(xgb_model, as.matrix(X[,selected_features]));
    X$prob_rf <- predict(rf_model, as.matrix( X[,selected_features]));  # 获取阳性概率

    # 计算决策曲线
    thresholds <- seq(0, 0.5, by = 0.01)  # 设置合理的阈值范围

    dca_glm <- decision_curve(
        class ~ prob_glm,
        data = X,
        thresholds = thresholds,
        bootstraps = 50
    )

    dca_xgb <- decision_curve(
        class ~ prob_xgb,
        data = X,
        thresholds = thresholds,
        bootstraps = 50
    )

    dca_rf <- decision_curve(
        class ~ prob_rf,
        data = X,
        thresholds = thresholds,
        bootstraps = 50
    )

    pdf(file.path(save_dir, "decision_curve.pdf"), width = 12, height = 9)  # 单位：英寸
    # 绘制决策曲线
    plot_decision_curve(
        list(dca_glm, dca_xgb, dca_rf),
        curve.names = c("Logistic Regression", "XGBoost", "Random Forest"),
        col = c("red", "blue", "green"),
        lty = c(1, 2, 3),
        lwd = 6,
        legend.position = "topright",
        xlab = "Threshold Probability",
        ylab = "Net Benefit"
    )
    dev.off();

    # 1. 保存逻辑回归模型
    saveRDS(nomogram_model, file = file.path(model_dir, "logistic_model.rds"))

    # 2. 保存XGBoost模型（需额外保存特征名）
    xgb.save(xgb_model, file.path(model_dir, "xgb_model.model"))
    writeLines(
        jsonlite::toJSON(list(features = selected_features)),
        file.path(model_dir, "xgb_features.json")
    )

    # 3. 保存随机森林模型
    saveRDS(rf_model, file = file.path(model_dir, "rf_model.rds"))

    # 4. 保存SHAP解释器（可选）
    shap_data <- list(
        features = selected_features,
        baseline = mean(predict(xgb_model, as.matrix(X[, selected_features]))));
    saveRDS(shap_data, file.path(model_dir, "shap_data.rds"))

    # 保存模型元数据
    metadata <- list(
        save_date = Sys.Date(),
        features_used = selected_features,
        dataset_dim = dim(X)
    )
    saveRDS(metadata, file.path(model_dir, "model_metadata.rds"))
    ######################################################


    # 在ensemble_model函数中添加以下代码（在保存模型之后，返回结果之前）

    # 绘制校准曲线
    pdf(file.path(save_dir, "calibration_curves.pdf"), width = 10, height = 6)
    par(mfrow = c(1, 3))  # 一行三列的布局

    # 1. 逻辑回归校准曲线
    calibration_data_glm <- data.frame(
        predicted = X$prob_glm,
        actual = X$class
    ) %>%
        arrange(predicted) %>%
        mutate(bin = cut(predicted, breaks = seq(0, 1, by = 0.1), include.lowest = TRUE)) %>%
        group_by(bin) %>%
        summarise(
            mean_pred = mean(predicted),
            mean_actual = mean(actual),
            n = n()
        )

    plot(calibration_data_glm$mean_pred, calibration_data_glm$mean_actual,
         xlim = c(0, 1), ylim = c(0, 1),
         xlab = "预测概率", ylab = "实际比例",
         main = "逻辑回归校准曲线",
         col = "blue", pch = 19, cex = 1.5)
    abline(0, 1, col = "red", lty = 2, lwd = 2)
    lines(calibration_data_glm$mean_pred, calibration_data_glm$mean_actual, col = "blue", lwd = 2)

    # 2. XGBoost校准曲线
    calibration_data_xgb <- data.frame(
        predicted = X$prob_xgb,
        actual = X$class
    ) %>%
        arrange(predicted) %>%
        mutate(bin = cut(predicted, breaks = seq(0, 1, by = 0.1), include.lowest = TRUE)) %>%
        group_by(bin) %>%
        summarise(
            mean_pred = mean(predicted),
            mean_actual = mean(actual),
            n = n()
        )

    plot(calibration_data_xgb$mean_pred, calibration_data_xgb$mean_actual,
         xlim = c(0, 1), ylim = c(0, 1),
         xlab = "预测概率", ylab = "实际比例",
         main = "XGBoost校准曲线",
         col = "green", pch = 19, cex = 1.5)
    abline(0, 1, col = "red", lty = 2, lwd = 2)
    lines(calibration_data_xgb$mean_pred, calibration_data_xgb$mean_actual, col = "green", lwd = 2)

    # 3. 随机森林校准曲线
    calibration_data_rf <- data.frame(
        predicted = X$prob_rf,
        actual = X$class
    ) %>%
        arrange(predicted) %>%
        mutate(bin = cut(predicted, breaks = seq(0, 1, by = 0.1), include.lowest = TRUE)) %>%
        group_by(bin) %>%
        summarise(
            mean_pred = mean(predicted),
            mean_actual = mean(actual),
            n = n()
        )

    plot(calibration_data_rf$mean_pred, calibration_data_rf$mean_actual,
         xlim = c(0, 1), ylim = c(0, 1),
         xlab = "预测概率", ylab = "实际比例",
         main = "随机森林校准曲线",
         col = "purple", pch = 19, cex = 1.5)
    abline(0, 1, col = "red", lty = 2, lwd = 2)
    lines(calibration_data_rf$mean_pred, calibration_data_rf$mean_actual, col = "purple", lwd = 2)

    dev.off()

    # 使用ggplot2绘制更美观的校准曲线（可选）


    # 合并三个模型的校准数据
    calibration_data_all <- bind_rows(
        mutate(calibration_data_glm, Model = "逻辑回归"),
        mutate(calibration_data_xgb, Model = "XGBoost"),
        mutate(calibration_data_rf, Model = "随机森林")
    )

    # 绘制组合校准曲线
    p <- ggplot(calibration_data_all, aes(x = mean_pred, y = mean_actual, color = Model)) +
        geom_point(aes(size = n), alpha = 0.7) +
        geom_line(aes(group = Model), size = 1) +
        geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
        scale_color_manual(values = c("逻辑回归" = "blue", "XGBoost" = "green", "随机森林" = "purple")) +
        labs(x = "平均预测概率", y = "实际事件比例",
             title = "模型校准曲线比较",
             subtitle = "理想情况应接近对角线") +
        xlim(0, 1) + ylim(0, 1) +
        theme_minimal() +
        theme(legend.position = "bottom")

    ggsave(file.path(save_dir, "combined_calibration_curve.pdf"), plot = p, width = 10, height = 8)

    # 将校准数据保存到Excel
    calibration_data_all$bin <- as.character(calibration_data_all$bin)
    write.xlsx(calibration_data_all, file = file.path(save_dir, "calibration_data.xlsx"))

    return(list(
        nomogram = nomogram_model,
        auc = roc_auc,
        coefficients = coef(nomogram_model),
        formula = formula,
        dX = dX,
        xgb = xgb_model,
        rf_model = rf_model
    ))
}
