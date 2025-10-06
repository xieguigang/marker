

#' Predict using a pre-trained ensemble model
#'
#' This function loads a saved ensemble model (comprising XGBoost, Logistic Regression, and Random Forest models)
#' and generates predictions on new data. The ensemble model must have been previously saved using
#' `save_ensemble_models()` or a compatible function.
#'
#' @param new_data A data.frame or matrix containing the new data to make predictions on.
#'   It should contain all the features used during model training.
#' @param model_dir Character string specifying the directory path where the ensemble model
#'   components and metadata are stored. Default is "saved_models".
#'
#' @return A data.frame with row names corresponding to `new_data` and three columns:
#'   \itemize{
#'     \item \code{xgb}: Prediction probabilities from the XGBoost model
#'     \item \code{lr}: Prediction probabilities from the Logistic Regression model  
#'     \item \code{rf}: Prediction probabilities from the Random Forest model
#'   }
#'   All predictions are returned as probabilities (numeric values between 0 and 1).
#'
#' @examples
#' \dontrun{
#' # Load new data for prediction
#' new_samples <- read.csv("new_patients.csv")
#' 
#' # Generate predictions using saved models
#' predictions <- predict_with_saved_model(new_data = new_samples, 
#'                                        model_dir = "my_models")
#' 
#' # View the prediction results
#' head(predictions)
#' 
#' # Extract XGBoost predictions only
#' xgb_predictions <- predictions$xgb
#' }
#'
#' @note 
#' Important considerations for use:
#' \itemize{
#'   \item The function assumes the ensemble model was saved using `save_ensemble_models()` 
#'         or a compatible method that includes metadata about features used during training.
#'   \item The new data must contain at least all the features that were used in model training.
#'   \item Feature names and data types in `new_data` should match those used during training.
#'   \item This function is designed for classification tasks and returns probability scores.
#' }
#'
#' importFrom xgboost predict
#' importFrom stats predict
#' importFrom randomForest predict
#' 
#' @seealso
#' \code{\link{save_ensemble_models}} for saving ensemble models
#' \code{\link[stats]{predict}} for the generic predict function
#' 
#' 
#' @export
predict_with_saved_model <- function(new_data, model_dir = "saved_models") {
    models <- load_ensemble_models(model_dir)

    # 确保新数据特征与训练时一致
    new_data <- new_data[, models$metadata$features_used]

    # XGBoost预测
    xgb_pred <- predict(models$xgb, as.matrix(new_data))

    # 逻辑回归预测
    lr_pred <- predict(models$nomogram, as.data.frame(new_data), type = "response")

    # 随机森林预测
    rf_pred <- predict(models$rf, as.matrix( new_data));

    data.frame(
        xgb = xgb_pred,
        lr = lr_pred,
        rf = rf_pred,
        row.names = rownames(new_data)
    );
}
