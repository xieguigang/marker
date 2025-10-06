#' Load ensemble models from a specified directory
#'
#' This function loads pre-saved ensemble machine learning models and their associated metadata from a local directory.
#' It supports loading multiple model types including logistic regression, XGBoost, and random forest models,
#' checking for the existence of each model file before attempting to load them.
#'
#' @param model_dir A character string specifying the path to the directory containing the saved model files.
#'   Default is "saved_models". The function will check this directory for the following files:
#'   \itemize{
#'     \item "logistic_model.rds" for the logistic regression model
#'     \item "xgb_model.model" for the XGBoost model
#'     \item "xgb_features.json" for the XGBoost feature names
#'     \item "rf_model.rds" for the random forest model
#'     \item "model_metadata.rds" for general model metadata
#'   }
#'
#' @return A list containing the loaded models and associated data. The list structure may include:
#'   \itemize{
#'     \item \code{nomogram} - Logistic regression model (if "logistic_model.rds" exists)
#'     \item \code{xgb} - XGBoost model object (if "xgb_model.model" exists)
#'     \item \code{xgb_features} - Character vector of feature names used by the XGBoost model (if "xgb_features.json" exists)
#'     \item \code{rf} - Random forest model (if "rf_model.rds" exists)
#'     \item \code{metadata} - Additional model metadata (if "model_metadata.rds" exists)
#'   }
#'   If a particular model file does not exist, the corresponding list element will be omitted.
#'
#' @examples
#' \dontrun{
#' # Load models from the default directory "saved_models"
#' ensemble_models <- load_ensemble_models()
#'
#' # Load models from a custom directory path
#' ensemble_models <- load_ensemble_models(model_dir = "my_model_directory")
#'
#' # Access individual models
#' logistic_model <- ensemble_models$nomogram
#' xgb_model <- ensemble_models$xgb
#' }
#'
#' @importFrom xgboost xgb.load
#' @importFrom jsonlite fromJSON
#' @export
load_ensemble_models <- function(model_dir = "saved_models") {
    models <- list()

    # 1. 加载逻辑回归模型
    if (file.exists(file.path(model_dir, "logistic_model.rds"))) {
        models$nomogram <- readRDS(file.path(model_dir, "logistic_model.rds"))
    }

    # 2. 加载XGBoost模型及特征
    if (file.exists(file.path(model_dir, "xgb_model.model"))) {
        models$xgb <- xgb.load(file.path(model_dir, "xgb_model.model"))
        feature_json <- jsonlite::fromJSON(file.path(model_dir, "xgb_features.json"))
        models$xgb_features <- feature_json$features
    }

    # 3. 加载随机森林模型
    if (file.exists(file.path(model_dir, "rf_model.rds"))) {
        models$rf <- readRDS(file.path(model_dir, "rf_model.rds"))
    }

    # 4. 加载元数据
    if (file.exists(file.path(model_dir, "model_metadata.rds"))) {
        models$metadata <- readRDS(file.path(model_dir, "model_metadata.rds"))
    }

    return(models)
}
