
# 新增模型加载函数
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