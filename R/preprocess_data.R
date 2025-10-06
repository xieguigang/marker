#' Preprocess Dataset for Machine Learning
#'
#' This function performs data cleaning and preprocessing on a dataset intended for machine learning tasks. It handles missing values, optional normalization of numeric features, and separates the data into feature matrix and label vector.
#'
#' @param data A data frame containing the raw dataset. It must include a column named `class` which represents the target variable.
#' @param remove_na Logical, indicating whether to remove rows with missing values in the `class` column. **Note:** This parameter is currently not utilized in the function logic; rows with `NA` in `class` are always removed.
#' @param normalize Logical, indicating whether to normalize numeric feature columns by dividing by their maximum value (scaling to [0, 1]). Default is `TRUE`.
#' @param labels A character vector specifying the factor levels for the `class` column, defining the order of groups (e.g., control group first). Default is `c("CON", "Treatment")`.
#'
#' @return A list containing the following components:
#' \itemize{
#'   \item \code{X} - A numeric matrix representing the processed feature variables (all columns except `class`).
#'   \item \code{y} - A factor vector derived from the `class` column, with levels set according to the `labels` parameter.
#' }
#'
#' @details
#' The preprocessing steps are executed as follows:
#' \enumerate{
#'   \item **Data Cleaning:** Rows with missing values (`NA`) in the `class` column are removed.
#'   \item **Column Name Cleaning:** Leading and trailing whitespace from all column names are trimmed.
#'   \item **Missing Value Imputation:** For each numeric feature column (all columns except `class`), missing values (`NA`) are replaced with half the mean of the non-missing values in that column.
#'   \item **Normalization:** If `normalize = TRUE`, each numeric feature column is divided by its maximum value to scale the data between 0 and 1.
#'   \item **Data Splitting:** The dataset is split into a numeric matrix `X` (features) and a factor vector `y` (labels).
#' }
#' Non-numeric columns other than `class` may cause errors during conversion to matrix and are not explicitly handled in the current function.
#'
#' @examples
#' \dontrun{
#' # Create a sample dataset
#' sample_data <- data.frame(
#'   class = c("CON", "Treatment", "CON", NA, "Treatment"),
#'   feature1 = c(1, 2, NA, 4, 5),
#'   feature2 = c(10, 20, 30, 40, 50)
#' )
#'
#' # Preprocess the data
#' result <- preprocess_data(data = sample_data,
#'                           normalize = TRUE,
#'                           labels = c("CON", "Treatment"))
#'
#' # Check the structure of the results
#' str(result)
#' }
#'
#' @seealso
#' For more advanced preprocessing options (e.g., PCA, centering, scaling), see the \code{\link[caret]{preProcess}} function in the `caret` package.
#'
#' @export
preprocess_data <- function(data, remove_na = TRUE, normalize = TRUE, labels = c("CON","Treatment")) {
    # 数据清洗
    data_clean <- data %>%
        filter(!is.na(class));

    colnames(data_clean) <- trimws(colnames(data_clean));

    for(name in colnames(data_clean)) {
        if (name != "class") {
            v <- as.numeric(data_clean[,name]);
            v[is.na(v)] <- mean(v[!is.na(v)])/2;

            if(normalize) {
                v = v / max(v);
            }

            data_clean[,name] = v;
        }
    }

    # 分离特征和标签
    X <- as.matrix(data_clean %>% select(-class))
    y <- factor(data_clean$class, levels = labels);

    return(list(X = X, y = y))
}
