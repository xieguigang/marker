#' Flexibly convert input data to a data frame
#'
#' @title Convert Input to Data Frame with Class Column Validation
#' @description This function handles multiple input types (file path, data frame, list, etc.)
#'   and converts them into a data frame. It is particularly useful for ensuring downstream 
#'   functions receive a consistent data structure. The function validates the presence of 
#'   a 'class' column and reorders it to the first position for convenience in classification tasks.
#'
#' @details 
#' This function is designed for robustness in data preprocessing pipelines, especially in 
#' bioinformatics and machine learning contexts where a 'class' label column is often required.
#' When a character string is provided, it is treated as a file path and read using `read.csv()` 
#' with settings suitable for bioinformatics data (e.g., `check.names = FALSE` preserves 
#' numeric column names, `row.names = 1` sets the first column as row names). 
#' 
#' Key features:
#' \itemize{
#'   \item Handles character (file paths), data frames, and lists inputs
#'   \item Validates the existence of a 'class' column critical for supervised learning
#'   \item Reorders columns to ensure 'class' is first while preserving other column order
#'   \item Uses conservative CSV reading settings to maintain data integrity
#' }
#'
#' @param data An input object to be converted. Can be:
#'   \itemize{
#'     \item A character string (interpreted as a file path to a CSV file)
#'     \item An existing data frame (returned as-is after validation)
#'     \item A list (coerced to data frame using `as.data.frame`)
#'   }
#'
#' @return A data frame with the following characteristics:
#'   \itemize{
#'     \item If input is a character string: contents of the CSV file as a data frame
#'     \item If input is a data frame: the same data frame with validated 'class' column
#'     \item If input is a list: a data frame coerced from the list
#'     \item The 'class' column is guaranteed to be in the first position
#'     \item Other columns maintain their original order
#'   }
#'
#' @examples
#' # Example with CSV file (requires valid file path)
#' \dontrun{
#' df1 <- dataframe("path/to/your/file.csv")
#' }
#'
#' # Example with data frame (iris dataset modified to include 'class' column)
#' iris_df <- iris
#' iris_df$class <- ifelse(iris_df$Species == "setosa", "setosa", "other")
#' df2 <- dataframe(iris_df)
#' head(df2)
#'
#' # Example with list input
#' my_list <- list(class = c("A", "B", "A"), value = c(1, 2, 3))
#' df3 <- dataframe(my_list)
#' print(df3)
#'
#' @section Errors:
#' The function will stop with an error message in these cases:
#' \itemize{
#'   \item Input cannot be converted to a data frame (e.g., numeric input)
#'   \item The resulting data frame lacks a 'class' column
#'   \item File path provided but file doesn't exist or can't be read
#' }
#'
#' @seealso
#' Base functions used: 
#' \code{\link[base]{is.character}}, \code{\link[base]{is.data.frame}},
#' \code{\link[base]{is.list}}, \code{\link[utils]{read.csv}}, \code{\link[base]{as.data.frame}}
#' 
#' Related packages for enhanced functionality:
#' \code{\link[readr]{read_csv}} for faster CSV reading,
#' \code{\link[data.table]{fread}} for large file support
#'
#' @export
dataframe = function(data) {
    if (is.character(data)) {
        data = read.csv(data, row.names = 1, check.names = FALSE, stringsAsFactors = FALSE);
    } 
    if (is.list(data)) {
        data = as.data.frame(data);
    }
    
    if (is.data.frame(data)) {
        # check and ensure that class column is existsed inside the dataframe
        if (!("class" %in% colnames(data))) {
            stop("missing of the sample class label in your dataframe data input!");
        }
        
        # 将'class'列置于第一列，其他列保持原有顺序
        data <- data[, c("class", setdiff(names(data), "class"))];
        data;
    } else {
        stop("the given input data can not be cast as dataframe table!");
    }
}

safe_read_csv = function(file) {
    # 先读入，不带行名
    df <- read.csv(file, row.names = NULL, check.names = FALSE);
    # 假设原来的第 1 列名是 "SampleID"
    # 用 make.unique 生成唯一名字（重复的会自动加后缀 .1, .2 ...）
    unique_ids <- make.unique(df[,1]);
    # 把这个唯一 ID 设为行名
    rownames(df) <- unique_ids;

    df[, -1, drop = FALSE];
}