#' Flexibly convert input data to a data frame
#'
#' This function handles multiple input types (file path, data frame, list, etc.)
#' and converts them into a data frame. It is particularly useful for ensuring
#' downstream functions receive a consistent data structure. When a character
#' string is provided, it is treated as a file path and read using `read.csv()`
#' with settings suitable for bioinformatics data (e.g., preserving numeric column names).
#'
#' @param data An input object to be converted. Can be a character string (interpreted as a file path),
#'   an existing data frame (returned as-is), or a list (coerced to data frame using `as.data.frame`).
#'
#' @return A data frame. If the input is a character string pointing to a CSV file, the function returns
#'   the contents of that file as a data frame. If the input is already a data frame, it is returned unchanged.
#'   If the input is a list, it is converted to a data frame.
#'
#' @examples
#' \dontrun{
#' # From a CSV file
#' df1 <- dataframe("path/to/your/file.csv")
#'
#' # From an existing data frame
#' df2 <- dataframe(iris)
#'
#' # From a list
#' my_list <- list(a = 1:3, b = letters[1:3])
#' df3 <- dataframe(my_list)
#' }
#'
#' @seealso
#' Base functions used: \code{\link[base]{is.character}}, \code{\link[base]{is.data.frame}},
#' \code{\link[base]{is.list}}, \code{\link[utils]{read.csv}}, \code{\link[base]{as.data.frame}}
#'
#' @export
dataframe = function(data) {
    if (is.character(data)) {
        read.csv(data, row.names = 1, check.names = FALSE, stringsAsFactors = FALSE);
    } else if (is.data.frame(data)) {
        data;
    } else if (is.list(data)) {
        as.data.frame(data);
    } else {
        stop("the given input data can not be cast as dataframe table!");
    }
}
