
#' Load required packages for analysis
#'
#' This function loads a comprehensive set of R packages commonly used for data analysis,
#' machine learning, visualization, and handling specific issues like Chinese character
#' display in graphics. It is intended to be executed at the beginning of an analysis
#' script to ensure all necessary dependencies are available.
#'
#' @details
#' The function loads packages from several key categories:
#' \itemize{
#'   \item **Machine Learning**: `glmnet`, `randomForest`, `e1071`, `caret`, `xgboost`, `SHAPforxgboost`
#'   \item **Data Visualization**: `ggplot2`, `pheatmap`, `ggcorrplot`, `ggthemes`, `showtext`
#'   \item **Data Manipulation**: `dplyr`, `tidyverse`
#'   \item **Model Evaluation & Analysis**: `pROC`, `rmda`
#'   \item **Excel File Handling**: `xlsx`, `openxlsx`
#' }
#' Additionally, it enables `showtext` automatic rendering to address Chinese character
#' display issues in PDF graphics[4](@ref).
#'
#' @return
#' This function does not return a value. It is called for its side effects, which are
#' attaching the specified packages to the search path and configuring `showtext`[1](@ref).
#'
#' @section Side effects:
#' \itemize{
#'   \item Attaches multiple packages to the R session using `library()`.
#'   \item Enables `showtext_auto()` to automatically handle text rendering, which is
#'   particularly useful for displaying Chinese characters in graphics[4](@ref).
#' }
#'
#' @section Warning:
#' \itemize{
#'   \item The function uses `library()` to attach packages, which will mask functions
#'   if multiple packages contain functions with the same name. It does not check if
#'   packages are already installed or loaded[3](@ref).
#'   \item The package `randomForest` is loaded twice in the code; you may consider
#'   removing the duplicate call for efficiency.
#'   \item Ensure that all packages listed are installed in your R environment before
#'   calling this function, as missing packages will cause errors[3](@ref).
#' }
#'
#' @examples
#' \dontrun{
#' # Load all required packages
#' .requires()
#' }
#'
#' @seealso
#' \code{\link[base]{library}} for attaching packages, \code{\link[showtext]{showtext_auto}}
#' for configuring text rendering.
#'
#' @export
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
