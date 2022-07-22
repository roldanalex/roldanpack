#' Create a column index
#'
#' This function will create a column as index for further analytics.
#'
#' @import base
#' @import dplyr
#' @param data data to be processed.
#' @param column_name index column name.
#' @export

index_to_col <- function(data, column_name){
  data <- base::cbind(newColName = base::rownames(data), data)
  base::rownames(data) <- 1:base::nrow(data)
  base::colnames(data)[1] <- column_name
  return (data)
}
