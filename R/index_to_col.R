#' Create a column index
#'
#' This function will create a column as index for further analytics.
#'
#' @import dplyr
#' @param data data to be processed.
#' @param column_name index column name.
#' @export

index_to_col <- function(data, column_name){

  data <- cbind(newColName = rownames(data), data)
  rownames(data) <- 1:nrow(data)
  colnames(data)[1] <- column_name
  return (data)

  }
