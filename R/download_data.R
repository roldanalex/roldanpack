#' Download CSV data
#'
#' This function is designed to download CSV data
#'
#' @import shiny
#' @import dplyr
#' @import readr
#' @param exportname name of the CSV file.
#' @param data data to be download.
#' @export

download_data <- function(exportname, data) {
  shiny::downloadHandler(
    filename = function() {
      paste(exportname, "_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      readr::write_csv(data, file)
    }
  )
}
