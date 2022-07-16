#' Download CSV data
#'
#' This function is designed to download CSV data
#'
#' @export
#' @import shiny
#' @import dplyr
#' @param exportname name of the CSV file.
#' @param data data to be download.
#'

download_data <- function(exportname, data) {
  shiny::downloadHandler(
    filename = function() {
      paste(exportname, "_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write_csv(data, file)
    }
  )
}
