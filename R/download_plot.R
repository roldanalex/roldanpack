#' Download static plot
#'
#' This function is designed to download plots
#'
#' @import shiny
#' @import dplyr
#' @import ggplot2
#' @param exportname name of the CSV file.
#' @param plot plot to be download.
#' @export

download_plot <- function(exportname, plot) {
  shiny::downloadHandler(
    filename = function() {
      paste(exportname, "_",Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      ggplot2::ggsave(file, plot = plot, device = "png", width = 8)
    }
  )
}
