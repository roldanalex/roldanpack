#' Plot no data available
#'
#' This function will show a message when there's no data to populate a plot
#'
#' @import plotly
#' @import dplyr
#' @param height_plot value showing the height of the plot.
#' @param message message to be used by the function when data is not available.
#' @param text_size text size for the message.
#' @export

plot_nodata <- function(height_plot = 450, message = "No data available", text_size = 20) {
  text_na <- list(x = 5, y = 5, text =  message, size = text_size,
                  xref = "x", yref = "y",  showarrow = FALSE)
  
  plot_ly(height = height_plot) %>%
    layout(annotations = text_na,
           #empty layout
           yaxis = list(showline = FALSE, showticklabels = FALSE, showgrid = FALSE, fixedrange=TRUE),
           xaxis = list(showline = FALSE, showticklabels = FALSE, showgrid = FALSE, fixedrange=TRUE),
           font = list(family = '"Helvetica Neue", Helvetica, Arial, sans-serif')) %>% 
    config( displayModeBar = FALSE) # taking out plotly logo and collaborate button
  }
