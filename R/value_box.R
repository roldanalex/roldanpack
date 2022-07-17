#' Download static plot
#'
#' This function is designed to download plots
#'
#' @import shiny
#' @import dplyr
#' @param value value to be added to the box.
#' @param subtitle subtitle for the box.
#' @param icon select the icon for the box
#' @param color choose the color of the box.
#' @param width width size for the box.
#' @export

value_box <- function(value, subtitle, icon, color, width = 3) {
  div(class = paste0("col-lg-", width, "col-md-6"),
      div(class = "panel panel-primary",
          div(class = "panel-heading", style = paste0("background-color:", color),
              div(class = "row",
                  div(class = "col-xs-3",
                      shiny::icon(icon, "fa-4x")
                  ),
                  div(class = ("col-xs-9 text-right"),
                      div(style = ("font-size: 55px; font-weight: bold;"),
                          shiny::textOutput(value)
                      ),
                      div(subtitle)
                      )
                  )
              )
          )
      )
  }
