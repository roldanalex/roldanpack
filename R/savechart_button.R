#' Save chart button
#'
#' This function will create a CSS script for save chart button
#'
#' @import dplyr
#' @import shinyBS
#' @param outputId output id for the button.
#' @param label label for the button.
#' @param class button class type.
#' @param disabled boolean if button is disable or enabled.
#' @param icon_image name of the icon image to be displayed.
#' @export

savechart_button <- function(outputId, label = "Save chart",
                             class = NULL, disabled = FALSE, icon_image = "image"){

  if (disabled == TRUE){

    # Message to display when disabled button is clicked
    disabled_msg = list(p("A software update has disabled the save chart functionality. We are working on a replacement."),
                        p("In the meantime, you can:"),
                        tags$ul(
                          tags$li("Download the data with the Download data button and build new charts in tools like Excel"),
                          tags$li("Take a screenshot of the chart area using ",
                                  tags$a(href="https://support.microsoft.com/en-us/windows/open-snipping-tool-and-take-a-screenshot-a35ac9ff-4a58-24c9-3253-f12bac9f9d44",
                                         "Snipping Tool"),
                                  " or ",
                                  tags$a(href="https://blogs.windows.com/windowsexperience/2019/04/08/windows-10-tip-snip-sketch/",
                                         "Snip and Sketch."),
                                  "At least one of these tools is usually installed on recent versions of Windows."
                          )))

    # Create button without link
    disabled_button = tags$p(id = outputId, class = paste("btn btn-default shiny-download-link", class, "down_disabled"),
                             icon(icon_image), label)

    # Define popup message box
    disabled_popup = shinyBS::bsModal(paste0(outputId, "-disabled-modal"), "Save Chart Disabled", outputId, disabled_msg, size="small")

    # need to explicitly return both ui elements otherwise only the last will be returned
    return(tagList(disabled_button, disabled_popup))


  } else {
    tags$a(id = outputId, class = paste("btn btn-default shiny-download-link", class),
           href = "", target = "_blank", download = NA, icon(icon_image), label)
  }


}
