#' Create main box
#'
#' This function creates a main box for UI.
#'
#' @import shiny
#' @import dplyr
#' @param title_box title of the box.
#' @param image_name name of the image to be added to the box.
#' @param button_name name of the box button.
#' @param description brief description of the box.
#' @export


lp_main_box <- function(title_box, image_name, button_name, description) {

  div(class="landing-page-box",
      div(title_box, class = "landing-page-box-title"),
      div(description, class = "landing-page-box-description"),
      div(class = "landing-page-icon", style = paste0("background-image: url(", image_name, ".png);
          background-size: auto 80%; background-position: center; background-repeat: no-repeat; ")),
      shiny::actionButton(button_name, NULL, class = "landing-page-button")
  )

}
