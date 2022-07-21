#' Highlight word in table
#'
#' This function is to create a CSS script to highlight a specific word
#'
#' @import shiny
#' @import dplyr
#' @param word_highlight word to be highlighted.
#' @param color_highlight color used to highlight the word.
#' @export

wordHighlight <- function(word_highlight, color_highlight = 'yellow') {
  paste0('<span style="background-color:', color_highlight,'">', word_highlight, '</span>')
}
