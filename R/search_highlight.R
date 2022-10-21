#' Create a highlight search function
#'
#' This function will highlight query within specific column.
#'
#' @import stringr
#' @import dplyr
#' @import purrr
#' @import tidytext
#' @param column column to highlight.
#' @param pattern pattern to highlight within column.
#' @export

search_highlight <- function(column, pattern){

  extract_wrd <- str_extract_all(column, regex(pattern, ignore_case = T)) %>%
    map(unique)

  highlight <- map(extract_wrd, function(x) str_c("<mark>", x, "</mark>"))

  pmap_chr(list(column, extract_wrd, highlight), function(x, y, z){

    if (length(y) > 0){
      for (i in seq_along(y)){
        if (all(str_detect(x, "^<a href", negate = T))){
          x <- str_replace_all(x, y[i], z[i])
        } else {
          x <- str_extract_all(x, "<[^<>]+>|(?<=>)[^<>]+(?=<)") %>%
            unlist() %>%
            map_chr(function(a){
              if (str_detect(a, "^<")){
                a
              } else {
                str_replace_all(a, y[i], z[i])
              }
            }) %>%
            str_c(collapse = "")
        }
      }

      x

    } else {

        x

    }

  })

}
