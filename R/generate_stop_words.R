#' Stop words list
#'
#' This function process free text data into a corpus for further analytics.
#'
#' @import stopwords
#' @import dplyr
#' @param list list of stop words elements.
#' @param type_list type of analysis. By site or by language.
#' @export

generate_stop_words <- function(list, type_list = "site"){

  i <- 1
  stop_word_list <- NULL

  if (type_list == "site") {

    while (i < (length(list) + 1)) {

      if (("Buenos Aires Plant - Argentina" %in% list) |
          ("Naucalpan Plant - Mexico" %in% list)) {

        stop_word_list <-
          append(stop_word_list,
                 stopwords::stopwords(language = "es", source = "stopwords-iso")
          )

      } else if (("Hikari Plant - Japan" %in% list) |
                 ("Osaka Plant - Japan" %in% list)) {

        stop_word_list <-
          append(stop_word_list,
                 stopwords::stopwords(language = "ja", source = "marimo")
          )

      } else if (("Pisa Plant - Italy" %in% list) |
                 ("Rieti Plant - Italy" %in% list)) {

        stop_word_list <-
          append(stop_word_list,
                 stopwords::stopwords(language = "it", source = "stopwords-iso")
          )

      } else if ("Lessines Plant - Belgium" %in% list) {

        stopwords::stopwords(language = "fr", source = "stopwords-iso")

      } else if (("Linz Plant - Austria" %in% list) |
                 ("Oranienburg Plant - Germany" %in% list) |
                 ("Singen Plant - Germany" %in% list) |
                 ("Vienna Plant - Austria" %in% list)) {

        stop_word_list <-
          append(stop_word_list,
                 stopwords::stopwords(language = "de")
          )

      } else {

        stop_word_list <-
          append(stop_word_list,
                 stopwords::stopwords(language = "en", source = "stopwords-iso")
          )

      }

      i <- i + 1

    }

  } else if (type_list == "language")  {

    while (i < (length(list) + 1)) {

      if ("spanish" %in% list) {

        stop_word_list <-
          append(stop_word_list,
                 stopwords::stopwords(language = "es", source = "stopwords-iso")
          )

      } else if ("japanese" %in% list) {

        stop_word_list <-
          append(stop_word_list,
                 stopwords::stopwords(language = "ja", source = "marimo")
          )

      } else if ("italian" %in% list) {

        stop_word_list <-
          append(stop_word_list,
                 stopwords::stopwords(language = "it", source = "stopwords-iso")
          )

      } else if ("french" %in% list) {

        stopwords::stopwords(language = "fr", source = "stopwords-iso")

      } else if ("german" %in% list) {

        stop_word_list <-
          append(stop_word_list,
                 stopwords::stopwords(language = "de")
          )

      } else {

        stop_word_list <-
          append(stop_word_list,
                 stopwords::stopwords(language = "en", source = "stopwords-iso")
          )

      }

      i <- i + 1

    }

  }

  return(stop_word_list)

}
