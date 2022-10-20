#' Create a token dataset
#'
#' This function will create a list of tokenized datasets.
#'
#' @import gtools
#' @import dplyr
#' @import tidyr
#' @import tidytext
#' @import tidyr
#' @param data data to be tokenized.
#' @param remove_stopwords boolean to decide if need to remove stop words.
#' @param no.words number of words to analyze (ngram).
#' @param id id column from dataset.
#' @param stop_words list of stopwords (vector).
#' @param field_search combined column to tokenize.
#' @export

tokenize_data <- function(
    data, id, remove_stopwords = FALSE,
    stop_words = NULL, no.words = 1,
    field_search){

  body <- data %>%
    unnest_tokens(
      word, {{field_search}}, token = "ngrams", n = no.words)

  if (remove_stopwords) {

    if (no.words > 1){

      wi <- str_c("w", seq_len(no.words))
      body <- body %>%
        separate(word, wi, sep = " ") %>%
        filter(across(all_of(wi), ~ !.x %in% stop_words)) %>%
        unite(word, all_of(wi), sep = " ")

    } else {

      body <- body %>%
        anti_join(
          tibble(row = 1, word = stop_words),
          by = "word") %>%
        filter(str_length(word) > 1)

    }

  }

  body %>%
    count({{id}}, word, sort = T) %>%
    bind_tf_idf(term = word, document = {{id}}, n = n)

}
