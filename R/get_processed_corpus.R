#' Process corpus
#'
#' This function process free text data into a corpus for further analytics.
#'
#' @import quanteda
#' @import dplyr
#' @param id_field column to be used as document id.
#' @param column_name name of the s3 bucket.
#' @param dataset prefix used on the s3 bucket.
#' @param ngrams number of grams to be used for analysis.
#' @param stop_word_lang vector containing stop words.
#' @param dictionary_vector vector containing data dictionary to be removed.
#' @export


get_processed_corpus <- function(id_field = NULL, column_name, dataset, ngrams = 2,
                                    stop_word_lang, dictionary_vector){

  if (is.null(id_field)) {

    Processed_Corpus <- dataset %>%
      quanteda::corpus(
        text_field = column_name)

  } else {

    Processed_Corpus <- dataset %>%
      quanteda::corpus(
        docid_field = id_field,
        text_field = column_name)

  }

  ## Create tokens
  tokens_words <-
    quanteda::tokens(
      # Takes the corpus
      Processed_Corpus,
      # Remove numbers
      remove_numbers = TRUE,
      # Remove punctuation
      remove_punct = TRUE,
      # Remove symbols
      remove_symbols = TRUE,
      # Remove URL
      remove_url = TRUE,
      # Split up hyphenated words
      split_hyphens = FALSE,
      # And include the doc vars (we'll need them later)
      include_docvars = TRUE
    )

  token_ungd_eq <- quanteda::tokens_select(
    tokens_words,
    dictionary_vector,
    selection = "remove",
    valuetype = "regex",
    verbose = TRUE
    ) %>%
    quanteda::tokens_remove(stop_word_lang, padding  = TRUE) %>%
    quanteda::tokens_ngrams(n = ngrams, concatenator = " ") %>%
    quanteda::tokens_remove(pattern = stop_word_lang)

  ## Remove words and create dfm
  Final_Corpus_dfm <- quanteda::dfm(
    # Take the token object
    token_ungd_eq,
    # Lower the words
    tolower = TRUE,
    # Get the stem of the words
    stem = TRUE,
    # Remove stop words
    remove = stop_word_lang
  )

  return(Final_Corpus_dfm)

}
