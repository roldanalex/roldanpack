#' Create a search word engine
#'
#' This function look for specific keywords within dataset.
#'
#' @import gtools
#' @import dplyr
#' @import tidyr
#' @import tidytext
#' @import tidyr
#' @param data data to be processed.
#' @param token_list list of processed tokens.
#' @param query keywords to search.
#' @param id id column from dataset.
#' @param stop_words list of stopwords (vector).
#' @param n_top max number of results.
#' @export

search_words <- function(data, token_list, query, id,
                         list_n = 3, stop_words, n_top = 200){

  a <- query %>%
    tolower %>%
    str_extract_all("\\w+") %>%
    unlist() %>%
    .[!. %in% stop_words] %>%
    unique()

  word_count <- length(a)

  word_combo <- seq_len(min(word_count, list_n)) %>%
    map(function(x) permutations(n = word_count, r = x, v = a)) %>%
    # for above, permutations more comprehensive, combinations quicker and more cost effective
    map(as_tibble) %>%
    set_names(map_int(., ncol)) %>%
    map(function(x) unite(x, col = "combo", everything(), sep = " "))

  srch_words <- token_list[seq_len(min(word_count, list_n))]

  filtered_data <- pmap(
    list(word_combo, srch_words, names(srch_words)),
    function(x,y,xx) map_df(x$combo, function(z) filter(y, word == z)) %>%
      mutate(no.words = xx) %>%
      select({{id}}, word, no.words, matches("f"))) %>%
    bind_rows %>%
    arrange(desc(no.words), desc(tf_idf), desc(idf), desc(tf)) %>%
    filter(!duplicated({{id}})) %>%
    slice_head(n = n_top)

  # output
  left_join(filtered_data, data, by = {{id}}) %>%
    select(all_of(names(data)))

}
