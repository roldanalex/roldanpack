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
#' @param list_n number of dataset within list
#' @export

search_words <- function(data, token_list, query, id,
                         stop_words, list_n = 3){

  token1 <- query %>%
    tolower %>%
    str_extract_all("\\w+") %>%
    unlist() %>%
    .[!. %in% stop_words] %>%
    unique()

  # Create token variation
  query_final <- token1 %>%
    str_replace("ou?r$", "ou?r") %>%
    str_replace("i(s|z)e$", "i(s|z)e") %>%
    str_replace("i(s|z)ation$", "i(s|z)ation") %>%
    str_replace("y(s|z)e$", "y(s|z)e") %>%
    str_replace("(?<=[aeiou])ll?(?=(ing|ed|er)$)", "ll?") %>%
    str_replace_all("ae|oe", "(ae|oe|e)") %>%
    str_replace("ence$|ense$", "en(s|c)e") %>%
    str_replace("og(ue)?$", "og(ue)?") %>%
    str_replace("re$|er$", "(er|re)") %>%
    str_replace("s$", "s?")

  word_count <- length(query_final)

  word_combo <- seq_len(min(word_count, list_n)) %>%
    map(function(x) permutations(n = word_count, r = x, v = query_final)) %>%
    # for above, better and faster permutations
    map(as_tibble) %>%
    set_names(map_int(., ncol)) %>%
    map(function(x) unite(x, col = "combo", everything(), sep = " "))

  srch_words <- token_list[seq_len(min(word_count, list_n))]

  relevant_all <- pmap(
    list(word_combo, srch_words, names(srch_words)),
    function(x,y,xx) map_df(x$combo,
                            function(z) filter(y, str_detect(word, z))) %>%
      mutate(no.words = xx) %>%
      select({{id}}, word, no.words, matches("f"))) %>%
    bind_rows %>%
    arrange(desc(no.words), desc(tf_idf), desc(idf), desc(tf)) %>%
    filter(!duplicated({{id}}))

  # final output
  left_join(relevant_all, data) %>%
    select(all_of(names(data)))

}
