#' Munge dataframe
#'
#' @param df
#'
#' @return a clean dataframe
#' @export
#'
#' @examples
#' munge_dataframe(bob)

mung_dataframe <- function(df) {
  df <- df %>%
    mutate_if(is.factor, as.character) %>%
    clean_names() %>%
    as_tibble()
  return(df)
}
