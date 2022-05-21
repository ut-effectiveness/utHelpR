#' Munge dataframe
#'
#' @param df
#'
#' @return a clean dataframe
#' @export
#'
#' @examples

mung_dataframe <- function(df) {
  df <- df %>%
    mutate_if(is.factor, as.character) %>%
    clean_names() %>%
    as_tibble()
  return(df)
}

#' Get data from a sql file
#'
#' @param file_name the name of the sql file that you want to use
#' @param dsn A DSN entry: edify, PROD, REPT, BRPT, etc...
#'
#' @return a dataframe containing the results of your sql query
#' @export
#'
#' @examples

get_data_from_sql_file <- function(file_name, dsn) {
  conn <- get_conn(dsn)
  query <- read_file( here::here('sql', file_name) )
  df <- dbGetQuery(conn, query) %>%
    mung_dataframe()
  return(df)
}

#' Get data from a sql url
#'
#' @param query_url the url of a sql query
#' @param dsn A DSN entry: edify, PROD, REPT, BRPT, etc...
#'
#' @return a dataframe containing the results of your sql query
#' @export
#'
#' @examples

get_data_from_sql_url <- function(query_url, dsn) {
  conn <- get_conn(dsn)
  query <- read_file(query_url)
  df <- dbGetQuery(conn, query) %>%
    mung_dataframe()
  return(df)
}
