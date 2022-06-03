#' Munge dataframe
#'
#' @param df a dataframe you want to clean.
#'
#' @return a clean dataframe
#' @export
#'
#' @importFrom dplyr mutate_if
#' @importFrom janitor clean_names
#' @importFrom magrittr %>%
#' @importFrom tibble as_tibble
#'
#' @examples

mung_dataframe <- function(df) {
  output_df <- df %>%
    mutate_if(is.factor, as.character) %>%
    clean_names() %>%
    as_tibble()
  return(output_df)
}

#' Get data from a sql file
#'
#' @param file_name the name of the sql file that you want to use
#' @param dsn A DSN entry: edify, PROD, REPT, BRPT, etc...
#'
#' @return a dataframe containing the results of your sql query
#' @export
#'
#' @importFrom readr read_file
#' @importFrom DBI dbGetQuery
#'
#' @examples

get_data_from_sql_file <- function(file_name, dsn) {
  conn <- get_connection_object(dsn)
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
#' @importFrom readr read_file
#' @importFrom DBI dbGetQuery
#' @examples

get_data_from_sql_url <- function(query_url, dsn) {
  conn <- get_connection_object(dsn)
  query <- read_file(query_url)
  df <- dbGetQuery(conn, query) %>%
    mung_dataframe()
  return(df)
}

#' Get data from a rds source.
#'
#' @param file_name The rds file you want to load
#'
#' @return a dataframe containing the data from the .rds file
#' @export
#'
#' @examples

load_data_from_rds <- function(file_name) {
  df <- readRDS( here::here('data', file_name) ) %>%
    mung_dataframe()
  return(df)
}
