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
#'
load_data_from_rds <- function(file_name) {
  df <- readRDS( here::here('data', file_name) ) %>%
    mung_dataframe()
  return(df)
}

#' Get data from a pin stored in RStudio Connect
#'
#' @param pin_name The name of the pin, as stored on RStudio Connect instance.
#'
#' @return a dataframe containing all data from requested pin
#' @export
#'
#' @examples
#' get_data_from_pin(pin_name="student_type_audit_student_type_determination_variables_pin")
#'
get_data_from_pin <- function(pin_name) {
  # Obtain the API key from environment variable.
  api_key <- Sys.getenv("RSCONNECT_SERVICE_USER_API_KEY")
  # If API key is not available as environment variable, use keyring entry.
  # NOTE: The API key should only be an environment variable on the server
  #       For local machines, set a keyring entry.
  if (api_key == "") {
    api_key <- keyring::key_get("pins", "api_key")
  }
  # Register the connection to the pinning board.
  board_register_rsconnect(key=api_key, server="https://data.dixie.edu")
  # pull data from the pin
  df <- pin_get(pin_name, board="rsconnect") %>%
    mung_dataframe()
  return(df)
}

#' Load data from an Excel spreadsheet formatted file
#'
#' @param file_name the name of the file, assumed to be located in a directory called 'data'
#'
#' @return a dataframe with all content of the Excel spreadsheet
#' @export
#'
#' @examples
#' load_data_from_xlsx("additional_information.xlsx")
load_data_from_xlsx <- function(file_name) {
  df <- readxl::read_excel( here::here('data', file_name) )
  return(df)
}
