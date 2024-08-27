#' get_data_from_pin_prod
#' A helper function for get_data_from_pins()
#'
#' @param pin_name a pin already hosted on one of the Posit Servers
#'
#' @return data stored as a pin
#' @importFrom pins pin_read
#'
get_data_from_pin_prod <- function(pin_name){

  server_choice <- "https://connect.ie.utahtech.edu"

  api_key <- Sys.getenv("RSCONNECT_SERVICE_USER_API_KEY")
  rsconnect <- pins::board_connect(key =api_key, server = server_choice)
  output_df <- pins::pin_read(pin_name, board = rsconnect)

  return(output_df)
}

#' get_data_from_pin_test
#' A helper function for get_data_from_pins()
#'
#' @param pin_name a pin already hosted on one of the Posit Servers
#'
#' @return data stored as a pin
#'
get_data_from_pin_test <- function(pin_name){

  server_choice <- "https://connect.test.ie.utahtech.edu"

  api_key <- Sys.getenv("RSCONNECT_SERVICE_USER_API_KEY")
  rsconnect <- pins::board_connect(key =api_key, server = server_choice)
  output_df <- pins::pin_read(pin_name, board = rsconnect)

  return(output_df)
}

#' get_data_from_pin_local
#' A helper function for get_data_from_pins()
#'
#' @param pin_name a pin already hosted on one of the Posit Servers
#' @param prod defaulted to true, is for the user to pick which server to pull pins from locally. If false it will pull pins from the test server
#'
#' @return data stored as a pin
#' @importFrom pins pin_read
#' @importFrom keyring key_get
#'
get_data_from_pin_local <- function(pin_name, prod=TRUE) {

  if(prod == TRUE){
    server_choice <- "https://connect.ie.utahtech.edu"
    api_key <- keyring::key_get("pins", "api_key")

  } else {
    server_choice <- "https://connect.test.ie.utahtech.edu"
    api_key <- keyring::key_get("pins_test", "api_key")
  }

  rsconnect <- pins::board_connect(key=api_key, server=server_choice)
  df <- pins::pin_read(pin_name, board=rsconnect) %>%
    mung_dataframe()
  return(df)
}


#' get_data_from_pins()
#' This R function retrieves pins data from a Posit server. It uses the pins package to connect to the server, authenticate, and fetch the required data. This function ensures secure and efficient data retrieval from a Posit server, making it easy to integrate pins data into your R workflows.
#'
#' @param pin_name The name of the pin to retrieve hosted on one of Utah Tech's Posit Servers
#' @param prod defaulted to true to pull pins from the prod Posit Server, if false it will pull pins from the test server. This parameter is only used when retrieving pins locally.
#'
#' @return The name of the pin to retrieve
#' @export
#'
uth_get_data_from_pins <- function(pin_name, prod=TRUE){

  output_df <- tryCatch(get_data_from_pin_prod(pin_name),
                        error=function(e) get_data_from_pin_test(pin_name),
                        error=function(e) get_data_from_pin_local(pin_name, prod)
  )

  return(output_df)
}
