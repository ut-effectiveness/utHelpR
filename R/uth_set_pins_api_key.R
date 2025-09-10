#' This function sets the API key for the pins package using the `keyring` package. It allows users to choose between a production or test environment and provides an option to overwrite an existing key. see the UT Connect Posit Servers for your api keys `https://connect.ie.utahtech.edu/ `for Prod or `https://connect.test.ie.utahtech.edu/` for Test.
#'
#' @param prod Logical, if TRUE sets the key for the Posit Connect production environment, if FALSE sets it for the test environment.
#' @param overwrite Logical, if TRUE allows overwriting an existing key.
#'
#' @returns NULL, but sets the API key in the keyring.
#' @export
#' @importFrom keyring key_set key_get

uth_set_pins_api_key <- function(prod = TRUE, overwrite = FALSE) {

  service_choice <- if (prod) "pins" else "pins_test"
  message("Using service: ", service_choice)

  # Check if key already exists
  key_exists <- tryCatch({
    keyring::key_get(service = service_choice, username = "api_key")
    TRUE
  }, error = function(e) {
    FALSE
  })

  if (key_exists && !overwrite) {
    message("API key already exists for '", service_choice, "'. Use `overwrite = TRUE` to reset it.")
  } else {
    tryCatch({
      keyring::key_set(service = service_choice, username = "api_key",
                       prompt = "Please enter your API key:")
      message("API key successfully set.")
    }, error = function(e) {
      message("Failed to set API key: ", e$message)
    })
  }
}
