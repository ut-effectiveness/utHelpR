#' Set Banner DB Password
#'
#' `set_edify_password` will prompt you with a dialog box to input your new password
#' for Banner DB.
#'
#' @details **To reset your Banner DB password.**
#'
#' @importFrom keyring key_set
#'
#' @export

set_banner_db_password <- function() {
  # for storing an individual's password
  key_set(service="sis_db", username="password")
}

#' Set Banner DB Username
#'
#' `set_banner_db_username` will prompt you with a dialog box to input your user name
#' for Banner DB.
#'
#' @details **To set your Banner DB user name. **
#'
#' @importFrom keyring key_set
#'
#' @export

set_banner_db_username <- function() {
  # for storing an individual's username
  key_set(service="sis_db", username="username")
}
