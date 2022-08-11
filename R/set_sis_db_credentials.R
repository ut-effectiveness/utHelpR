#' Set SIS DB Password
#'
#' `set_sis_db_password` will prompt you with a dialog box to input your new password
#' for SIS DB.
#'
#' @details **To reset your SIS DB password.**
#'
#' @importFrom keyring key_set
#'
#' @export

set_sis_db_password <- function() {
  # for storing an individual's password
  key_set(service="sis_db", username="password")
}

#' Set SIS DB User Name
#'
#' `set_sis_db_username` will prompt you with a dialog box to input your user name
#' for SIS DB.
#'
#' @details **To set your SIS DB user name. **
#'
#' @importFrom keyring key_set
#'
#' @export

set_sis_db_username <- function() {
  # for storing an individual's username
  key_set(service="sis_db", username="username")
}
