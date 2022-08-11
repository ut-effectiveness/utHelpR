#' Set Edify Password
#'
#' `set_edify_password` will prompt you with a dialog box to input your new password
#' for Edify DB connections.
#'
#' @details **To reset your Edify password.**
#'
#' @details
#'
#' - Navigate to \href{https://utahtech.edh.eab.com/}{Edify}.
#' - Select the SSO login.
#' - Login with your UT single sign-on credentials.
#' - Click the question mark in the upper-right corner.
#' - Select User Profile.
#' - Select Generate new credential.
#' - New password can be found here.
#'
#' @importFrom keyring key_set
#'
#' @export
#'

set_edify_password <- function() {
  # for storing an individual's password
  key_set(service="edify", username="password")
}

#' Set Edify User Name
#'
#' `set_edify_username` will prompt you with a dialog box to input your user name
#' for Edify DB connections.
#'
#' @details **To set your Edify user name.**
#'
#' @details
#'
#' - Navigate to \href{https://utahtech.edh.eab.com/}{Edify}.
#' - Select the SSO login.
#' - Login with your UT single sign-on credentials.
#' - Click the question mark in the upper-right corner.
#' - Select User Profile.
#' - Edify username can be found here.
#'
#' @importFrom keyring key_set
#'
#' @export
#'

set_edify_username <- function() {
  # for storing an individual's username
  key_set(service="edify", username="username")
}
