#' Set Edify password
#'
#' `set_edify_password` will prompt you with a dialog box to input your new password
#' from edify.
#'
#' @details **To reset your Edify password**
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
#' @export
#'
#' @importFrom keyring key_set

set_edify_password <- function() {
  # for storing an individual's password
  key_set(service="edify", username="password")
}

#' Set Edify username
#'
#' `set_edify_username` will prompt you with a dialog box to input your username
#' from Edify.
#'
#' @details **To set your Edify username**
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
#' @export
#'
#' @importFrom keyring key_set

set_edify_username <- function() {
  # for storing an individual's username
  key_set(service="edify", username="username")
}


#' Go to Edify
#'
#' `go_to_edify` will take you to the Edify login page
#'
#' @export
#'
#' @importFrom utils browseURL

go_to_edify <- function() {
  browseURL("https://utahtech.edh.eab.com/app/#/auth/login")
}
