#' Set Edify password
#'
#' `set_edify_password` will prompt you with a dialog box to input your new password
#' from edify.
#'
#' @details **To reset your Edify password**
#'
#' @details
#'
#' - Navigate to \href{https://dixie.edh.eab.com/}{Edify}.
#' - Select the SSO login.
#' - Login with your UT single sign-on credentials.
#' - Click the question mark in the upper left corner.
#' - Select User Profile
#' - Select Generate new credential.
#'
#' @export
#'
#' @importFrom keyring key_set

set_edify_password <- function() {
  keyring::key_set("edify", "password")
}
