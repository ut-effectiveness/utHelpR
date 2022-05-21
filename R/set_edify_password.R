#' Reset Edify Password
#'
#' @return
#' @export
#'
#' @examples
set_edify_password <- function() {
  keyring::key_set("edify", "password")
}
