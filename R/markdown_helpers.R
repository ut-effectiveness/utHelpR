#' Display sql code with comments removed
#'
#' `display_sql_no_comments()` will display a sql file with the comments removed.
#' Any comments indicated by the double dash -- will be removed. Multiline comments
#' will not be removed.
#'
#' @param file The name of the sql file you want to display.
#'
#' @return
#' @export
#'

display_sql_no_comments <- function(file) {
  x <- read_lines(here::here('sql', file))

  comments_removed <- x[!str_detect(x, '--')]

  paste0(comments_removed, collapse = '\n')
}
