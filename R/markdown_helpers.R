#' Display sql code with comments removed
#'
#' `display_sql_no_comments()` will display a sql file with the comments removed.
#' Any comments indicated by the double dash -- will be removed. Multiline comments
#' will not be removed.
#'
#' @param file The name of the sql file you want to display.
#'
#' @export
#'
#' @importFrom readr read_lines
#' @importFrom stringr str_detect

display_sql_no_comments <- function(file) {

  x <- read_lines(here::here('sql', file))

  comments_removed <- x[!str_detect(x, '--')]

  paste0(comments_removed, collapse = '\n')
}

#' Display sql with comments
#'
#' @param file The name of the sql file you want to display.
#'
#' @export
#'
#'@importFrom readr read_file

diplay_sql_with_comments <- function(file) {

  x <- read_file(here::here('sql', 'clo_pull.sql'))

  return(x)
}
