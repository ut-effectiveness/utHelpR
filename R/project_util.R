#' Make a directory structure for a standard project
#'
#' `make_standard_folders()` will create our standard directory structure in the
#' file you are in when you run it. You should run this function when you are
#' setting up a project. When you run `make_standard_folders()` you should be in
#' the same directory as your .Rproj file.
#'
#' @export
#'
#' @importFrom purrr map
#' @importFrom fs dir_ls file_create
#' @importFrom usethis use_git use_blank_slate
#' @importFrom here here

make_standard_folders <- function() {
  usethis::use_blank_slate(scope = c("user", "project"))
  # Test to see if there is a .Rproj file in the present working directory.
  if( length(fs::dir_ls(".", glob = '*.Rproj')) != 1 ) {
    stop('There is no .Rproj file in the current directory. You should create a project in Rstudio before you create your directory structure. If you have already created a project, run getwd() in the console and check that your .Rproj file is in that directory. If you have further questions Slack Matt, or Craig.')
  }

  folders <- c('data', 'sensitive', 'markdown', 'R', 'sql')

  pitch <- purrr::map(folders, fs::dir_create)

  if (file.exists(here::here(".gitignore")))
  {
    fs::file_create("sensitive/test")
    cat("this is a test", file = here::here("sensitive/test"), append = TRUE)
    cat("sensitive/", file = here::here(".gitignore"), append = TRUE, sep = "\n")
  }
  else {
    usethis::use_git()
    fs::file_create("sensitive/test")
    cat("Make sure that this file is not showing up in your git tab. ",
      file = here::here("sensitive/test"), append = TRUE)
    cat("sensitive/", file = here::here(".gitignore"), append = TRUE, sep = "\n")
  }
}


