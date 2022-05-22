#' Make a directory structure for a standard project
#'
#' `make_standard_folders()` will create our standard directory structure in the
#' file you are in when you run it. You should run this function when you are
#' setting up a project. When you run `make_standard_folders()` you should be in
#' the same directory as your .Rproj file.
#'
#' @importFrom purrr map
#' @importFrom fs dir_ls

make_standard_folders <- function() {
  # Test to see if there is a .Rproj file in the present working directory.
  if( length(fs::dir_ls(".", glob = '*.Rproj')) != 1 ) {
    stop('There is no .Rproj file in the current directory. You should create a project in Rstudio before you create your directory structure. If you have already created a project, run getwd() in the console and check that your .Rproj file is in that directory. If you have further questions Slack Matt, Justin, or LaVoy.')
  }

  folders <- c('data', 'sensitive', 'markdown', 'R', 'sql')

  pitch <- map(folders, fs::dir_create)
}
