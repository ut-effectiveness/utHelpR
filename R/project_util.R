#' Make a Standard Directory Structure for a Project
#'
#' `make_standard_folders()` creates a standard directory structure in the current working directory. This function should be run when setting up a new project. Ensure you are in the same directory as your `.Rproj` file when running this function.
#'
#' @details This function performs the following tasks:
#' - Checks for the presence of an `.Rproj` file in the current directory.
#' - Creates standard folders: `data`, `sensitive`, `markdown`, `R`, and `sql`.
#' - Initializes a Git repository if one does not exist.
#' - Adds a test file in the `sensitive` folder.
#' - Updates the `.gitignore` file to exclude the `sensitive` folder, `.html` files, and `.DS_Store` from version control, ensuring no duplicates.
#'
#' @export
#'
#' @importFrom purrr walk
#' @importFrom fs dir_ls file_create dir_create
#' @importFrom usethis use_git use_blank_slate
#' @importFrom here here
#'
#' @examples
#' \dontrun{
#' # Example usage:
#' make_standard_folders()
#' }
make_standard_folders <- function() {
  usethis::use_blank_slate(scope = c("user", "project"))

  # Test to see if there is a .Rproj file in the present working directory.
  if (length(fs::dir_ls(".", glob = '*.Rproj')) != 1) {
    stop('There is no .Rproj file in the current directory. You should create a project in RStudio before you create your directory structure. If you have already created a project, run getwd() in the console and check that your .Rproj file is in that directory. If you have further questions, contact Matt or Craig.')
  }

  # gitignore
  gitignore_entries <- c(".html", "sensitive/*", "!sensitive/.gitkeep")

  if (file.exists(here::here(".gitignore"))) {
    existing_gitignore <- readLines(here::here(".gitignore"))
    new_entries <- setdiff(gitignore_entries, existing_gitignore)
    if (length(new_entries) > 0) {
      cat(new_entries, file = here::here(".gitignore"), append = TRUE, sep = "\n")
    }
  } else {
    usethis::use_git()
    cat(gitignore_entries, file = here::here(".gitignore"), append = TRUE, sep = "\n")
  }

  folders <- c('data', 'sensitive', 'markdown', 'R', 'sql')

  purrr::walk(folders, fs::dir_create)

  file_name <- ".gitkeep"

  purrr::walk(folders, ~{
    file_path <- file.path(.x, file_name)
    fs::file_create(file_path)
  })

  fs::file_create("sensitive/test")
  cat("this is a test", file = here::here("sensitive/test"), append = TRUE)
}



