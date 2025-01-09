#' Utah Tech check package manager status
#'
#' The `uth_check_pkg_mgr_status` function retrieves the list of available packages, converts it into a data frame, and selects the first row. It then extracts the Repository column value from this row and prints the current package manager URL. This function is useful for checking the URL of the package repository currently being used.
#'
#' @return prints the current package manager URL and R version.
#' @export
#' @importFrom utils available.packages
#'
uth_check_pkg_mgr_status <- function() {
  # Get the current R version
  current_r_version <- R.version.string

  # Get the current package manager URL
  current_package_manager_url <- utils::available.packages() %>%
    as.data.frame() %>%
    slice(1) %>%
    pull(Repository)

  # Print the R version and package manager URL
  message("This is your current R version: ", current_r_version)
  message("This is your current Posit package manager URL: ", current_package_manager_url)

  browseURL("https://rs-connect.utahtech.edu/dev_book/")
}


#' Utah Tech save my packages information
#'
#' The `uth_save_my_pkg_info` function creates a data frame of installed packages, excluding base packages, and adds a column is_installed set to TRUE. It then gets the path to the temporary directory and defines the file path for my_packages_info.rda. The function saves the data frame to this file and informs the user that the file has been successfully saved, returning the file path for later retrieval. This function is designed to save the current package information to a temporary file for future use.
#'
#' @return the file path of the temporary directory saved data frame.
#' @export
#' @importFrom utils available.packages installed.packages
#'
uth_save_my_pkg_info <- function() {
  my_packages_info <- as.data.frame(utils::installed.packages()) %>%
    filter(is.na(Priority) | Priority != "base") %>%
    select(Package) %>%
    mutate(is_installed = TRUE)

  # Get the path to the temporary directory
  #temp_dir <- tempdir()

  # Define the file path
  #file_path <- file.path(temp_dir, "my_packages_info.rda")

  # Determine the download folder path
  download_folder <- path.expand("~/Downloads")
  file_path <- file.path(download_folder, "my_packages_info.rda")

  # Save the output to the file
  save(my_packages_info, file = file_path)

  # Inform the user that the file has been saved
  message("A file of your current packages has been saved successfully to: ", file_path)

  return(file_path)  # Return the file path for later retrieval
}


#' Utah Tech load my package information
#'
#' The `uth_load_my_pkg_info` function first gets the path to the temporary directory and defines the file path for my_packages_info.rda. It checks if this file exists; if it does, the function loads the file, displays a success message, and returns the my_packages_info data. If the file does not exist, it issues a warning message indicating that the save_my_packages_info function should be run first and returns NULL. This function is designed to load previously saved package information from a temporary file.
#'
#' @return This function complements the save_my_pkg_info function by providing a way to retrieve the saved package information.
#' @export
#'
#'
uth_load_my_pkg_info <- function() {
  # Get the path to the temporary directory
  #temp_dir <- tempdir()

  # Define the file path
  #file_path <- file.path(temp_dir, "my_packages_info.rda")

  download_folder <- path.expand("~/Downloads")
  file_path <- file.path(download_folder, "my_packages_info.rda")


  # Check if the file exists
  if (file.exists(file_path)) {
    # Load the file
    load(file_path)
    message("The file of your packages has been loaded successfully.")
    return(my_packages_info)
  } else {
    # Give a warning message
    warning("The file of your packages does not exist. Please run the 'save_my_packages_info' function first.")
    return(NULL)
  }
}


#' Utah Tech install from Posit package manager
#'
#' The uth_install_from_pkg_mgr function manages and updates R packages based on previously saved package information. It first fetches the list of available packages and loads the saved package data. It then joins this data with the available packages to identify which ones are installed. After deleting the temporary file containing the saved package information, the function updates the installed packages. If the update_and_install parameter is set to TRUE, it also re-installs the packages. This ensures that your R environment is up-to-date with the latest versions of your installed packages, providing an efficient way to manage package updates and installations.
#'
#' @param update_and_install It has a single parameter update_and_install which defaults to FALS which will only update currently installed packages. If you want to install new packages use please use TRUE.
#'
#' @return Loads the saved package information.
#' @export
#' @importFrom dplyr select mutate left_join filter
#' @importFrom utils available.packages install.packages installed.packages update.packages
#'
uth_install_from_pkg_mgr <- function(update_and_install = FALSE) {

  # Get available packages from the package manager
  available_packages <- as.data.frame(available.packages()) %>%
    dplyr::select(Package)

  # Load the saved packages info
  my_packages_info <- uth_load_my_pkg_info()


  # Join available packages with my installed packages info
  available_packages_check <<- available_packages %>%
    dplyr::left_join(my_packages_info, by = "Package") %>%
    dplyr::mutate(is_installed = !is.na(is_installed)) %>%
    dplyr::filter(is_installed)

  #temp_dir <- tempdir()

  #file_path <- file.path(temp_dir, "my_packages_info.rda")

  #file.exists(file_path)
  #file.remove(file_path)
  #message("Temporary file has been deleted.")

  #  Install packages if install is TRUE
  if (update_and_install == TRUE) {
    utils::update.packages(available_packages_check$Package)
    utils::install.packages(available_packages_check$Package)
    message("Packages have been successfully updated and installed.")
  } else {
    update.packages(available_packages_check$Package)
    message("Packages have been successfully updated. If you wanted to install new packages please re-run with the arugment 'update_and_install == TRUE'")
  }

}


#' Utah Tech download R for Mac OS
#'
#'The `uth_download_r_for_mac` function constructs a download URL for the specified version of R based on the type of Mac (either "M1" or other). It sets the base URL for CRAN, appends the appropriate architecture specification (-arm64 for M1 or -x86_64 for others), and forms the complete download link and file name. The function then determines the download folder path, constructs the destination file path, and attempts to download the file using download.file, handling any errors with tryCatch. If successful, it informs the user of the download location; otherwise, it provides an error message. This function simplifies downloading the correct version of R for different Mac architectures.
#'
#' @param suggested_version user input of a desired version of R example "R.4.4.1" as a string
#' @param mac_type  a string signifying user's Mac processor type, currently supports "M1" or "Intel"
#'
#' @return a download R file from CRAN in the user's download folder.
#' @export
#' @importFrom utils download.file
#'
uth_download_r_for_mac <- function(suggested_version, mac_type) {
  # Base download link for CRAN
  base_url <- "https://cran.r-project.org/bin/macosx/big-sur"

  # Determine the appropriate download link based on the Mac type
  if (mac_type == "M1") {
    mac_spec <- "-arm64"
  } else {
    mac_spec <- "-x86_64"
  }

  download_link <- paste0(base_url, mac_spec, "/base/")
  file_name <- paste0("R-", suggested_version, mac_spec,".pkg")

  # Full download URL
  full_url <- paste0(download_link, file_name)

  # Determine the download folder path
  download_folder <- path.expand("~/Downloads")
  destfile <- file.path(download_folder, file_name)

  # Download the file with error handling
  tryCatch({
    utils::download.file(full_url, destfile = destfile, mode = "wb")
    message("Downloaded ", file_name, " to ", destfile, " from ", full_url)
  }, error = function(e) {
    message("Failed to download ", file_name, ": ", e$message)
  })
}
