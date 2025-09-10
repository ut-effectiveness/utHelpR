#' Get Gender Description
#'
#' This function processes a data frame to assign gender codes based on the last digit of the `student_id` and provides a descriptive gender label. It is particularly useful for datasets where gender information might be incomplete or needs to be inferred.
#'
#' @param .data A data frame containing the `student_id` and `gender_code` columns.
#' @param assign_gender A logical value indicating whether to assign gender codes based on the last digit of the `student_id`. Default is `FALSE`.
#'
#' @return A data frame with an additional `gender_desc` column that contains the gender description ("Female", "Male", or "Unspecified").
#'
#' @details If `assign_gender` is `TRUE`, the function assigns gender codes based on the last digit of the `student_id`. If the last digit is even, the gender code is assigned as "F" (Female); if odd, it is assigned as "M" (Male). If `assign_gender` is `FALSE`, the function uses the existing `gender_code` column.
#'
#' @examples
#' \dontrun{
#' # Example usage:
#' df <- data.frame(student_id = c("00123456", "00234567", "00345678"), gender_code = c("F", "M", NA))
#' get_gender_desc(df, assign_gender = TRUE)
#' }
#'
#' @import dplyr
#' @export
ut_get_gender_desc <- function(.data, assign_gender = FALSE) {
  if (assign_gender == TRUE) {
    intermediate_df <- .data %>%
      dplyr::mutate(last_digit = as.numeric(substr(student_id, nchar(student_id), nchar(student_id)))) %>%
      dplyr::mutate(gender_code_assigned = if_else(last_digit %% 2 == 0, "F", "M")) %>%
      dplyr::mutate(gender_code = dplyr::if_else(gender_code %in% c("M", "F"), gender_code, gender_code_assigned)) %>%
      dplyr::select(-c(gender_code_assigned, last_digit))
  } else {
    intermediate_df <- .data
  }

  output_df <- intermediate_df %>%
    dplyr::mutate(gender_desc = dplyr::case_when(
      gender_code == "F" ~ "Female",
      gender_code == "M" ~ "Male",
      TRUE ~ "Unspecified"
    ))

  return(output_df)
}


#' Clean Student ID
#'
#' This function processes student IDs to ensure they are in a consistent character format without leading "D" or "d". It can handle various formats of student IDs, including numeric IDs, email addresses, and IDs with leading characters.
#'
#' @param student_id A vector of student IDs, which can be numeric, email addresses, or text with a leading "D" or "d".
#'
#' @importFrom dplyr mutate if_else case_when everything
#' @importFrom stringr str_detect str_starts str_sub str_pad str_remove
#' @importFrom rlang sym
#'
#' @return A character vector of cleaned student IDs, each formatted to be 8 characters long, with leading zeros if necessary. The function removes any leading "D" or "d" and handles email addresses by truncating them to the first 8 characters.
#'
#' @details This function standardizes student IDs by:
#' - Removing leading "D" or "d" characters.
#' - Handling email addresses by extracting the first 8 characters.
#' - Converting numeric IDs to character format.
#' - Padding IDs with leading zeros to ensure they are 8 characters long.
#'
#' This function is useful for preparing student IDs for analysis or integration with other systems that require a standardized format.
#'
#' @examples
#' \dontrun{
#' # Example usage:
#' ids <- c("D1234567", "d2345678", "BO345678", "D1234567@utahtech.edu", 123456)
#' cleaned_ids <- uth_clean_student_id(ids)
#'
#' # Using the function within a dplyr pipeline
#' df <- data.frame(student_id = ids)
#' df <- df %>%
#'   dplyr::mutate(cleaned_student_id = uth_clean_student_id(student_id))
#' }
#'
#' @export
uth_clean_student_id <- function(student_id) {
  student_id <- {{student_id}}

  output_df <- tibble::tibble(
    student_id = student_id) %>%
    dplyr::mutate(student_id = ifelse(stringr::str_detect(student_id, "@"), stringr::str_sub(student_id, 1, 8), student_id)) %>%
    dplyr::mutate(student_id = dplyr::case_when(
      is.numeric(student_id) ~ as.character(student_id),
      is.character(student_id) & stringr::str_starts(student_id, "BO") ~ stringr::str_remove(student_id, "BO"),
      is.character(student_id) & stringr::str_starts(student_id, "D") ~ stringr::str_remove(student_id, "D"),
      is.character(student_id) & stringr::str_starts(student_id, "d") ~ stringr::str_remove(student_id, "d"),
      TRUE ~ as.character(student_id))) %>%
    dplyr::mutate(student_id = stringr::str_pad(student_id, 8, "left", "0")) %>%
    dplyr::select(student_id) %>%
    dplyr::pull(student_id)
}

