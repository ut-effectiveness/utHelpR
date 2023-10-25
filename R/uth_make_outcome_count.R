#' Make a table of column aggregation counts
#'
#' @description
#'
#' Use this function to aggregate counts in a data frame. A typical use case
#' would involve a data frame with student ids and a collection of attributes. The utHelpR
#' package includes a data frame called fake_enrollment with this structure. The following
#' code illustrates how to use uth_make_outcome_count
#'
#' test <- fake_enrollment %>%
#'  select(student_id, gender_code) %>%
#'  filter(!is.na(gender_code))
#'
#'  uth_make_outcome_count(test, gender_code)
#'
#' @param .data A data frame with columns you would like to aggregate on.
#' @param ... The columns you would like to aggregate over. Don't forget the
#' column you are counting, i.e. student number.
#'
#' @return A table with aggregated counts of column attributes
#' @export
#'
#' @import dplyr
uth_make_outcome_count <- function(.data, ...){

  output_df <- .data %>%
    select(...) %>%
    group_by(across(c(...))) %>%
    summarize(count = n()) %>%
    ungroup()

  return(output_df)
}
