#' uth_make_outcome_rate
#'
#' @description Use this function to aggregate counts and totals to then make rates in a data frame. A typical use case would involve a data frame with student ids, a Boolean variable to make a rate such as is_returned_next_fall or Is_graduated, as well as a collection of grouping attributes.
#'
#' @family rate functions
#'
#' @param .data A data frame with columns you would like to aggregate on.
#' @param rate_var A Boolean variable to make a rate such as is_returned_next_fall or Is_graduated. This must be the 2nd argument used in the function or must be hard coded as rate_var = .
#' @param ... The columns you would like to aggregate over. Don't forget the column you are counting, i.e. student number. all the grouping variables for a given metric. this can be a variables in "quotes" separated by commas or be it can be variables defined in a named list.
#'
#' Example uses:
#'
#''uth_make_outcome_rate(.data = grad_data_raw, rate_var = “is_graduated”, “cohort_start_term_id”, “gender_code”)
#'
#'grouping_list <- c(“cohort_start_term_id”, “gender_code”, “cohort_code_desc”)
#'
#'uth_make_outcome_rate(grad_data_raw, “is_graduated”, grouping_list)

#' @return A table with aggregated counts, totals, and rates of column attributes
#' @export
#' @importFrom dplyr select mutate group_by summarise relocate
#' @importFrom tidyr pivot_wider replace_na
#' @importFrom magrittr %>%
#' @importFrom scales percent
#' @importFrom rlang sym
#' @examples utHelpR::uth_make_outcome_rate(fake_enrollment, "is_graduated", "gender_code", "primary_degree_id")
#' @examples utHelpR::uth_make_outcome_rate(fake_enrollment, "is_returned_next_fall", "student_type_code")

uth_make_outcome_rate <- function(.data, rate_var, ...){

  output_df <- .data %>%
    dplyr::select(!! rlang::sym(rate_var), ...) %>%
    dplyr::mutate(rate_var = !! rlang::sym(rate_var),
                  positive_outcome = dplyr::if_else(is.na(rate_var) |
                  rate_var == FALSE, "outcome_missed", "outcome_achieved")) %>%
    dplyr::select(-c(rate_var, !! rlang::sym(rate_var))) %>%
    utHelpR::uth_make_outcome_count(., dplyr::everything()) %>%
    tidyr::pivot_wider(names_from = positive_outcome, values_from = count) %>%
    dplyr::ungroup() %>%
    dplyr::mutate_if(is.numeric, ~tidyr::replace_na(., 0)) %>%
    dplyr::mutate(outcome_total = outcome_achieved + outcome_missed,
      outcome_rate_num = round(100*(outcome_achieved/outcome_total), digits = 1),
      outcome_rate = scales::percent(outcome_rate_num, scale = 1, accuracy = .1),
      rate_var = {{rate_var}}) %>%
    dplyr::relocate(rate_var, .before = outcome_achieved)

  return(output_df)
}
