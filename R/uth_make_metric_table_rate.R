#' uth_make_metric_table_rate
#'
#' @description Use this function to concatenate grouping variables first then aggregate counts, totals and rates in a data frame. A typical use case would involve a data frame with student ids, a Boolean variable to make a rate such as is_returned_next_fall or Is_graduated, as well as a collection of grouping attributes. This is very similar to uth_make_ourtcome_rate but without concatenation of the grouping variables.
#'
#' @family rate functions
#'
#' @param .data A data frame with columns you would like to aggregate on.
#' @param rate_var A Boolean variable to make a rate such as is_returned_next_fall or Is_graduated. This must be the 2nd argument used in the function or must be hard coded as rate_var = .
#' @param ... The columns you would like to aggregate over. Don't forget the column you are counting, i.e. student number. all the grouping variables for a given metric. this can be a variables in "quotes" separated by commas or be it can be variables defined in a named list.
#'
#' Example uses:
#'
#'uth_make_metric_table_rate(.data = grad_data_raw, rate_var = “is_graduated”, “cohort_start_term_id”, “gender_code”)
#'
#'grouping_list <- c(“cohort_start_term_id”, “gender_code”, “cohort_code_desc”)
#'
#'uth_make_metric_table_rate(grad_data_raw, “is_graduated”, grouping_list)

#' @return A table with aggregated counts, totals, and rates of column attributes
#' @export
#' @importFrom dplyr select mutate
#' @importFrom tidyr unite
#' @importFrom magrittr %>%
#' @importFrom rlang sym
#' @examples utHelpR::uth_make_metric_table_rate(fake_enrollment, "is_graduated", "gender_code", "primary_degree_id")
#' @examples utHelpR::uth_make_metric_table_rate(fake_enrollment, "is_returned_next_fall", "gender_code", "student_type_code")

uth_make_metric_table_rate <- function(.data, rate_var, ...){

  segregated_by_df <- .data %>%
    dplyr::select(!! rlang::sym(rate_var), ...) %>%
    dplyr::mutate(rate_var = !! rlang::sym(rate_var),
      rate_var = if_else(is.na(rate_var), FALSE, rate_var),
      outcome = {{rate_var}}) %>%
    dplyr::select(-!! rlang::sym(rate_var)) %>%
    tidyr::unite(segregated_by, -rate_var, sep = "-", remove = FALSE) %>%
    dplyr::select(rate_var, segregated_by, outcome)

  output_df <- uth_make_outcome_rate(segregated_by_df, "rate_var", "segregated_by", "outcome") %>%
    dplyr::select(-rate_var)

  return(output_df)
}
