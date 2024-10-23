#` Summarize Outcome Variable by Group
#'
#' This function takes a data frame, a variable to be summarized, and grouping variables,
#' and returns a data frame with the total sum of the specified variable, rounded to a specified number of decimal places.
#'
#' @param .data A data frame containing the data.
#' @param sum_var A string representing the name of the variable to be summarized.
#' @param ... Additional grouping variables.
#' @param dp An integer specifying the number of decimal places to round the total sum to. Default is 2.
#'
#' @return A data frame with the total sum of the specified variable for each group.
#' @export
#'
#' @import dplyr
uth_make_outcome_sum <- function(.data, sum_var, ..., dp =2){

  output_df <- .data %>%
    dplyr::select(!!rlang::sym(sum_var), ...) %>%
    dplyr::mutate(sum_var = !!rlang::sym(sum_var)) %>%
    dplyr::group_by(dplyr::across(c(...))) %>%
    dplyr::summarise(total = round(sum(sum_var), dp)) %>%
    dplyr::mutate(sum_var = {{sum_var}})

  return(output_df)
}
