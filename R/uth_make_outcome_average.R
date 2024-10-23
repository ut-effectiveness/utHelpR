#' Aggregate Column Averages in a Data Frame
#'
#' @description
#' Calculates summary statistics (mean, standard deviation, and median) for a specified variable within a data frame, grouped by one or more additional variables. The results are rounded to a specified number of decimal places.
#'
#' @param .data A data frame containing the data to be summarized.
#' @param avg_var A string representing the name of the variable for which the summary statistics will be calculated.
#' @param ... One or more grouping variables by which the data will be grouped before calculating the summary statistics.
#' @param dp An integer specifying the number of decimal places to which the summary statistics should be rounded. The default value is 2.
#'
#' @return A data frame with the aggregated averagegs of the specified column, grouped by the additional columns.
#' @export
#'
#' @import dplyr
#' @importFrom stats median sd
#'
uth_make_outcome_average <- function(.data, avg_var, ..., dp =2){

  output_df <- .data %>%
    select(!!rlang::sym(avg_var), ...) %>%
    mutate(avg_var = !!rlang::sym(avg_var)) %>%
    group_by(across(c(...))) %>%
    summarise(
              mean = round(mean(avg_var, na.rm = TRUE), dp),
              median = round(median(avg_var, na.rm = TRUE), dp),
              standard_dev = round(stats::sd(avg_var, na.rm = TRUE), dp) # do we need this?
              ) %>%
    mutate(avg_var = {{avg_var}})

  return(output_df)

  }
