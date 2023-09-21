#' uth_make_outcome_count
#'
#' @param .data A data frame with columns you would like to aggregate on
#' @param ... the columns you would lke to aggregate over
#'
#' @return
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
