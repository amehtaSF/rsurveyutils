#' Tally scale
#'
#' This function aggregates responses from a set of columns into a single score which gets appended as a new column, e.g. sum or mean score
#'
#' @param df Dataframe of data
#' @param var_regex Regex string to capture column names which will be aggregated
#' @param new_var_name Optional argument defining the column name for the aggregate score. If no value is supplied, the new column will be named the value of var_regex
#' @param join_function Optional argument specifying join function. If NULL, aggregated values will be returned without original data
#' @param na.rm Optional argument passed to tally_function
#' @param tally_function Function used to tally scores, e.g. mean, sum, etc.
#'
#' @return A dataframe with tallyed scales appended or dataframe with tallyed scales alone
#' @importFrom rlang :=
#' @importFrom rlang !!
#' @importFrom rlang .data
#' @importFrom magrittr %>%
#' @export
#'
#' @examples
#'
#' library(dplyr)
#' df <- tribble(
#' ~scale_sub1_1, ~scale_subscale1_2, ~scale_subscale2_1, ~scale_subscale2_2,
#' 1,             2,                  3,                  3,
#' 2,             3,                  2,                  3,
#' 3,             2,                  3,                  3
#' )
#' df %>%
#'    tally_scale("^scale_(subscale1|subscale2)_[0-9]$", "scale_total") %>%
#'    tally_scale("^scale_subscale1_[0-9]$", "scale_subscale1_total") %>%
#'    tally_scale("^scale_subscale2_[0-9]$", "scale_subscale2_total")
#'
tally_scale <- function(df,var_regex, new_var_name=NULL, join_function=dplyr::full_join, na.rm=F, tally_function=sum){
  if(is.null(new_var_name)) new_var_name <- var_regex
  cols_to_tally <- names(df)[stringr::str_detect(names(df), var_regex)] # identify columns to aggregate
  cat(paste0("[", new_var_name, "] ", length(cols_to_tally), " columns:\n"))
  cat(paste0("\t"))
  cat(paste0(cols_to_tally))
  cat("\n\n")
  df <- df %>%
    dplyr::mutate(.data$..tmp_row_id.. = 1:nrow(df)) # add a temporary id column
  df_tally <- df %>%
    dplyr::select(.data$..tmp_row_id.., dplyr::one_of(cols_to_tally))  # select columns to aggregate
  df_tally <- tryCatch({df_tally %>% dplyr::mutate_all(as.numeric)}, # convert to numeric data type
                       warning = function(w) stop("Non-numeric values found in columns"),
                       error = function(e) e)
  df_tally <- df_tally %>%
    tidyr::pivot_longer(-.data$..tmp_row_id..) %>% # convert to long format
    dplyr::group_by(.data$..tmp_row_id..) %>% # group by row
    dplyr::summarize("{new_var_name}" := tally_function(.data$value, na.rm=na.rm)) # aggregate according to tally_function
  if(!is.null(join_function)){
    df_tally <- df %>%
      join_function(df_tally, by="..tmp_row_id..")  # join aggregated scores with original dataframe using join_function
  }
  df_tally <- df_tally %>% dplyr::select(-.data$..tmp_row_id..)
  return(df_tally)
}
