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
#' @export
#'
#' @examples
#'
#' df <- read.csv("datafile.csv")
#' df %>%
#'    tally_scale("^tas_(dif|ddf|eot)_[0-9]{1,2}$", "tas") %>%
#'    tally_scale("^tas_dif_[0-9]{1,2}$", "tas_dif") %>%
#'    tally_scale("^tas_ddf_[0-9]{1,2}$", "tas_ddf") %>%
#'    tally_scale("^tas_eot_[0-9]{1,2}$", "tas_eot")
#'
tally_scale <- function(df,
                        var_regex,
                        new_var_name=NULL,
                        join_function=dplyr::full_join,
                        na.rm=F,
                        tally_function=sum){
  if(is.null(new_var_name)) new_var_name <- var_regex
  cols_to_tally <- names(df)[str_detect(names(df), var_regex)] # identify columns to aggregate
  print(paste0("[", new_var_name, "] tallying ", length(cols_to_tally), " columns:"))
  print(cols_to_tally)
  cat("\n")
  df <- df %>%
    dplyr::select(one_of(cols_to_tally))  # select columns to aggregate
  if(any(purrr::map_dfc(df, ~grepl("[^0-9.]", .)))){stop("Non-numeric values found in df")}
  df <- df %>%
    dplyr::mutate_all(as.numeric) %>% # convert to numeric data type
    dplyr::mutate(..temp_id_col.. = 1:n()) %>% # add a temporary id column
    tidyr::pivot_longer(-..temp_id_col..) %>% # convert to long format
    dplyr::group_by(..temp_id_col..) %>% # group by participant
    dplyr::summarize("{new_var_name}" := tally_function(value, na.rm=na.rm)) # aggregate according to tally_function
  if(!is.null(join_function)){
    df_tally <- df_id %>%
      join_function(df_tally, by="..temp_id_col..")  # join aggregated scores with original dataframe using join_function
  }
  df <- df %>% dplyr::select(-..temp_id_col..)
  return(df)
}
