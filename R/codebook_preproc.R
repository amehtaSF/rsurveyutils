#' Codebook Renamer
#'
#' Rename the columns of a dataframe.
#'
#' @param df Dataframe with columns to be renamed
#' @param names_from Vector of column names found in df
#' @param names_to Vector of column names with the same length as names_from to which columns in df will be renamed
#'
#' @return A dataframe with renamed columns
#' @export
#'
#' @examples
codebook_renamer <- function(df, names_from, names_to){
  if(length(names_from) != length(names_to)){stop(paste("names_from and names_to lengths must match"))}
  codebook_data_match_idx <- names_from %in% names(df)
  if(sum(codebook_data_match_idx) < length(names(df))){
    msg <- paste("Warning:", sum(!codebook_data_match_idx), "items found in names_from that are not valid columns in df.")
    warning(msg)
    warning(names_from[!codebook_data_match_idx])
  }
  complete_codebook_lines_idx <- !is.na(names_from) & !is.na(names_to)
  names_from <- names_from[data_codebook_match_idx & complete_codebook_lines_idx]
  names_to <- names_to[data_codebook_match_idx & complete_codebook_lines_idx]
  df %>%
    dplyr::rename_at(vars(names_from), ~names_to)
}


#' Codebook Recoder
#'
#' Recode the values of multiple columns .
#'
#' @param df Dataframe of data to be recoded.
#' @param var_regexes Vector of regex patterns describing columns to recode values.
#' @param values_from Vector of possible values to be recoded.
#' @param values_to Vector of values to recode values_from into.
#' @param mutate_fun Function to to apply to column after recoding. Set to NULL if no function is desired.
#'
#' @return A dataframe with recoded values.
#' @export
#'
#' @examples
codebook_recoder <- function(df, var_regexes, values_from, values_to, mutate_fun=as.numeric){
  if(length(var_regexes) != length(values_from)){stop(paste("var_regexes and values_from lengths must match"))}
  if(length(var_regexes) != length(values_to)){stop(paste("var_regexes and values_to lengths must match"))}
  if(length(values_from) != length(values_to)){stop(paste("values_from and values_to lengths must match"))}

  preproc <- function(x){x %>% str_replace_all(., "\\\r|\\\n", "") %>% tolower}

  var_regexes <- unique(var_regexes)
  values_from <- preproc(values_from)

  for(var in var_regexes){
    level_key <- tibble::tibble(values_from = values_from[values_from == var],
                                values_to = values_to[values_from == var]) %>%
      tibble::deframe

    df <- df %>%
      dplyr::mutate_at(vars(matches(var)), ~preproc) %>%
      if(!is.null(mutate_fun)){df <- df %>% dplyr::mutate_at(vars(matches(var)), mutate_fun)}
  }
  return(df)
}
