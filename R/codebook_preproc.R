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
  if(sum(codebook_data_match_idx) < length(codebook_data_match_idx)){
    msg <- paste("Warning:", sum(!codebook_data_match_idx), "item(s) found in names_from not found in df columns. \n",
                 paste(names_from[!codebook_data_match_idx], collapse=" "), "\n")
    warning(msg)
  }
  data_codebook_match_idx <- names(df) %in% names_from
  if(sum(data_codebook_match_idx) < length(data_codebook_match_idx)){
    msg <- paste("Warning:", sum(!data_codebook_match_idx), "columns(s) found in df not found in names_from. \n",
                 paste(names(df)[!data_codebook_match_idx], collapse=" "))
    warning(msg)
  }

  complete_codebook_lines_idx <- !is.na(names_from) & !is.na(names_to)
  names_from <- names_from[codebook_data_match_idx & complete_codebook_lines_idx]
  names_to <- names_to[codebook_data_match_idx & complete_codebook_lines_idx]
  df %>%
    dplyr::rename_at(dplyr::vars(dplyr::all_of(names_from)), ~names_to)
}


#' Codebook Recoder
#'
#' Recode the values of multiple columns .
#'
#' @param df Dataframe of data to be recoded.
#' @param var_regex Vector of regex patterns describing columns to recode values.
#' @param values_from Vector of possible values to be recoded.
#' @param values_to Vector of values to recode values_from into.
#' @param mutate_fun Function to to apply to column after recoding. Set to NULL if no function is desired.
#'
#' @return A dataframe with recoded values.
#' @importFrom rlang .data
#'
#' @export
#'
#' @examples
codebook_recoder <- function(df, var_regex, values_from, values_to, mutate_fun=as.numeric){
  if(length(var_regex) != length(values_from)){stop(paste("var_regex and values_from lengths must match"))}
  if(length(var_regex) != length(values_to)){stop(paste("var_regex and values_to lengths must match"))}
  if(length(values_from) != length(values_to)){stop(paste("values_from and values_to lengths must match"))}

  preproc <- function(x){stringr::str_replace_all(x, "\\\r|\\\n", "") %>% tolower}

  var_regex <- unique(var_regex)
  values_from <- preproc(values_from)

  for(current_regex in var_regex){
    # current_vars <- str_extract(names(df), paste0(".*", current_regex,".*"))
    level_key <- tibble::tibble(v_from = values_from[var_regex == current_regex],
                                v_to = values_to[var_regex == current_regex]) %>%
      tibble::deframe()

    df <- df %>%
      dplyr::mutate_at(dplyr::vars(tidyselect::matches(current_regex)), preproc) %>%
      dplyr::mutate_at(dplyr::vars(tidyselect::matches(current_regex)), ~dplyr::recode(., !!!level_key))
    if(!is.null(mutate_fun)){df <- df %>% dplyr::mutate_at(dplyr::vars(tidyselect::matches(current_regex)), mutate_fun)}
  }
  return(df)
}
