
# TODO: Check for duplicate values in codebooks

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
#' @param FUN Function to to apply to column after recoding. Set to NULL if no function is desired.
#'
#' @return A dataframe with recoded values.
#' @importFrom rlang .data
#'
#' @export
#'
#' @examples
codebook_recoder <- function(df, var_regex, values_from, values_to, FUN=as.numeric){
  clean_string <- function(x){stringr::str_replace_all(x, "\\\r|\\\n", "") %>% stringr::str_trim() %>% tolower}
  key <-  tryCatch({
    tibble::tibble(
      var_rgx = var_regex,
      val_from = clean_string(values_from),
      val_to =  as.character(values_to)
    )
  },
  warning=function(w){w},
  error=function(e){e})

  if(any(is.na(key$var_rgx) | is.na(key$val_from) | is.na(key$val_to))){
    warning("Removing incomplete items")
  }
  col_names <- names(df)

  for(current_regex in unique(key$var_rgx)){
    level_key <- key %>%
      dplyr::filter(.data$var_rgx == current_regex) %>%
      dplyr::select(.data$val_from, .data$val_to) %>%
      tibble::deframe()
    current_col_idx <- stringr::str_detect(col_names, current_regex)
    current_col_names <- col_names[current_col_idx]
    df <- df %>%
      dplyr::mutate_at(dplyr::vars(dplyr::all_of(current_col_names)), clean_string)
    df <- df %>%
      dplyr::mutate_at(dplyr::vars(dplyr::all_of(current_col_names)), ~dplyr::recode(., !!!level_key))
    if(!is.null(FUN)){
      safe_func <- function(x, ...) {
        tryCatch(expr=FUN(x, ...),
                 warning=function(w) {warning(w)},
                 error=function(e) e)
      }
      df <- df %>%
        dplyr::mutate_at(dplyr::vars(dplyr::any_of(current_col_names)), safe_func)
    }
  }
  return(df)
}
