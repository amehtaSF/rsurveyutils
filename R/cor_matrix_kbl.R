#' cor_matrix_kbl
#'
#' This function creates a kable correlation matrix with colored cells indicating effect size and significance.
#' Cells are darker green with increasing positive effect size and darker red with increasing negative effect size.
#' Non-significant cells are not colored.
#'
#' @param dat dataframe: data
#' @param column_grid dataframe: first two columns contain the names of pairwise variables found in dat
#' @param alpha numeric: p-value cutoff at which to start coloring cells
#'
#' @return
#' @importFrom magrittr %>%
#' @export
#'
#' @examples
#' data(mtcars)
#' size_columns <- c("hp", "wt")
#' power_columns <- c("hp", "qsec")
#' col_grid <- expand.grid(size_columns, power_columns)
#' cor_matrix_kbl(mtcars, col_grid)



cor_matrix_kbl <- function(dat, column_grid=NULL, alpha=.05, get_df=FALSE, round_digits=3){
  if(is.null(column_grid)){
    column_grid <- expand.grid(names(dat), names(dat))
  }
  cor_mat <- column_grid %>%
    dplyr::mutate(cor = purrr::map2_dbl(.[[1]], .[[2]], ~{
      stats::cor.test(dat[[.x]], dat[[.y]], use="complete.obs")$estimate
    }
    )) %>%
    tidyr::pivot_wider(id_cols = 1,
                       names_from = 2,
                       values_from = cor)
  p_mat <- column_grid %>%
    dplyr::mutate(p = purrr::map2_dbl(.[[1]], .[[2]], ~{
      stats::cor.test(dat[[.x]], dat[[.y]], use="complete.obs")$p.value
    }
    )) %>%
    tidyr::pivot_wider(id_cols = 1,
                       names_from = 2,
                       values_from = p)
  colnames(cor_mat)[1] <- " "
  df <- cor_mat %>%
    dplyr::mutate_if(is.numeric, ~round(., digits = round_digits))

  if(!get_df){
    # Return the kable table
    k <- df %>%
      kableExtra::kbl() %>%
      kableExtra::kable_styling()

    for(colnum in 2:ncol(cor_mat)){
      result <- k %>%
        kableExtra::column_spec(colnum,
                                background = dplyr::case_when(
                                  p_mat[,colnum] < alpha ~ dplyr::case_when(
                                    cor_mat[,colnum] > 0  & cor_mat[,colnum] <= .2 ~ "#99e699",
                                    cor_mat[,colnum] > .2  & cor_mat[,colnum] <= .4 ~ "#70db70",
                                    cor_mat[,colnum] > .4  & cor_mat[,colnum] <= .6 ~ "#47d147",
                                    cor_mat[,colnum] > .6  & cor_mat[,colnum] <= .8 ~ "#2eb82e",
                                    cor_mat[,colnum] > .8  & cor_mat[,colnum] <= 1 ~ "#248f24",
                                    cor_mat[,colnum] < 0  & cor_mat[,colnum] <= .2 ~ "#ffb3b3",
                                    cor_mat[,colnum] < .2  & cor_mat[,colnum] <= .4 ~ "#ff8080",
                                    cor_mat[,colnum] < .4  & cor_mat[,colnum] <= .6 ~ "#ff4d4d",
                                    cor_mat[,colnum] < .6  & cor_mat[,colnum] <= .8 ~ "#ff1a1a",
                                    cor_mat[,colnum] < .8  & cor_mat[,colnum] <= 1 ~ "#e60000",
                                  ),
                                  TRUE ~ "#ffffff")
        )
    }

    return(result)
  } else {
    # Return the dataframe

    result <- df %>%
      dplyr::mutate_all(as.character)
    for(col_num in 2:ncol(df)){
      for(row_num in 1:nrow(df)){
        if(p_mat[row_num, col_num] < .001){
          result[row_num, col_num] <- paste(result[row_num, col_num], "***")
        } else if(p_mat[row_num, col_num] < .01){
          result[row_num, col_num] <- paste(result[row_num, col_num], "**")
        } else if(p_mat[row_num, col_num] < .05){
          result[row_num, col_num] <- paste(result[row_num, col_num], "*")
        }
      }
    }
    return(result)
  }

}
