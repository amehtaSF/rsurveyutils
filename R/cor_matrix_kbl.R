#' cor_matrix_kbl
#'
#' This function creates a kable correlation matrix with colored cells for
#'
#' @param column_grid dataframe: first two columns contain the names of pairwise variables found in dat
#' @param dat dataframe: data
#' @param alpha numeric: p-value cutoff at which to start coloring cells
#'
#' @return
#' @importFrom magrittr %>%
#' @export
#'
#' @examples



cor_matrix_kbl <- function(column_grid, dat, alpha=.05){
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
  k <- cor_mat %>%
    dplyr::mutate_if(is.numeric, ~round(., digits = 3)) %>%
    dplyr::rename(` `=Var1) %>%
    kableExtra::kbl() %>%
    kableExtra::kable_styling()

  for(colnum in 2:ncol(cor_mat)){
    k <- k %>%
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
  return(k)
}
