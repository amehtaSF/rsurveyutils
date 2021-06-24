
#' Scale
#'
#' Wrapper for base scale function. Returns vector instead of list.
#'
#' @param ... Arguments passed to scale
#'
#' @return Vector of scaled values
#' @export
#'
#' @examples
#'
#' myVec <- c(1,6,3,6,8,4,5,4,2,2)
#' myScaledVec <- scale(myVec)
#'
scale <- function(...){
  as.vector(unlist(base::scale(...)))
}

#' to_sd_categories
#'
#' @param x Vector of numeric values
#'
#' @return Vector of 3 level factor by standard deviation. (Below 1 SD, within 1 SD, greater than 1 SD)
#' @export
#'
#' @examples
#' myVec <- c(1,6,3,6,8,4,19,5,4,1,7,17,,9,5,5,11,4,3,4,11,4,2,2)
#' myCategoricalVec <- to_sd_categories(myVec)
#'
to_sd_categories <- function(x, na.rm){
  case_when(
    x < (mean(x, na.rm = na.rm) - sd(x, na.rm = na.rm)) ~ "< -1 SD",
    x > (mean(x, na.rm = na.rm) + sd(x, na.rm = na.rm)) ~ "> +1 SD",
    x <= (mean(x, na.rm = na.rm) + sd(x, na.rm = na.rm)) &
      x >= (mean(x, na.rm = na.rm) - sd(x, na.rm = na.rm)) ~ "within 1 SD",
  ) %>%
    factor(levels = c("< -1 SD", "within 1 SD", "> +1 SD"))
}
