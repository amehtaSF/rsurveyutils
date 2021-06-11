
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
