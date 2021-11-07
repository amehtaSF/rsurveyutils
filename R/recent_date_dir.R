#' recent_date_dir
#'
#' @param directory string: top-level directory in which to search for date directories
#' @param recursive boolean: indicates whether to search recurvsively
#'
#' @return string: path of most recent dated directory
#' @export
#'
#' @examples
recent_date_dir <- function(directory, recursive=FALSE){
  files <- list.files(directory,
                      pattern="[0-9]{4}-[0-9]{1,2}-[0-9]{1,2}$",
                      include.dirs=TRUE,
                      recursive=recursive)
  files <- sort(as.Date(files), decreasing = TRUE)
  date_dir_path <- paste0(directory, "/", files[1])
  return(date_dir_path)
}
