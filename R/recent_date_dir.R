#' recent_date_dir
#'
#' @param directory string: top-level directory in which to search for date directories
#' @param recursive boolean: indicates whether to search recurvsively
#'
#' @return string: path of most recent dated directory
#' @export
#'
#' @examples
#'
#'
#' myScaledVec <- recent_date_dir("data/raw")
#'
recent_date_dir <- function(directory, recursive=FALSE){
  if(!dir.exists(directory)){
    warning(paste("Input directory does not exist:", directory))
    return(FALSE)
  }
  files <- list.files(directory,
                      pattern="[0-9]{4}-[0-9]{1,2}-[0-9]{1,2}$",
                      include.dirs=TRUE,
                      recursive=recursive)
  if(length(files) == 0) {
    warning("No date files found")
    return(FALSE)
    }
  files <- sort(as.Date(files), decreasing = TRUE)
  date_dir_path <- file.path(directory, files[1])
  return(date_dir_path)
}
