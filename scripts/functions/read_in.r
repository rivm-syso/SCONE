#' Read in csv file: use public repository if local repository is not available
#'
#' @param file filename
#' @param local local repository
#' @param public public repository
#'
#' @return
#' @export
#'
#' @examples
read_in <- function(file, local, public) {
  if(file.exists(paste0(local, file))) {
    read_csv(paste0(local, file))
  } else {
    read_csv(paste0(public, file))
  }
}

