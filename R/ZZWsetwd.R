#' Set work path
#' @examples
#' ZZWsetwd()
#' @export

ZZWsetwd <- function() {
  x <- readline("Please paste the directory:")
  setwd(x)
}
