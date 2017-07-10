#' Scale transformation for numeric matrix as rows.
#'
#' This is the function to complete scale transformation fot matrix or data.frame.
#'
#' @param raw.data A numeric matrix or data.frame
#' @return scaling transformation of \code{raw.data}
#' @examples
#' test.matrix <- matrix(1:100, ncol=10)
#' ZZWscale(test.matrix, method = "auto")
#' @export
ZZWscale <- function(raw.data, method = "auto"){
  if (method=="auto") {
    temp.data <- lapply(c(1:nrow(raw.data)), function(i){
      temp.scale.data <- (raw.data[i,]-mean(raw.data[i,]))/sd(raw.data[i,])
    })
    # temp.result <- as.data.frame(do.call(rbind, temp.data))
    temp.result <- do.call(rbind, temp.data)
    row.names(temp.result) <- row.names(raw.data)
  }
  return(temp.result)
}
