#' Scale transformation for numeric matrix as rows.
#'
#' This is the function to complete scale transformation fot matrix or data.frame.
#'
#' it supports auto-scale or min
#'
#'
#' @param raw.data A numeric matrix or data.frame.
#' @param method Scale method: auto, auto scale; "mintmax", min-max scale.
#' @return scaling transformation of \code{raw.data}
#' @examples
#' test.matrix <- matrix(1:100, ncol=10)
#' ZZWscale(test.matrix, method = "auto")
#' @export

ZZWscale <- function(raw.data, method = "auto"){
  if (method=="auto") {
    temp.data <- lapply(c(1:nrow(raw.data)), function(i){
      temp <- as.numeric(raw.data[i,])
      temp.scale.data <- (temp-mean(temp))/sd(temp)
    })
    # temp.result <- as.data.frame(do.call(rbind, temp.data))
    temp.result <- do.call(rbind, temp.data)
    row.names(temp.result) <- row.names(raw.data)
    colnames(temp.result) <- colnames(raw.data)
  }

  if (method=="mintmax") {
    temp.data <- lapply(c(1:nrow(raw.data)), function(i){
      temp <- as.numeric(raw.data[i,])
      temp.scale.data <- (temp-min(temp))/(max(temp)-min(temp))
    })
    # temp.result <- as.data.frame(do.call(rbind, temp.data))
    temp.result <- do.call(rbind, temp.data)
    row.names(temp.result) <- row.names(raw.data)
    colnames(temp.result) <- colnames(raw.data)
  }
  return(temp.result)
}
