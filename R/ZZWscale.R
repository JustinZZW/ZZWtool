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


#' @title ZZWzcore
#' @author Zhiwei Zhou
#' @param raw_data a matrix or data.frame in numeric
#' @param type "by_column" or "by_row". Default: "by_row"
#' @export

setGeneric(name = 'ZZWzscore',
           def = function(raw_data,
                          type=c('by_row', 'by_column')){

             type <- match.arg(type)
             method <- match.arg(method)

             if (type=='by_column') {
               temp_mean <- apply(raw_data, 2, mean)
               temp_sd <- apply(raw_data, 2, sd)
               standardization_table <- data.frame(mean=temp_mean,
                                                   sd=temp_sd,
                                                   stringsAsFactors = F)

               stanardization_result <- apply(raw_data, 2, function(x){
                 temp <- as.numeric(x)
                 temp_mean <- mean(x)
                 temp_sd <- sd(x)
                 temp_result <- (x-temp_mean)/temp_sd
                 temp_result
               })

               result <- list(stanardization_result=stanardization_result,
                              standardization_table=standardization_table)

               return(result)

             }

             if (type=='by_row') {
               temp_mean <- apply(raw_data, 1, mean)
               temp_sd <- apply(raw_data, 1, sd)
               standardization_table <- data.frame(mean=temp_mean,
                                                   sd=temp_sd,
                                                   stringsAsFactors = F)

               stanardization_result <- apply(raw_data, 1, function(x){
                 temp <- as.numeric(x)
                 temp_mean <- mean(x)
                 temp_sd <- sd(x)
                 temp_result <- (x-temp_mean)/temp_sd
                 temp_result
               })

               result <- list(stanardization_result=stanardization_result,
                              standardization_table=standardization_table)

               return(result)

             }

           }
)
