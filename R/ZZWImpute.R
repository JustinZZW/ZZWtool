#' @title ZZWImpute
#' @description impute NA using KNN algorthem
#' @author Zhiwei Zhou
#' @param raw_matrix row: feature; column: sample
#' @param label feature id
#' @export


setGeneric('ZZWImpute',
           def = function(
             raw_matrix,
             label,
             k=10,
             rowmax=0.5,
             colmax=0.8,
             maxp=1500,
             rng.seed=362436069
           ){
             n_inf <- apply(raw_matrix, 2, function(x){
               sum(is.infinite(x))
             })

             n_na <- apply(raw_matrix, 2, function(x){
               temp <- is.na(x)
               sum(temp)
             })

             n_nan <- apply(raw_matrix, 2, function(x){
               temp <- is.nan(x)
               sum(temp)
             })

             cat(paste0('The number of inf: ', sum(n_inf), '\n'),
                 paste0('The number of NA: ', sum(n_na), '\n'),
                 paste0('The number of NAN: ', sum(n_nan), '\n')
             )

             result <- impute::impute.knn(as.matrix(raw_matrix),
                                          k = k,
                                          rowmax = rowmax,
                                          colmax = colmax,
                                          maxp = maxp,
                                          rng.seed = rng.seed)

             result <- result[[1]]
             result <- data.frame(name=label,
                                  result,
                                  stringsAsFactors = F)

             return(result)
           })
