#' The capacity (or fuzzy measure) associated with a Weighted Mean (WEM)
#'
#' @param weights_p A numerical vector of positive numbers. It gives the weights to use in a weighted mean (WEM)
#'
#' @return A numeric vector giving the capacity corresponding to each subset of the set formed from the numbers 1,2,..., n
#' @export
#'
#' @examples
#' capacity_WEM(1:3)
#' capacity_WEM(c(0.25, 0.5, 0.25))
capacity_WEM<-function(weights_p){

  if (any(!is.numeric(weights_p), weights_p < 0, is.na(weights_p)))
    stop("All the weights must be positive numbers")

  n<-length(weights_p)

  if (sum(weights_p )!=1) {
            weights_p <- weights_p/sum(weights_p)
            }

  all_subsets <- power_set(n)

  mu <-numeric(2^n)

  for(i in 2:(2^n)){
   indices <- all_subsets$elements[[i]]
   mu[i]<-sum(weights_p[ indices ])
  }

  names(mu)<-  all_subsets$subsets

  return(mu)
}
