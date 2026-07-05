#' The capacity (or fuzzy measure) associated with an Ordered Weighted Average (OWA)
#'
#' @param weights_w A numerical vector of positive numbers. It gives the weights to use in an Ordered Weighted Average (OWA)
#'
#' @return A numeric vector giving the capacity corresponding to each subset from the set formed from the numbers 1,2,..., n
#' @export
#'
#' @examples
#' capacity_OWA(c(0.2, 0.3, 0.3, 0.2))
capacity_OWA <- function(weights_w){

  if (any(!is.numeric(weights_w), weights_w < 0, is.na(weights_w)))
    stop("All the weights must be positive numbers")


  n <- length(weights_w)

  if (sum(weights_w )!=1) {
    weights_w <- weights_w/sum(weights_w)
  }

  all_subsets <- power_set(n)
  cardinality <- all_subsets$size

  mu <- numeric(2^n)

  for(i in 2:(2^n)){
      mu[i] <- sum(weights_w[1:cardinality[i]] )
  }

  names(mu) <- all_subsets$subsets

  return(mu)
}
