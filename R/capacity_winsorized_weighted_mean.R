#' The capacity (or fuzzy measure) associated with the (r,s)-fold Winsorized weighted mean
#'
#' @param n A positive integer. It represents the dimension of the vector to apply winsorized mean
#' @param r A positive integer. The number of observations to be winsorized from the lower end of x before the mean is computed
#' @param s A positive integer. The number of observations to be winsorized from the upper end of x before the mean is computed
#' @param alpha A real number between 0 and 0.5. It gives the proportion of observations to be winsorized from the lower end of x before the mean is computed
#' @param beta A real number between 0 and 0.5. It gives the proportion of observations to be winsorized from the upper end of x before the mean is computed
#' @param weights A numerical vector of weights with the same length as x. It gives the weights to use in the Winsorized weighted mean
#'
#' @return An object of class capacity.
#' @export
#'
#' @examples
#' capacity_winsorized_weighted_mean(n=6, r=2)
capacity_winsorized_weighted_mean<- function (n, r ,  s = r,
                            alpha = NULL, beta = alpha,
                            weights=NULL){

  if (is.null(weights)) weights<-rep(1, n)

  if (any(!is.numeric(weights), weights < 0, is.na(weights)))
    stop("The weights must be positive numbers")

  if (length(weights) != n)
    stop(" 'weights' must have length n")

  if (any(c(!is.null(alpha),!is.null(beta))))
    if (any(c(alpha > 0.5, beta > 0.5, alpha < 0, beta < 0)))
      stop("the winsorized fraction must be between 0 and 0.5")

  if (!is.numeric(r) || length(r) != 1L )
    stop("'r' must be numeric of length one")
  if (!is.numeric(s) || length(s) != 1L)
    stop("'s' must be numeric of length one")
  if (!is.null(alpha))  r <- floor(n * alpha) + 1
  if (!is.null(beta))   s <- floor(n * beta) + 1

  if (r<0 || s<0)
    stop("'r' and 's' must be positive numbers" )

  if (r > n/2 | s > n/2)
    stop("r and s must be less than half the length of x" )

  subsets<-sets::set_power(1:n)
  longitudes<-sapply(subsets, length)


  mu <-numeric(2^n)


for(i in 1:2^n){
  if (longitudes[i]<=s) mu[i]<-0
  if(longitudes[i]>=n-r) mu[i]<-1
  if(longitudes[i]>s & longitudes[i]<n-r)
    mu[i]<-sum(weights[unlist(unclass(subsets)[i])])/sum(weights)
}
 mu<- kappalab::capacity(mu)

  return(mu)
}

