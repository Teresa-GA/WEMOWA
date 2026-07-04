#' A favor indice to measure the behaviour of of the winsorized weighted means
#'
#' @param weights A numerical vector of weights
#' @param r An integer. The number of observations to be winsorized from the lower end of x before the mean is computed
#' @param s An integer. The number of observations to be winsorized from the upper end of x before the mean is computed
#' @param alpha The fraction (0 to 0.5) of observations to be winsorized from the lower end of x before the mean is computed
#' @param beta The fraction (0 to 0.5) of observations to be winsorized from the upper end of x before the mean is computed
#'
#' @return A numerical vector of the same length of weights. A favor indice measures the degree with which the behaviour of a criterion (coordenate of the vector to aggregate) is like a favor
#' @export
#'
#' @examples
#' favor_winsorized_weighted_mean(weights = c(rep(0.2,10), 0.5,0.5), r=2, s=2)
favor_winsorized_weighted_mean<-function(weights, r, s=r, alpha=NULL, beta=alpha){

  if (any(!is.numeric(weights), weights < 0, is.na(weights)))
    stop("The weights must be positive numbers")

  if (any(c(!is.null(alpha),!is.null(beta))))
    if (any(c(alpha > 0.5, beta > 0.5, alpha < 0, beta < 0)))
      stop("the winsorized fraction must be between 0 and 0.5")

  n <- length(weights)

  if (any(r<0 , s<0 , r > n/2 , s > n/2) )
    stop("'r' and 's' must be positive numbers less
           than  half the length of x" )

  if (!is.null(alpha))  r <- floor(n * alpha) + 1
  if (!is.null(beta))   s <- floor(n * beta) + 1

  p <- weights / sum(weights)
  a <- ((n - 1)*(n - 2) + r*(r + 1) - s*(s - 1)) / (2*(n - 1)^2)
  favor <- (1 - p)*a + p*(1 - (s / (n - 1)))

  return(favor)
}



