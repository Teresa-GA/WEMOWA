#' Weigths to use in a Winsorized weighted mean calculated from Shapley vales given
#'
#' @param Shapley_values A numerical vector with the Shapley values given
#' @param r An integer. The number of observations to be winsorized from the lower end of x before the mean is computed
#' @param s An integer. The number of observations to be winsorized from the upper end of x before the mean is computed
#' @param alpha The fraction (0 to 0.5) of observations to be winsorized from the lower end of x before the mean is computed
#' @param beta The fraction (0 to 0.5) of observations to be winsorized from the upper end of x before the mean is computed
#'
#' @return A vector of weights
#' @export
#'
#' @examples
#' v<- c(rep(0.2,3),rep(0.1, 4))
#' weights_from_Shapley_values(Shapley_values=v , r=1, s=0)
#'
weights_from_Shapley_values<-function(Shapley_values, r, s, alpha=NULL, beta=alpha){


  if (any(c(!is.null(alpha),!is.null(beta))))
    if (any(c(alpha > 0.5, beta > 0.5, alpha < 0, beta < 0)))
      stop("the winsorized fraction must be between 0 and 0.5")

  n <- length(Shapley_values)

  if (any(r<0 , s<0 , r > n/2 , s > n/2) )
    stop("'r' and 's' must be positive numbers less
           than  half the length of x" )

  if (!is.null(alpha))  r <- floor(n * alpha) + 1
  if (!is.null(beta))   s <- floor(n * beta) + 1

  if (any(!is.numeric(Shapley_values), is.na(Shapley_values)))
    stop("The Shapley values must be numeric without NA")

  if (r + s >= n - 1)
    stop( paste(" r + s must be less than ", length(Shapley_values)-1), "for that Shapley values" )

  if (min(Shapley_values) < (r + s) / (n * (n - 1)))
    stop("Weights do not exits for the required Shapley values with the r and s given")


  weights <- (1 / n) + ((n - 1) / (n - 1 - r - s)) * (Shapley_values - (1 / n))

  return(weights)
}
