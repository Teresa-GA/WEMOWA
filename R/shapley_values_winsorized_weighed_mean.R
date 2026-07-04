#' Shapley values associated with the (r,s)-fold Winsorized weighted mean
#'
#' @param n A positive integer. It represents the dimension of the vector to apply winsorized mean
#' @param r A positive integer. The number of observations to be winsorized from the lower end of x before the mean is computed
#' @param s A positive integer. The number of observations to be winsorized from the upper end of x before the mean is computed
#' @param alpha A real number between 0 and 0.5. It gives the proportion of observations to be winsorized from the lower end of x before the mean is computed
#' @param beta A real number between 0 and 0.5. It gives the proportion of observations to be winsorized from the upper end of x before the mean is computed
#' @param w A numerical vector of weights with the same length as x. It gives the weights to use in the Winsorized weighted mean
#'
#' @return The Shapley values for a winsorized weighted mean for a vecotor of length n computed using the arguments given
#' @export
#'
#' @examples
#' x<-1:20
#' winsorized_weighted_mean(x,r=2)
#' Shapley_values_winsorized_weighed_mean(n=20, r=2)
Shapley_values_winsorized_weighed_mean<- function (n,
                       r = 1,  s = r,
                       alpha = NULL, beta = alpha,
                       w=NULL){

  # checking if arguments are valid

  if (is.null(w)) w<-rep(1, n)


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

  # computing the Shapley values

  p <- w / sum(w)
  a <- (r + s) / (n - 1)
  phi <- a * (1 / n) + (1 - a) * p

  return(phi)
 }
