#' Winsorized weighted mean
#'
#' @param x A numerical vector
#' @param r An integer. The number of observations to be winsorized from the lower end of x before the mean is computed
#' @param s An integer. The number of observations to be winsorized from the upper end of x before the mean is computed
#' @param alpha The fraction (0 to 0.5) of observations to be winsorized from the lower end of x before the mean is computed
#' @param beta The fraction (0 to 0.5) of observations to be winsorized from the upper end of x before the mean is computed
#' @param w A numerical vector of weights the same length as x giving the weights to use for elements of x
#' @param na.rm a logical evaluating to TRUE or FALSE indicating whether NA values should be stripped before the computation proceeds
#' @param ... arguments to be passed to or from methods
#'
#' @return The winsorized weighted mean computed using the arguments
#' @export
#'
#' @examples
#' x<-1:20
#' winsorized_mean(x,r=2)
winsorized_mean<- function (x,
            r = 1,  s = r,
            alpha = NULL, beta = alpha,
            w=NULL ,
            na.rm = FALSE,
            ...){

    if (!is.numeric(x)) {
      warning("argument is not numeric: returning NA")
      return(NA_real_)
    }
    if (is.null(w)) w<-rep(1, length(x))

    if (length(w) != length(x))
      stop("'x' and 'w' must have the same length")

    if (any(c(!is.null(alpha),!is.null(beta))))
      if (any(c(alpha > 0.5, beta > 0.5, alpha < 0, beta < 0)))
        stop("the winsorized fraction must be between 0 and 0.5")
    if (isTRUE(na.rm)) {
      w <- w[!is.na(x)]
      x <- x[!is.na(x)]
    }
    if (is.complex(x))
      stop("winsorized means are not defined for complex data")
    if (anyNA(x))
      return(NA_real_
      )

    n <- length(x)

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

    x_winsorized <- x[order(x)]
    if (r > 0)
      x_winsorized[1:r] <- x[r + 1]
    if (s > 0)
      x_winsorized[(n - s):n] <- x[n - s]

    m<-sum((x_winsorized * w)[w != 0])/sum(w)

    return(m)
  }
