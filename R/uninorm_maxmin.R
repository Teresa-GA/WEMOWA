#' The max - min uninorm
#'
#' @param x A number between 0 and 1
#' @param y A number between 0 and 1
#' @param n A natural number. 1/n is the neutral element of this max-min uninorm
#'
#' @return A numeric vector giving the capacity corresponding to each subset from the set formed from the numbers 1,2,..., n
#' @export
#'
#' @examples
#'uninorm_maxmin(0.02,0.04,10)
uninorm_maxmin<-function(x,y,n){

  if (any(!is.numeric(x), length(y) != 1L, !is.numeric(x), length(y) != 1L) )
    stop("'x' and 'y' must be numeric of length one")
  if (any(sign(x) ==-1, sign(y)==-1, x > 1, y>1) )
      stop("'x' and 'y' must be in [0, 1]")

  if(as.integer(n)!= n) stop("n should be an integer")

  n<-as.integer(n)

  if (y<1/n) u<-min(x,y)
  else {if (y==1/n) u<-x
  else u<-max(x,y)
  }
  return(u)
}


