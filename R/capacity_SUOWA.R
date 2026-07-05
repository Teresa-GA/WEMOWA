#' The capacity (or fuzzy measure) associated with a SUOWA operator
#'
#' @param weights_w A numerical vector of positive numbers. It gives the weights to use in the Ordered Weighted Average (OWA)
#' @param weights_p A numerical vector of positive numbers. It gives the weights to use in the weighted mean (WEM)
#' @param uninorm The name of the uninom nedeed to computue the capacity. At the moment, only uninom_maxmin is avaliable
#' @return A numeric matrix giving the capacity corresponding to each subset of the set formed from the numbers 1,2,..., n, for the SUOWA operator and the corresponding  to the WEM and the OWA that the SUOWA operator generalized
#' @export
#' @importFrom utils combn
#'
#' @examples
#' capacity_SUOWA(weights_p = 1:3, weights_w = 1:3)
capacity_SUOWA <- function(weights_p, weights_w, uninorm="uninorm_maxmin") {

  if (any(!is.numeric(weights_p), weights_p < 0, is.na(weights_p)))
    stop("All the weights must be positive numbers")

  if (any(!is.numeric(weights_w), weights_w < 0, is.na(weights_w)))
    stop("All the weights must be positive numbers")

  if (length(weights_p) != length(weights_w))
    stop("The length of weights_p and weights_w  must be the same")

  n<-length(weights_p)

  if (sum(weights_w )!=1) { weights_w <- weights_w/sum(weights_w) }

  if (sum(weights_p )!=1) {weights_p <- weights_p/sum(weights_p) }


  all_subsets <- power_set(n)
  cardinality <- all_subsets$size

  mu_WEM <-numeric(2^n)
  mu_OWA <- mu_WEM
  mu_SUOWA <-mu_WEM

  for(i in 2:(2^n)){

    indices <- all_subsets$elements[[i]]
    mu_WEM[i]<-sum(weights_p[ indices ])

    mu_OWA[i]<- sum(weights_w[1:cardinality[i]] )

    mu_SUOWA[i] <- cardinality[i]*uninorm_maxmin( mu_WEM[i]/cardinality[i],
                                              mu_OWA[i]/cardinality[i],
                                              n=n)

      }

  output <- cbind(cardinality, mu_SUOWA, mu_WEM, mu_OWA)
  rownames(output) <- all_subsets$subsets

  return(output)

}


