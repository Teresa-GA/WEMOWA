#' It computes the SUOWA aggegated value of a vector
#'
#' @param x A numerical vector to be aggegate
#' @param mu_SUOWA A numerical vector giving the capacity (fuzzy measure) to be used
#' @param final_weights A logical value to require the specific final weights used during the computation of the SUOWA aggregated value
#' @return A numeric value with the aggegated value obtained using the SUOWA operator. When it is require, this function returns the ordered initial vector toghether with the corresponding weights used for the computation
#' @export
#'
#' @examples
#' we<-c(0, 0.5, 0.5, 0 )
#' pe<- c(0.5, 0.2, 0.2, 0.1)
#' mu <- capacity_SUOWA(weights_p = pe, weights_w = we, uninorm = "uninom_max_min")[, "mu_SUOWA"]
#' x <- c(10, 5, 4, 6)
#' SUOWA(x, mu_SUOWA = mu)
#' SUOWA(x, mu_SUOWA = mu, final_weights =TRUE)
SUOWA <- function(x, mu_SUOWA, final_weights = FALSE){

  n<-length(x)

  A <-list() ; valores_en_A <-list()
  A_anterior <-list() ;

  for (i in 1:n){
    A[[i]]<-sort(order(x, decreasing = T)[1:i])
    valores_en_A[[i]]<-x[order(x, decreasing = T)][1:i]
    if (i !=1) A_anterior[[i]] <- sort(order(x, decreasing = T)[1:(i-1)])
    else A_anterior[[1]] <-NA
  }

  # genera power_set en una lista en el orden que utiliza kappalab
  n <- length(x)
  subconjuntos_todos <- c(list(integer(0)),
                          unlist(lapply(1:n,
                                        function(k) {combn(1:n, k, simplify = FALSE)}),
                                 recursive = FALSE ) )

  # subconjuntos_todos
  # emparejar los subconjuntos que necesitamos con la posición en todo
  # que será la posición en la capacity mu_SUOWA

  A.char <- sapply(A, paste, collapse = ",")
  A_anterior.char <- sapply(A_anterior, paste, collapse = ",")

  subconjuntos_todos.char <- sapply(subconjuntos_todos, paste, collapse = ",")

  posiciones <- match(A.char,subconjuntos_todos.char)
  posiciones_anterior <- match(A_anterior.char,subconjuntos_todos.char)

  mu_SUOWA_i_1 <- ifelse(is.na(posiciones_anterior),
                         0,
                         mu_SUOWA[posiciones_anterior])

  mu_SUOWA_i <- mu_SUOWA[posiciones]

  s<-mu_SUOWA_i -  mu_SUOWA_i_1
  names(s) <- NULL
  suowa <-sum(s*x[order(x, decreasing = T)])

  if(final_weights == TRUE) {output <- list(suowa= suowa,
                                     computation = cbind(s=s,
                                                         reordered_x = x[order(x, decreasing = T)]
                                                         )
                                    )} else output <- suowa

  return(output)
}

