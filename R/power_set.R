
#' Title
#'
#' @param n A positive integer. It represents the size of the set
#' @param set A vector containing the elements od a set
#' @param sort A logical denoting if the subsets are sorted by cardinality
#'
#' @returns A list containing all the subsets (the power set) togetherr their cardinality. If sort is TRUE (the dafault), sorted by cardinality.
#' @export
#'
#' @examples power_set(4)
#' @examples power_set(set=c("a", "b", "c","d"))
power_set<-function (n, set=NULL, sort = TRUE)
{
  if (is.null(set)) {set<-1:n}
  d <- length(set)
  out <- expand.grid(rep(list(c(FALSE, TRUE)), d))
  out <- as.matrix(out)
  out <- apply(out, 1, function(x) set[x])
  if (sort) {
    i <- order(unlist(lapply(out, length)))
  }  else {
    i <- 1:length(out)
  }
  size <- sapply(out, length)[i]
  names(out) <- NULL
  nombres<-sapply(out[i],paste, collapse = ",")
  paste0("{",nombres, "}")


  return(list(subsets= paste0("{",nombres, "}"), elements = out[i], size = size))
}


