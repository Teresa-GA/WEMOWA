#' Plot of Shapley values
#'
#' @param w A numerical vector of positive values.
#' @param r An integer. The number of observations to be winsorized from the lower end of x before the mean is computed
#' @param s An integer. The number of observations to be winsorized from the upper end of x before the mean is computed
#'
#' @return Plot of weights and Shapley values
#' @export
#' @importFrom ggplot2 ggplot aes geom_point coord_flip theme_classic
#' @examples
#' plot_Shapley_values(w=c(rep(0.2,3), rep(0.1,4)), r=2)
plot_Shapley_values <- function(w, r = NULL, s = r) {
  indice<-shapley<-NULL
  shapley <-Shapley_values_winsorized_weighed_mean(n = length(w),
                                           r = r, s = s,
                                           w = w)
  datos <- data.frame(indice = 1:length(w), w, shapley )
  g1 <- ggplot2::ggplot(datos, aes(x = w, y = indice)) +
    geom_point() +
    geom_point(aes(y = indice, x = shapley),
               pch = 21,
               col = "red") +
    coord_flip() +
    theme_classic()
  return(g1)
}
