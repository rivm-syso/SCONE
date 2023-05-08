qqbinom <- Vectorize(
#' Calculate quantile of binomial distribution 
#'
#' @param q quantile
#' @param x number of observations
#' @param n total number of attempts
#'
#' @return value at quantile q
#' @export
#'
#' @examples
  function(q, x, n) {
    if(q >= 0.5 & x == n) {
      1 
    } else if(q < 0.5 & x == 0) {
      0 
    } else if(x <= 0.5*n) {
      uniroot(function(p) pbinom(q = x, size = n, prob = p) - (1-q), c(0,1))$root
    } else {
      1-uniroot(function(p) pbinom(q = n-x, size = n, prob = p) - q, c(0,1))$root
    }
  }
)
