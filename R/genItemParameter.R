#' Generate Item Parameter Value
#'
#' @param type Either "a" for the item discrimination or "b" for the item
#'   difficulty
#' @param parms vector of length two. If type = "a", parms must be setup such
#'   that \eqn{a ~ U(parms[1], parms[2])}. For type = "b", parms must be such
#'   that \eqn{b ~ N(parms[1], parms[2])}.
#'
#' @return scalar containing one item parameter
#' @export
#'
genItemParameter <- function(type, parms) {
  if (type == "a") {
    item.parameter <- runif(n = 1, min = parms[1], max = parms[2])
  } else if (type == "b") {
    item.parameter <- rnorm(n = 1, mean = parms[1], sd = parms[2])
  }
  return(item.parameter)
}
