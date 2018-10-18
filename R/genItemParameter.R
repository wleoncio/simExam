#' Generate Item Parameter Values
#'
#' @param type Either "a" for the item discrimination or "b" for the item
#'   difficulty
#' @param parms vector of length two. If type = "a", parms must be setup such
#'   that \eqn{a ~ U(parms[1], parms[2])}. For type = "b", parms must be such
#'   that \eqn{b ~ N(parms[1], parms[2])}.
#' @param quantity number of item parameters to generate
#' @param truncate.b vector of length 2 containing the limits for the difficulty
#'   distribution
#' @importFrom msm rtnorm
#' @return scalar containing one item parameter
#' @export
#'
genItemParameter <- function(type, parms, quantity = 1,
                             truncate.b = c(-Inf, Inf)) {
  if (type == "a") {
    item.parameter <- runif(n = quantity, min = parms[1], max = parms[2])
  } else if (type == "b") {
    item.parameter <- msm::rtnorm(n     = quantity,
                                  mean  = parms[1],
                                  sd    = parms[2],
                                  lower = truncate.b[1],
                                  upper = truncate.b[2])
  } else {
    stop("type must be 'a' or 'b'.")
  }
  return(item.parameter)
}
