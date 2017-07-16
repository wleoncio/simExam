#' Probability of Getting an Item Correct
#'
#' @description Calculates the probability of getting an item correct under a
#'   2-parameter IRT model.
#'
#' @param theta examinee skill
#' @param a     item discimination
#' @param b     item difficulty
#'
#' @return Probability of getting a correct item.
#' @export
probIRT <- function(theta, a, b) {
  ex   <- exp(a * (theta - b))
  prob <- ex / (1 + ex)
  return(prob)
}
