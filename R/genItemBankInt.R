#' Generate item bank for NEAT with internal anchor items
#'
#' @param C number of common items between two test
#' @param J number of total items (unique + common) per test
#' @param t.tot number of tests
#' @param min.a Lower bound for the (uniform) distribution of item
#'   discrimination parameter
#' @param max.a Upper bound for the (uniform) distribution of item
#'   discrimination parameter
#' @param mu.b Mean of (normally-distributed) item difficulty parameter
#' @param sd.b Standard deviation of (normally-distributed) item difficulty
#'   parameter
#' @param leading0 format item names with leading zeros for better ordering?
#' @param num.digits number of digits for representing items
#' @param linkage.plan square matrix or order t.tot representing the number of
#'   items each form has in common with another form. If NULL, links between
#'   forms will be chosen randomly.
#' @param ... parameters to pass to child functions
#' @importFrom stats runif rnorm complete.cases
#' @importFrom Matrix tril
#' @return list containing 2PL item parameters per form
#' @export
genItemBankInt <- function(C, J, t.tot, min.a, max.a, mu.b, sd.b,
                           leading0 = TRUE, num.digits = 4,
                           linkage.plan = NULL, ...) {
  if (missing(C)) C <- NA
  if (missing(J)) J <- NA
  if (!is.null(linkage.plan)) {
    total.U <- sum(Matrix::tril(linkage.plan))
  } else {
    if (C > J) stop("C must be smaller than J")
    if (C > (J / 2)) message("For J = ", J, ", C should be at most ",
                             floor(J / 2), ", otherwise the linkage plan may ",
                             "be impossible. \nIn case of errors, try a",
                             " smaller value for C.")
    total.U <- J * t.tot - (C * (t.tot - 1))

  }


  # Create empty aggregated item bank
  true.items <- matrix(nrow = total.U, ncol = 2 * t.tot)
  if (leading0) {
    itemNames <- formatC(1:total.U, width = num.digits, flag = 0)
  } else {
    itemNames <- 1:total.U
  }
  rownames(true.items) <- paste0("j", itemNames)
  colnames(true.items) <- paste0(rep(1:t.tot, each = 2), letters[1:2])

  # Expand linkage plan
  if (is.null(linkage.plan)) {
    items <- fillRandomLinkagePlan(t.tot, J, C, true.items, min.a, max.a,
                                   mu.b, sd.b, ...)
  } else {
    items <- fillCustomLinkagePlan(t.tot, true.items, linkage.plan,
                                   min.a, max.a, mu.b, sd.b)
  }
  output <- list("matrix" = items$matrix, "list" = items$list)
  return(output)
}
