#' Generate true item parameters for NEAT design
#'
#' @description Creates true item parameters for a 2-parameters IRT model for
#'   posterior generation of item responses, IRT implementation and equating.
#'   Compatible with the common-item, non-equivalent group design (see Kolen et.
#'   al. 2004).
#'
#' @param C number of common items between two forms
#' @param J number of items per form
#' @param U number of unique items per form (only valid for external anchor)
#' @param num.forms number of forms
#' @param min.a Lower bound for the (uniform) distribution of item
#'   discrimination parameter
#' @param max.a Upper bound for the (uniform) distribution of item
#'   discrimination parameter
#' @param mu.b Mean of (normally-distributed) item difficulty parameter
#' @param sd.b Standard deviation of (normally-distributed) item difficulty
#'   parameter
#' @param anchor.type type of anchor item set ("internal" or "external")
#' @param output type of output; "list" gives a list of used items per form,
#'   "matrix" gives a joint matrix (data.frame).
#' @param linkage.plan matrix containing linkage plan
#' @param ... extra parameters to be passed to genItemBankInt() or
#'   genItemBankExt()
#' @return List of true item parameters per form
#' @author Waldir Leoncio
#' @export

genTrueItems <- function(C, J, U, num.forms, min.a = .5, max.a = 2,
                         mu.b = 0, sd.b = 1,
                         anchor.type = "internal", output = "list",
                         linkage.plan = NULL, ...) {
  # Generate item bank
  if (anchor.type == "internal") {
    if (is.null(linkage.plan)) {
      if (missing(J)) J <- U + C
      true.items <- genItemBankInt(C, J, num.forms, min.a, max.a, mu.b, sd.b, ...)
    } else {
      true.items <- genItemBankInt(t.tot = num.forms, min.a = min.a,
                                   max.a = max.a, mu.b = mu.b, sd.b = sd.b,
                                   linkage.plan = linkage.plan, ...)
    }
  } else {
    if (missing(U)) U <- J - C
    true.items <- genItemBankExt(C, U, num.forms, min.a, max.a, mu.b, sd.b, ...)
  }
  return(true.items[[output]])
}
