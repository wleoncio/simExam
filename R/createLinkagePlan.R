#' Creates linkage plan
#'
#' @param J number of items per form
#' @param forms number of forms
#' @param C number of items in common between two forms
#' @param forms.linked maximum number of forms linked to one form
#' @param wrap.around if \code{TRUE}, links last form to first form
#' @return matrix with the number of items in common between two forms
#' @export
#'
#' @examples
#' # Linkage plan for 10 forms, 40 items per form, 5 items in common between
#' # each pair of forms, and with each form being connected at most to its 4
#' # closest neighbors.
#' createLinkagePlan(10, 40, 5, 4)
createLinkagePlan <- function(forms, J, C, forms.linked, wrap.around = FALSE) {
  plan <- matrix(0, nrow = forms, ncol = forms)
  for (t in seq(forms)) {
    linked.forms.first <- max(c(t - forms.linked, 0))
    linked.forms.last  <- min(c(t + forms.linked, forms))
    plan[t, linked.forms.first:linked.forms.last] <- C
  }
  diag(plan) <- J
  if (wrap.around) {
    plan[1, ncol(plan)] <- plan[ncol(plan), 1] <- C
  }
  return(plan)
}
