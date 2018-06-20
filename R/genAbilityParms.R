#' Generate True Ability Parameters
#'
#' @param forms.n number of forms
#' @param mu.first mean ability of first form
#' @param sd.first standard deviation of ability of first form
#' @param mu.last mean ability of last form
#' @param sd.last standard deviation of ability of last form
#'
#' @return List containing means and standard deviations per form
#' @export
#'
genAbilityParms <- function(forms.n, mu.first, sd.first,
                            mu.last = mu.first, sd.last = sd.first) {
  list(mu = seq(mu.first, mu.last, length = forms.n),
       sd = seq(sd.first, sd.last, length = forms.n))
}
