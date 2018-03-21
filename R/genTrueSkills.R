#' Simulates true examinee skills per form
#'
#' @description Generates a matrix containing the true (latent) skill per
#'   administration for a given group of examinees.
#'
#' @param I  number of test takers per form. It can have length 1, so all forms
#'   will have the same number of test takers, or be a larger vector with the
#'   respective number of examinees on each form
#' @param num.forms  number of test forms
#' @param mu.skill mean skill per form
#' @param sd.skill standard deviation of skills per form
#'
#' @return List of examinee skill per form
#' @export
#'
genTrueSkills <- function(I, num.forms, mu.skill = rep(0, num.forms),
                          sd.skill = rep(1, num.forms)) {
  true.skills <- list()
  if (length(I) == 1) I <- rep(I, num.forms)
  for (t in 1:num.forms) {
    true.skills[[t]] <- rnorm(n = I[t], mean = mu.skill[t], sd = sd.skill[t])
    names(true.skills[[t]]) <- paste0("i", 1:I[t])
  }
  names(true.skills) <- paste0("t", 1:num.forms)
  return(true.skills)
}
