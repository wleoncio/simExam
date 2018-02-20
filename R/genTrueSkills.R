#' Simulates true examinee skills per form
#'
#' @description Generates a matrix containing the true (latent) skill per
#'   administration for a given group of examinees.
#'
#' @param E  number of examinees per form
#' @param num.forms  number of test forms
#' @param mu.skill mean skill per form
#' @param sd.skill standard deviation of skills per form
#'
#' @return Matrix of examinee skill per form
#' @export
#'
genTrueSkills <- function(E, num.forms, mu.skill = rep(0, num.forms),
                          sd.skill = rep(1, num.forms)) {
  true.skills <- matrix(nrow = E, ncol = num.forms)
  for (e in 1:E) {
    for (t in 1:num.forms) {
      true.skills[e, t] <- rnorm(n = 1, mean = mu.skill[t], sd = sd.skill[t])
    }
  }
  rownames(true.skills) <- paste0("e", 1:E)
  colnames(true.skills) <- paste0("t", 1:num.forms)
  return(true.skills)
}
