#' Simulate exam test answers
#'
#' @description Simulates answers for a test, given the true item parameters (2PL model) as
#' well as the examinee skills per test.
#'
#' @param true.item.parms true item parameters per form
#' @param true.skills     true examinee skill per form
#' @export
#' @return List of size equal to the number of forms. Each element of the list
#'   contains a matrix of dichotomic answers (right or wrong) per examinee and
#'   item.
genExam <- function(true.item.parms = genTrueItems(),
                    true.skills     = genTrueSkills())
{
  # Generation of results for all administrations ---------------------------
  E <- nrow(true.skills)
  T <- length(true.item.parms)
  I <- nrow(true.item.parms[[1]])
  test.scores  <- replicate(T, matrix(nrow = E, ncol = I), simplify = FALSE)
  total.scores <- matrix(nrow = E, ncol = T)

  for (t in 1:T)
  {
    for (e in 1:E)
    {
      for (i in 1:I)
      {
        p <- probIRT(theta = true.skills[e, t],
                     a     = true.item.parms[[t]][i, 1],
                     b     = true.item.parms[[t]][i, 2])
        test.scores[[t]][e, i] <- rbinom(n = 1, size = 1, prob = p)
      }
    }
    colnames(test.scores[[t]]) <- rownames(true.item.parms[[t]])
    rownames(test.scores[[t]]) <- rownames(true.skills)
    total.scores[, t] <- rowSums(test.scores[[t]])
  }
  names(test.scores) <- paste0("t", 1:T)
  return(test.scores)
}
