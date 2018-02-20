#' Simulate exam test answers
#'
#' @description Simulates answers for a test, given the true item parameters
#'   (2PL model) as well as the examinee skills per test.
#'
#' @param true.item.parms true item parameters per form
#' @param true.skills     true examinee skill per form
#' @param join.anchors join answers for anchor items on each test?
#' @importFrom stats rbinom
#' @export
#' @return List of size equal to the number of forms. Each element of the list
#'   contains a matrix of dichotomic answers (right or wrong) per examinee and
#'   item.
genExamAnswers <- function(true.item.parms, true.skills, join.anchors = FALSE) {
  # Detection of external common items
  external.pos <- match("t0", names(true.item.parms))
  external     <- !is.na(external.pos)
  if (external) {
    not.ext.names <- names(true.item.parms)[-external.pos]
    true.skills  <- cbind(true.skills, true.skills)
    ext.names <- paste0("t0.", 1:(length(true.item.parms) - 1))
    colnames(true.skills) <- c(not.ext.names, ext.names)
    for (ext.name in ext.names) {
      true.item.parms[[ext.name]] <- true.item.parms[["t0"]]
    }
    true.item.parms <- true.item.parms[-external.pos]
    names(true.item.parms) <- colnames(true.skills)
  }

  # Generation of results for all administrations
  E <- nrow(true.skills)
  T <- length(true.item.parms)
  I <- lapply(true.item.parms, nrow)

  test.scores <- list()
  # total.scores <- matrix(nrow = E, ncol = T)

  # Creates test results for all tests
  for (t in 1:T) {
    test.scores[[names(I[t])]] <- matrix(nrow = E, ncol = I[[t]])
    for (e in 1:E) {
      for (i in 1:I[[t]]) {
        p <- probIRT(theta = true.skills[e, t],
                     a     = true.item.parms[[t]][i, 1],
                     b     = true.item.parms[[t]][i, 2])
        test.scores[[names(I[t])]][e, i] <- rbinom(n = 1, size = 1, prob = p)
      }
    }
    colnames(test.scores[[t]]) <- rownames(true.item.parms[[t]])
    rownames(test.scores[[t]]) <- rownames(true.skills)
    # total.scores[, t] <- rowSums(test.scores[[t]])
  }

  # Merge back external anchors
  if (join.anchors) {
    if (external) {
      ext.names <- grep("t0.", names(test.scores))
      test.scores.ext <- test.scores[ext.names]
      test.scores.ext <- Reduce(rbind, test.scores.ext)
      test.scores <- test.scores[-ext.names]
      # rownames(test.scores.ext) <- rep(rownames(true.skills), length(test.scores))
      test.scores[["t0"]] <- test.scores.ext
    }
  }
  return(test.scores)
}
