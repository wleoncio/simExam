#' Simulate exam test answers
#'
#' @description Simulates answers for a test, given the true item parameters
#'   (2PL model) as well as the examinee skills per test.
#'
#' @param true.item.parms true item parameters per form
#' @param true.skills     true examinee skill per form
#' @param join.anchors join answers for anchor items on each test?
#' @importFrom stats rbinom var
#' @export
#' @return List of size equal to the number of forms. Each element of the list
#'   contains a matrix of dichotomic answers (right or wrong) per examinee and
#'   item.
genExamAnswers <- function(true.item.parms, true.skills, join.anchors = FALSE) {
  # Detection of external common items
  external.pos <- match("t0", names(true.item.parms))
  external     <- !is.na(external.pos)
  if (external) {
    # TODO: breaks now that true.skills is a list!
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
  I <- sapply(true.skills, length)
  T <- length(true.item.parms)
  J <- sapply(true.item.parms, nrow)

  test.scores <- list()

  for (t in 1:T) {
    p <- sapply(true.skills[[t]],
                function(x) probIRT(theta = x,
                                    a     = true.item.parms[[t]][, 1],
                                    b     = true.item.parms[[t]][, 2]))
    test.scores[[t]] <-
      t(sapply(seq(I[[t]]), function(x) rbinom(n = J[[t]], 1, prob = p[, x])))
    one.ans <- apply(test.scores[[t]], 2, stats::var) == 0
    while (any(one.ans)) {
      cat("Form ", t, " has unanimous scores for items ", which(one.ans),
          ". Retesting.\n", sep = "")
      test.scores[[t]][, one.ans] <-
        t(sapply(seq(I[[t]]), function(x) rbinom(n = sum(one.ans), 1,
                                                 prob = p[one.ans, x])))
      one.ans <- apply(test.scores[[t]], 2, stats::var) == 0
    }
    colnames(test.scores[[t]]) <- rownames(true.item.parms[[t]])
    rownames(test.scores[[t]]) <- names(true.skills[[t]])
  }
  names(test.scores) <- names(true.skills)

  # Merge back external anchors
  if (join.anchors) {
    if (external) {
      ext.names <- grep("t0.", names(test.scores))
      test.scores.ext <- test.scores[ext.names]
      test.scores.ext <- Reduce(rbind, test.scores.ext)
      test.scores <- test.scores[-ext.names]
      test.scores[["t0"]] <- test.scores.ext
    }
  }
  return(test.scores)
}
