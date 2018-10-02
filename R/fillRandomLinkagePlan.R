#' Fill matrix of true.items with values for a random linkage plan
#'
#' @param t.tot number of tests
#' @param J number of total items (unique + common) per test
#' @param C number of common items between two test
#' @param true.items matrix containing all items in the rows and all test forms
#'   in the columns
#' @param min.a Lower bound for the (uniform) distribution of item
#'   discrimination parameter
#' @param max.a Upper bound for the (uniform) distribution of item
#'   discrimination parameter
#' @param mu.b Mean of (normally-distributed) item difficulty parameter
#' @param sd.b Standard deviation of (normally-distributed) item difficulty
#'   parameter
#' @param ... Arguments to pass to genItemParameter
#' @export
fillRandomLinkagePlan <- function(t.tot, J, C, true.items, min.a, max.a, mu.b,
                                    sd.b, ...) {
  true.items.short <- list()
  # First form only has unique items
  for (i in 1:J) {
    true.items[i, "1a"] <- genItemParameter("a", c(min.a, max.a), ...)
    true.items[i, "1b"] <- genItemParameter("b", c(mu.b, sd.b), ...)
    true.items.short    <- list(true.items[1:J, 1:2])  # for future ref.
    names(true.items.short) <- "t1"
  }

  if (t.tot > 1) {
    # Forms 2:t.tot are directly linked to one of their previous forms
    unique.i <- J - C
    for (t in 2:t.tot) {
      linked.t <- sample(x = 1:(t - 1), size = 1)  # Choose linked form
      if (t.tot > 2) cat("Form", t, "is directly linked to previous form",
                         linked.t, "\n")
      first.unique.i <- (J + 1) + unique.i * (t - 2)
      last.unique.i  <- first.unique.i + unique.i - 1
      a.col <- 2 * t - 1
      b.col <- a.col + 1
      for (i in first.unique.i:last.unique.i) {
        # Generates item parameters for unique items
        true.items[i, a.col] <- genItemParameter("a", c(min.a, max.a), ...)
        true.items[i, b.col] <- genItemParameter("b", c(mu.b, sd.b), ...)
      }

      # Takes some items from linked form
      if (linked.t == 1) {
        first.unique.linked <- 1
        last.unique.linked  <- J
      }
      else {
        first.unique.linked <- (J + 1) + unique.i * (linked.t - 2)
        last.unique.linked  <- first.unique.linked + unique.i - 1
      }
      items.linked <- true.items[first.unique.linked:last.unique.linked, ]
      common.i <- sort(sample(rownames(items.linked), C))
      true.items[common.i, a.col] <- true.items[common.i, paste0(linked.t, "a")]
      true.items[common.i, b.col] <- true.items[common.i, paste0(linked.t, "b")]

      true.items.short[[t]] <- true.items[, a.col:b.col]
      present.items <- complete.cases(true.items.short[[t]])
      true.items.short[[t]] <- true.items.short[[t]][present.items, ]
    }
    names(true.items.short) <- paste0("t", 1:t.tot)
  }
  out <- list("matrix" = true.items, "list" = true.items.short)
  return(out)
}
