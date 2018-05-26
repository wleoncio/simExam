#' Fill matrix of true.items with values from a custom linkage plan
#'
#' @param t.tot number of tests
#' @param J number of total items (unique + common) per test
#' @param C number of common items between two test
#' @param true.items matrix containing all items in the rows and all test forms
#'   in the columns
#' @param linkage.plan matrix containing the number of items each test form has
#'   in common with itself and another test form
#' @param min.a Lower bound for the (uniform) distribution of item
#'   discrimination parameter
#' @param max.a Upper bound for the (uniform) distribution of item
#'   discrimination parameter
#' @param mu.b Mean of (normally-distributed) item difficulty parameter
#' @param sd.b Standard deviation of (normally-distributed) item difficulty
#'   parameter
#'
#' @export
fillCustomLinkagePlan <- function(t.tot, J, C, true.items, linkage.plan,
                                  min.a, max.a, mu.b, sd.b) {
  true.items.short <- list()
  for (t.ref in seq(t.tot)) {
    a.col.ref <- (2 * t.ref) - 1
    b.col.ref <- 2 * t.ref
    first.unused.item <- nrow(true.items) -
      sum(apply(true.items, 1, function(x) all(is.na(x)))) + 1
    num.unique.items <- linkage.plan[t.ref, t.ref]
    last.item.to.use <- first.unused.item + num.unique.items - 1
    true.items[first.unused.item:last.item.to.use, a.col.ref] <-
      genItemParameter("a", c(min.a, max.a), num.unique.items)
    true.items[first.unused.item:last.item.to.use, b.col.ref] <-
      genItemParameter("b", c(mu.b, sd.b), num.unique.items)
    if (t.ref < t.tot) {
      for (t.link in (t.ref + 1):t.tot) {
        if (linkage.plan[t.ref, t.link] > 0) {
          a.col.link <- (2 * t.link) - 1
          b.col.link <- 2 * t.link
          first.unused.item <- nrow(true.items) -
            sum(apply(true.items, 1, function(x) all(is.na(x)))) + 1
          num.common.items <- linkage.plan[t.ref, t.link]
          last.item.to.use <- first.unused.item + num.common.items - 1
          true.items[first.unused.item:last.item.to.use, c(a.col.ref, a.col.link)] <-
            genItemParameter("a", c(min.a, max.a), num.common.items)
          true.items[first.unused.item:last.item.to.use, c(b.col.ref, b.col.link)] <-
            genItemParameter("b", c(mu.b, sd.b), num.common.items)
        }
      }
    }
    }

  # Converting to list
  for (t in seq(ncol(linkage.plan))) {
    true.items.t <- true.items[, (2 * t - 1):(2 * t)]
    true.items.t <- true.items.t[complete.cases(true.items.t), ]
    true.items.short[[t]] <- true.items.t
  }
  names(true.items.short) <- paste0("t", 1:t.tot)
  out <- list("matrix" = true.items, "list" = true.items.short)
  return(out)
}
