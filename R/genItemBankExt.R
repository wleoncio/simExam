#' Generate item bank for NEAT with external anchor items
#'
#' @param C number of common items between two tests
#' @param U number of unique items per test
#' @param t.tot number of forms
#' @param min.a Lower bound for the (uniform) distribution of item
#'   discrimination parameter
#' @param max.a Upper bound for the (uniform) distribution of item
#'   discrimination parameter
#' @param mu.b Mean of (normally-distributed) item difficulty parameter
#' @param sd.b Standard deviation of (normally-distributed) item difficulty
#'   parameter
#'
#' @importFrom stats runif rnorm
#' @return list containing 2PL item parameters per form
#' @export
genItemBankExt <- function(C, U, t.tot, min.a, max.a, mu.b, sd.b) {
  # Distributions of parameters
  gen.a <- function() runif(n = 1, min = min.a, max = max.a)
  gen.b <- function() rnorm(n = 1, mean = mu.b, sd = sd.b)

  # Check if I or U was determined
  total.U <- (U * t.tot + C)
  total.T <- t.tot + 1
  all.T   <- c(1:t.tot, 0)  # form with anchor items = t0

  # Create empty aggregated item bank
  true.items           <- matrix(nrow = total.U, ncol = 2 * t.tot + 2)
  true.items.short     <- list()
  rownames(true.items) <- paste0("i", 1:total.U)
  colnames(true.items) <- paste0(rep(all.T, each = 2), letters[1:2])

  # Generate unique items for all forms except 0 (with anchor/common items)
  for (t in 1:t.tot) {
    first.u <- (t - 1) * U + 1
    last.u  <- t * U
    a.col   <- 2 * t - 1
    b.col   <- a.col + 1
    for (u in first.u:last.u) {
      true.items[u, paste0(t, "a")] <- gen.a()
      true.items[u, paste0(t, "b")] <- gen.b()
    }
    true.items.short[[t]] <- true.items[first.u:last.u, a.col:b.col]
  }

  # Common items
  true.items[(total.U - C + 1):total.U, ] <- c(replicate(C, gen.a()),
                                               replicate(C, gen.b()))
  rows.common <- (total.U - C + 1):total.U
  cols.common <- (total.T * 2 - 1):(total.T * 2)
  true.items.short[[total.T]] <- true.items[rows.common, cols.common]
  names(true.items.short) <- paste0("t", all.T)
  if (length(rows.common) == 1) {  # works around bug for C = 1 (item w/o name)
    true.items.short[["t0"]] <- matrix(true.items.short[["t0"]],
                                       nrow = 1,
                                       dimnames = list(rownames(true.items)[nrow(true.items)],
                                                       c("0a", "0b")))
  }
  output <- list("matrix" = true.items, "list" = true.items.short)
  return(output)
}
