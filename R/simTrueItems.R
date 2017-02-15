#' Simulate true item parameters
#'
#' @description Creates true item parameters for a 2-parameters IRT model for
#'   posterior generation of item responses, IRT implementation and equating.
#'
#' @param I number of items per form
#' @param C number of common items between two forms
#' @param T number of forms
#' @param min.a Lower bound for the (uniform) distribution of item
#'   discrimination parameter
#' @param max.a Upper bound for the (uniform) distribution of item
#'   discrimination parameter
#' @param mu.b Mean of (normally-distributed) item difficulty parameter
#' @param sd.b Standard deviation of (normally-distributed) item difficulty
#'   parameter
#'
#' @return List of true item parameters per form
#' @author Waldir Leoncio
#' @export

simTrueItems <- function(I, C, T, min.a = .8, max.a = 1.2, mu.b = 0, sd.b = 1)
  # TODO: change gen.a range to .5--2? See Andersson & Wiberg, 2016
{
  # Distributions of parameters
  gen.a <- function() runif(n = 1, min = min.a, max = max.a)
  gen.b <- function() rnorm(n = 1, mean = mu.b,  sd = sd.b)

  # Generation of true item parameters --------------------------------------
  total.unique.items <- I * T - (C * (T - 1))
  true.item.parms    <- matrix(nrow = total.unique.items, ncol = 2 * T)
  rownames(true.item.parms) <- paste0("i", formatC(1:total.unique.items,
                                                   flag = "0", width = 2))
  colnames(true.item.parms) <- paste0(rep(1:T, each = 2), rep(letters[1:2], T))

  # First form only has unique items
  for (i in 1:I)
  {
    true.item.parms[i, "1a"] <- gen.a()
    true.item.parms[i, "1b"] <- gen.b()
    true.item.parms.short    <- list(true.item.parms[, 1:2])  # for future ref.
  }

  if (T > 1)
  {
    # Forms 2:T are directly linked to one of their previous forms
    unique.i <- I - C
    for (t in 2:T)
    {
      linked.form    <- sample(x = 1:(t - 1), size = 1)
      if (T > 2) cat("Form", t, "is directly linked to form", linked.form, "\n")
      first.unique.i <- (I + 1) + unique.i * (t - 2)
      last.unique.i  <- first.unique.i + unique.i - 1
      a.col <- 2 * t - 1
      b.col <- a.col + 1
      for (i in first.unique.i:last.unique.i)
      {
        # Generates unique items
        true.item.parms[i, a.col] <- gen.a()
        true.item.parms[i, b.col] <- gen.b()
      }

      # Takes some items from linked form
      if (linked.form == 1)
      {
        first.unique.linked <- 1
        last.unique.linked  <- I
      }
      else
      {
        first.unique.linked <- (I + 1) + unique.i * (linked.form - 2)
        last.unique.linked  <- first.unique.linked + unique.i - 1
      }
      items.linked <- true.item.parms[first.unique.linked:last.unique.linked, ]
      common.i <- sample(row.names(items.linked), C)
      true.item.parms[common.i, a.col] <- true.item.parms[common.i,
                                                          paste0(linked.form, "a")]
      true.item.parms[common.i, b.col] <- true.item.parms[common.i,
                                                          paste0(linked.form, "b")]
      true.item.parms.short[[t]]       <- true.item.parms[, a.col:b.col]
    }
  }

  for (t in 1:T)
  {
    present.items <- complete.cases(true.item.parms.short[[t]])
    true.item.parms.short[[t]] <- true.item.parms.short[[t]][present.items, ]
  }
  names(true.item.parms.short) <- paste0("t", 1:T)
  return(true.item.parms.short)
}
