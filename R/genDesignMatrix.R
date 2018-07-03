#' Generate design matrices
#'
#' @description Generates a design matrix to calculate the regression on a
#'   vector of item parameters
#' @param num.forms number of forms
#' @param num.items total number of items across all forms
#' @param J either a scalar with the number of items per form or a list with the
#'   number of items on each form
#' @param t.names names of forms
#' @param items.t matrix of items on each form
#' @param print print table after computations?
#' @return design matrix
#' @export
genDesignMatrix <- function(num.forms, num.items, J, t.names, items.t,
                             print = FALSE) {
  # Assembling partial design matrix X.part, which will compose the complete
  # design matrix X.out
  if (length(J) > 1) {
    row.names <- c(paste0(rep(t.names, J / 2),
                          rep("b", sum(J)),
                          unlist(items.t)),
                   paste0(rep(t.names, J / 2),
                          rep("a", sum(J)),
                          unlist(items.t)))
    X.part <- matrix(0, nrow = sum(J), ncol = num.items)
  } else {
    X.part <- matrix(0, nrow = J * num.forms, ncol = num.items)
    row.names <- c(paste0(rep(t.names, each = J),
                          rep("b", each = num.forms * J),
                          unlist(items.t)),
                   paste0(rep(t.names, each = J),
                          rep("a", each = num.forms * J),
                          unlist(items.t)))
  }

  for (t in t.names) {
    for (col in items.t[[t]]) {
      t.num <- as.numeric(gsub("t", "", t))
      if (length(J) > 1) {
        r <- which(rowSums(X.part) == 0)[1]  # first available row
      } else {
        r <- which(col == items.t[[t]]) + (t.num - 1) * length(items.t[[t]])
      }
      X.part[r, col] <- 1
    }
  }
  # Assembling the complete design matrix. Some parts are zeroed out because an
  # item item parameter can only be a or b.
  X.out <- rbind(cbind(X.part, 0 * X.part), cbind(0 * X.part, X.part))
  col.names <- paste0(rep(letters[2:1], each = num.items), 1:num.items)
  colnames(X.out) <- col.names
  rownames(X.out) <- row.names
  X.out <- X.out[rowSums(X.out) > 0, colSums(X.out) > 0]
  if (print) print.table(X.out, zero.print = "")
  return(X.out)
}
