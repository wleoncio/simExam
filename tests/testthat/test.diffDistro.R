context("Distribution of difficulties")

test_that("Difficulties are normally-distributed", {
  item.parms <- genTrueItems(C = 0, J = 1000, num.forms = 1)
  expect_true(min(item.parms$t1[, "1b"]) <= -2)
  expect_true(max(item.parms$t1[, "1b"]) >=  2)
})

test_that("Difficulties are truncated", {
  item.parms <- genTrueItems(C = 0, J = 1000, num.forms = 1, truncate.b = c(-2, 2))
  expect_true(min(item.parms$t1[, "1b"]) >= -2)
  expect_true(max(item.parms$t1[, "1b"]) <=  2)
})
