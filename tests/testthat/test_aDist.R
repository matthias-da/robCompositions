data(expenditures)
x <- as.matrix(expenditures)

test_that("aDist rejects x and y of different dimensions", {
  expect_error(aDist(x, x[1:3, ]), "same dimensions")
  expect_error(aDist(x[, 1:4], x), "same dimensions")
  expect_error(aDist(expenditures[, 1:4], expenditures), "same dimensions")
  expect_error(aDist(1:5, 1:4), "same dimensions")
})

test_that("aDist between data sets is unchanged for valid input", {
  expect_equal(aDist(x, x), 0)
  y <- x * 1.5                     # scale invariance: distance 0
  expect_equal(aDist(x, y), 0, tolerance = 1e-12)
  expect_equal(aDist(as.numeric(x[1, ]), as.numeric(x[2, ])),
               sqrt(sum((cenLR(x[1:2, ])$x.clr[1, ] - cenLR(x[1:2, ])$x.clr[2, ])^2)))
  one <- aDist(expenditures[1, ], expenditures[2, ])
  expect_equal(dim(one), c(1L, 1L))
  expect_equal(dimnames(one), list("1", "2"))
})

test_that("ced is aDist divided by ni", {
  xi <- x
  xi[1, 3] <- xi[1, 3] * 2
  xi[4, 2] <- xi[4, 2] / 3
  expect_equal(ced(x, xi, ni = 2), aDist(x, xi) / 2)
  expect_equal(ced(x, x, ni = 1), 0)
  expect_error(ced(x, xi[, 1:4], ni = 2), "same dimensions")
})
