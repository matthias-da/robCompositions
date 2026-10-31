grid <- seq(-4.7, 4.7, length = 1000)
step <- diff(grid[1:2])
f <- dnorm(grid, mean = 0, sd = 1.5)

test_that("trapzc integrates exactly for linear functions, including n = 2", {
  expect_equal(trapzc(1, c(1, 1)), 1)
  expect_equal(trapzc(0.5, c(0, 1, 2)), 1)
  expect_equal(trapzc(step, grid), 0, tolerance = 1e-10)
  expect_error(trapzc(1, 1), "at least two")
})

test_that("trapzc agrees with the previous implementation for n >= 3", {
  old <- function(step, f) step * (0.5 * f[1] + sum(f[2:(length(f) - 1)]) + 0.5 * f[length(f)])
  expect_equal(trapzc(step, f), old(step, f))
  expect_equal(trapzc(step, log(f)), old(step, log(f)))
})

test_that("fcenLR integrates to zero and is scale invariant", {
  cl <- fcenLR(grid, step, f)
  expect_length(cl, length(f))
  expect_equal(trapzc(step, cl), 0, tolerance = 1e-10)
  expect_equal(fcenLR(grid, step, 5 * f), cl)
})

test_that("fcenLRinv inverts fcenLR up to normalisation", {
  back <- fcenLRinv(grid, step, fcenLR(grid, step, f))
  expect_equal(back, f / trapzc(step, f))
  expect_equal(trapzc(step, fcenLRinv(grid, step, fcenLR(grid, step, f), k = 100)), 100)
})

test_that("fcenLRp is centred with respect to P and fcenLRu = sqrt(p) * fcenLRp", {
  t2 <- seq(1, 10, length = 500)
  s2 <- diff(t2[1:2])
  g <- dlnorm(t2, meanlog = 1.5, sdlog = 0.5)
  p <- dexp(t2, 0.25) / trapzc(s2, dexp(t2, 0.25))
  fp <- (g / p) / trapzc(s2, g / p)
  clp <- fcenLRp(t2, s2, fp, p)
  expect_equal(trapzc(s2, clp * p), 0, tolerance = 1e-10)
  expect_equal(fcenLRu(t2, s2, fp, p), sqrt(p) * clp)
  # with a constant reference density, fcenLRp reduces to fcenLR
  expect_equal(fcenLRp(t2, s2, g, rep(1, length(t2))), fcenLR(t2, s2, g))
})
