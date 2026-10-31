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

## ---- 2.7.0: type = "rows"/"cross", weights, na.rm, cellweights -----------------

# brute force from the definition: weighted mean over the available pairwise
# log-ratios, scaled to all D(D-1)/2 log-ratios (see ?aDist)
bf_dist <- function(x, y, p = rep(1, length(x)), v = rep(1, length(x))) {
  D <- length(x); num <- den <- 0; allp <- 0
  for (j in 1:(D - 1)) for (k in (j + 1):D) {
    allp <- allp + p[j] * p[k]
    w <- p[j] * v[j] * p[k] * v[k]
    if (w > 0) {
      num <- num + w * (log(x[j] / x[k]) - log(y[j] / y[k]))^2
      den <- den + w
    }
  }
  if (den == 0) return(NA_real_)
  sqrt(as.numeric(allp * num / den / sum(p)))
}

set.seed(123)
X <- matrix(exp(rnorm(60)), 10, 6, dimnames = list(letters[1:10], paste0("p", 1:6)))
Y <- X * exp(rnorm(60, sd = 0.4))

test_that("type = 'rows' and 'cross' agree with single-composition distances", {
  r <- aDist(X, Y, type = "rows")
  expect_named(r, letters[1:10])
  expect_equal(unname(r), sapply(1:10, function(i) aDist(X[i, ], Y[i, ])))
  expect_equal(sum(r), aDist(X, Y))
  cr <- aDist(X, Y[1:4, ], type = "cross")
  expect_equal(dim(cr), c(10L, 4L))
  for (i in c(1, 7)) for (j in 1:4) expect_equal(cr[i, j], aDist(X[i, ], Y[j, ]))
  expect_equal(unname(aDist(X, X, type = "cross")), unname(as.matrix(aDist(X))))
  expect_error(aDist(X, Y[, 1:5], type = "cross"), "same number of columns")
})

test_that("weighted distance matches the definition and its properties", {
  p <- c(2, 1, 0.5, 1, 3, 1)
  d <- aDist(X, Y, type = "rows", weights = p)
  expect_equal(unname(d), sapply(1:10, function(i) bf_dist(X[i, ], Y[i, ], p)))
  expect_equal(aDist(X, Y, weights = rep(1, 6)), aDist(X, Y))
  expect_equal(aDist(X, Y, type = "rows", weights = 4 * p), 2 * d)       # sqrt(c) scaling
  expect_true(all(aDist(X, Y, type = "rows", weights = p / 2) <= d))      # monotone in weights
  expect_equal(aDist(X * 3, Y / 2, type = "rows", weights = p), d)        # scale invariance
  dm <- aDist(X, weights = p)
  expect_s3_class(dm, "dist")
  expect_equal(as.matrix(dm)[2, 5], bf_dist(X[2, ], X[5, ], p))
  expect_error(aDist(X, Y, weights = 1:5), "6 positive numbers")
  expect_error(aDist(X, Y, weights = c(0, 1, 1, 1, 1, 1)), "positive")
})

test_that("na.rm uses the available log-ratios", {
  Xm <- X
  Xm[1, 2] <- NA; Xm[2, c(1, 4)] <- 0; Xm[3, ] <- NA; Xm[3, 1:2] <- 1
  expect_error(aDist(Xm, Y), "missing or infinite")
  expect_error(aDist(Xm, weights = rep(1, 6)), "missing or infinite")
  expect_error(aDist(replace(X, 1, Inf), Y), "missing or infinite")
  d <- aDist(Xm, Y, type = "rows", na.rm = TRUE)
  ok <- is.finite(Xm) & Xm > 0
  ref <- sapply(1:10, function(i) bf_dist(Xm[i, ], Y[i, ], v = ok[i, ] * 1))
  expect_equal(unname(d[-3]), ref[-3])
  expect_equal(unname(d[3]), ref[3])            # two parts left: still defined
  Xm[4, -1] <- NA
  expect_true(is.na(aDist(Xm, Y, type = "rows", na.rm = TRUE)[4]))   # one part left
  # complete data: na.rm = TRUE changes nothing
  expect_identical(aDist(X, Y, na.rm = TRUE), aDist(X, Y))
  expect_identical(aDist(X, na.rm = TRUE), aDist(X))
  # a part missing everywhere = scaled distance of the subcomposition
  Xs <- X; Xs[, 3] <- NA
  expect_equal(aDist(Xs, Y, type = "rows", na.rm = TRUE),
               sqrt(5 / 4) * aDist(X[, -3], Y[, -3], type = "rows"))
  # distance matrix of incomplete data
  dm <- aDist(Xs, na.rm = TRUE)
  expect_s3_class(dm, "dist")
  expect_equal(as.matrix(dm)[1, 2], sqrt(5 / 4) * aDist(X[1, -3], X[2, -3]))
})

test_that("zero or negative values give NaN with a warning (as before 2.7.0)", {
  Xz <- X; Xz[2, 4] <- 0; Xz[5, 1] <- -1
  expect_warning(d <- aDist(Xz, Y, type = "rows"), "zero or negative")
  expect_true(all(is.nan(d[c(2, 5)])))
  expect_equal(d[-c(2, 5)], aDist(X, Y, type = "rows")[-c(2, 5)])
  expect_warning(s <- aDist(Xz, Y), "zero or negative")
  expect_true(is.nan(s))
  expect_warning(dm <- as.matrix(aDist(Xz, weights = rep(1, 6))), "zero or negative")
  expect_true(all(is.nan(dm[c(2, 5), -c(2, 5)])) && is.nan(dm[2, 5]))   # diagonal of as.matrix(dist) is 0
  expect_equal(dm[-c(2, 5), -c(2, 5)], as.matrix(aDist(X))[-c(2, 5), -c(2, 5)])
  expect_warning(dc <- aDist(X[1:3, ], Xz, type = "cross"), "zero or negative")
  expect_true(all(is.nan(dc[, c(2, 5)])))
  expect_equal(dc[, -c(2, 5)], aDist(X[1:3, ], X, type = "cross")[, -c(2, 5)])
  # mixed-sign vectors, e.g. riskutility::ait() on rnorm() data: NaN, no error
  expect_warning(v <- aDist(c(-1, 2, 3), c(1, 2, 3)), "zero or negative")
  expect_true(is.nan(v))
  # na.rm = TRUE still uses the available parts
  expect_false(anyNA(aDist(Xz, Y, type = "rows", na.rm = TRUE)))
})

test_that("cell weights generalise na.rm and combine with part weights", {
  W <- matrix(runif(60), 10, 6)
  p <- c(2, 1, 0.5, 1, 3, 1)
  d <- aDist(X, Y, type = "rows", cellweights = W, weights = p)
  expect_equal(unname(d), sapply(1:10, function(i) bf_dist(X[i, ], Y[i, ], p, W[i, ])))
  expect_equal(aDist(X, Y, cellweights = matrix(1, 10, 6)), aDist(X, Y))
  W01 <- matrix(1, 10, 6); W01[1, 2] <- 0
  Xm <- X; Xm[1, 2] <- NA
  expect_equal(aDist(X, Y, type = "rows", cellweights = W01),
               aDist(Xm, Y, type = "rows", na.rm = TRUE))
  Wy <- matrix(runif(60), 10, 6)
  expect_equal(unname(aDist(X, Y, type = "rows", cellweights = list(x = W, y = Wy))),
               sapply(1:10, function(i) bf_dist(X[i, ], Y[i, ], v = W[i, ] * Wy[i, ])))
  expect_error(aDist(X, Y, cellweights = W[, 1:5]), "same dimensions")
  expect_error(aDist(X, Y, cellweights = -W), "non-negative")
})

test_that("aDist(x) without options is dist(cenLR(x)), also for incomplete data", {
  Xm <- X; Xm[1, 2] <- NA; Xm[2, 3] <- -1
  d <- suppressWarnings(aDist(Xm))
  expect_equal(as.matrix(d), as.matrix(suppressWarnings(dist(cenLR(Xm)$x.clr))))
  expect_true(all(is.finite(as.matrix(d)[3:10, 3:10])))
  expect_equal(as.matrix(d)[3:10, 3:10], as.matrix(aDist(X))[3:10, 3:10])
  expect_s3_class(d, "dist")
  # clustering on incomplete data keeps working
  Xn <- X; Xn[1, 2] <- NA
  expect_false(anyNA(aDist(Xn)))
})

test_that("cell weights may be a data frame; lists need names x and y", {
  W <- matrix(runif(60), 10, 6)
  expect_equal(aDist(X, Y, cellweights = as.data.frame(W)), aDist(X, Y, cellweights = W))
  expect_equal(as.matrix(aDist(as.data.frame(X), cellweights = as.data.frame(W))),
               as.matrix(aDist(X, cellweights = W)))
  expect_equal(aDist(X, Y, cellweights = list(x = as.data.frame(W))),
               aDist(X, Y, cellweights = list(x = W)))
  expect_error(aDist(X, Y, cellweights = list(W, W)), "'x' and 'y'")
})

test_that("one-row input and the distance matrix in the general path", {
  p <- c(2, 1, 0.5, 1, 3, 1)
  expect_length(aDist(X[1, , drop = FALSE], weights = p), 0)
  expect_length(aDist(X[1, , drop = FALSE], na.rm = TRUE), 0)
  expect_length(aDist(X[1, ], cellweights = matrix(1, 1, 6)), 0)
  expect_s3_class(aDist(c(1, NA, 3, 4), na.rm = TRUE), "dist")
  # general path (cell weights): all pairs against the definition
  W <- matrix(runif(60), 10, 6)
  m <- as.matrix(aDist(X, cellweights = W, weights = p))
  expect_equal(dimnames(m), list(letters[1:10], letters[1:10]))
  for (i in c(1, 4, 9)) for (j in c(2, 3, 10))
    expect_equal(m[i, j], bf_dist(X[i, ], X[j, ], p, W[i, ] * W[j, ]))
  # named vectors give a plain number, one-row data frames a 1 x 1 matrix
  expect_null(dim(aDist(c(a = 1, b = 2, c = 3), c(a = 2, b = 1, c = 1))))
})

## ---- aDistDens ------------------------------------------------------------------

tt <- seq(-4.7, 4.7, length = 1000)
st <- diff(tt[1:2])
f1 <- dnorm(tt, 0, 1.5); f2 <- dnorm(tt, 1, 1.5)

test_that("aDistDens is the L2 distance of the clr transforms", {
  d <- aDistDens(tt, st, f1, f2)
  expect_equal(d, sqrt(trapzc(st, (fcenLR(tt, st, f1) - fcenLR(tt, st, f2))^2)))
  # analytic: clr difference of two normals with equal sd is linear in t
  expect_equal(d, sqrt((2 * 4.7)^3 / 12) / 1.5^2, tolerance = 1e-5)
  expect_equal(aDistDens(tt, st, 7 * f1, f2), d)
  expect_equal(aDistDens(tt, st, f1, f1), 0)
})

test_that("aDistDens with a reference measure matches fcenLRp", {
  p <- dnorm(tt, sd = 2) / trapzc(st, dnorm(tt, sd = 2))
  d <- aDistDens(tt, st, f1 / p, f2 / p, p = p)
  expect_equal(d, sqrt(trapzc(st, (fcenLRp(tt, st, f1 / p, p) -
                                     fcenLRp(tt, st, f2 / p, p))^2 * p)))
})

test_that("aDistDens handles several densities", {
  dens <- t(sapply(c(0, 0.5, 1, 2), function(m) dnorm(tt, mean = m, sd = 1.5)))
  dm <- aDistDens(tt, st, dens)
  expect_s3_class(dm, "dist")
  expect_equal(as.matrix(dm)[1, 3], aDistDens(tt, st, f1, f2))
  expect_equal(aDistDens(tt, st, dens, dens[4:1, ]),
               c(as.matrix(dm)[1, 4], as.matrix(dm)[2, 3], as.matrix(dm)[3, 2],
                 as.matrix(dm)[4, 1]))
  expect_equal(dim(aDistDens(tt, st, dens, dens[1:2, ], type = "cross")), c(4L, 2L))
  expect_error(aDistDens(tt^3, st, f1, f2), "equidistant")
  expect_error(aDistDens(tt, st, replace(f1, 1, 0), f2), "strictly positive")
  expect_error(aDistDens(tt, st, f1[-1], f2), "one value per grid point")
})

test_that("aDistDens with a single density returns an empty dist", {
  expect_length(aDistDens(tt, st, f1), 0)
  expect_s3_class(aDistDens(tt, st, f1), "dist")
})
