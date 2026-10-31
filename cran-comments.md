## Submission of robCompositions 2.7.0

Minor release with new functionality for the Aitchison distance. A 2.6.1
maintenance release was prepared but never submitted, so its fixes are included
(see NEWS):

* `src/zeros.cpp` now includes `<cassert>` explicitly, needed to compile
  against Eigen 5.0.1 (the upcoming RcppEigen release); patch contributed by
  Dirk Eddelbuettel.
* `aDist()` gains row-wise and cross distances, weighted parts (weighted
  Aitchison geometry), and missing values/zeros/cell weights via the available
  pairwise log-ratios. It is now computed in R from clr coordinates; the only
  `.C()` routine (`src/robCompositions.c`) was removed. Results for the
  default arguments are unchanged; `aDist(x)` for one data set is still
  `dist(cenLR(x)$x.clr)`, including for incomplete data.
* New function `aDistDens()`: Aitchison distance between densities in the
  Bayes space.
* `aDist(x, y)` checks that `x` and `y` have the same dimensions (before, a
  `y` with fewer rows made the C code read beyond the end of the array).
* `trapzc()` gave a wrong result for a two-point grid.
* Rewritten help pages for `aDist()`, `ced()`, the `fcenLR()` family and
  `trapzc()`; new tests.
* Two outdated URLs in the imputation vignette (a retired page of the Swiss
  Federal Statistical Office and a `dx.doi.org` link) were removed.

## Test environments

* local: macOS 26 (aarch64-apple-darwin20), R 4.5.2

## R CMD check results

0 errors | 0 warnings | 1 note

* The NOTE flags https://www.eupedia.com/europe/european_y-dna_haplogroups.shtml
  (data source in `?haplogroups`) as "403 Forbidden". The URL is valid and
  opens normally in a browser (HTTP 200); the site rejects automated requests
  from the URL checker.

## Downstream dependencies

There are 5 reverse dependencies: `baker`, `classmap`, `CMMs`, `deepImp` and
`riskutility`.

* `classmap`, `CMMs`, `deepImp` and `riskutility` were checked with
  `R CMD check` against this version: no new problems.
* `baker` could not be checked locally (it needs JAGS via `R2jags`). It imports
  only `cenLR()`, which is unchanged.
* `deepImp` and `riskutility` call `aDist()` with the default arguments. For
  data with zero or negative values, `aDist()` returns `NaN` as before, now
  with a warning, so the example of `riskutility::ait()` on normally
  distributed data still runs.
