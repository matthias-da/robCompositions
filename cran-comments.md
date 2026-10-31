## Submission of robCompositions 2.6.1

This is a maintenance release:

* `src/zeros.cpp` now includes `<cassert>` explicitly, needed to compile
  against Eigen 5.0.1 (the upcoming RcppEigen release); patch contributed by
  Dirk Eddelbuettel.
* `trapzc()` gave a wrong result for a two-point grid; results for three or
  more points are unchanged.
* Rewritten help pages for the functional clr transformations (`fcenLR()`
  family, `trapzc()`), plus new tests.
* `aDist(x, y)` (and `ced()`, which calls it) now checks that `x` and `y`
  have the same dimensions. Previously a `y` with fewer rows made the C code
  read beyond the end of the array. The `.C()` registration of this routine
  was corrected from 6 to 5 arguments (a misspelled `NAOK` argument had been
  passed through). Results for valid input are unchanged.

## Test environments

* local: macOS 26 (aarch64-apple-darwin20), R 4.5.2

## R CMD check results

0 errors | 0 warnings | 1 note

* The NOTE flags https://www.eupedia.com/europe/european_y-dna_haplogroups.shtml
  (data source in `?haplogroups`) as "403 Forbidden". The URL is valid and
  opens normally in a browser (HTTP 200); the site rejects automated requests
  from the URL checker.

## Downstream dependencies

The reverse dependencies (`baker`, `CMMs`, `classmap`) are unaffected: they do
not call `trapzc()`, the `fcenLR()` family or `ced()`, and none of them calls
`aDist()` (`baker` imports only `cenLR()`; its single `aDist()` reference is
commented out). The header include in compiled code changes no R-level behaviour.
