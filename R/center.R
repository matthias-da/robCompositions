library(robCompositions)
data(expenditures)
x <- expenditures
## still in original space reported:
apply(x, 2, gm)
## this is equvalent to
means <- matrix(apply(cenLR(x)$x.clr, 2, method), nrow=1)
gms <- apply(x, 2, gm)
fac <- means/gms
means /fac
## but robust we need any ilr, but then to preserve the original scale is not possilbe/easy, as described at the previous mail ;)
isomLRinv(matrix(covMcd(isomLR(x))$center, nrow=1))
# compare
apply(x, 2, gm)
## Karels suggestion to take only obs that considered finally in MCD
mcd <- covMcd(isomLR(x))
gms_rob <- apply(x[mcd$best, ], 2, gm)
## finished?
gms_rob
## (the factor (fac) cannot be used for ilr transformed, since its length is longer (+1))


centerAndVar <- function(x, method = mean.default){
  z <- cenLR(x)
  means <- matrix(apply(z$x.clr, 2, method), nrow=1)
  gm <- apply(x, 2, gm)
  fac <- means/gm
  means / fac
  class(means) <- "clr"
  cenLRinv(means)
}