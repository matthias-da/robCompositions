{robCompositions}
=======================================

Robust Methods for Compositional Data

    using robCompositions

data(expenditures)

p1 <- pcaCoDa(expenditures)

plot(p1)

![Image](https://f.cloud.github.com/assets/5263685/2436199/8ef8f856-add7-11e3-84d7-88fdac4fdbe3.png?raw=true)


What is it?
-----------

-   Imputation of compositional data including robust methods, methods to impute rounded zeros
-   Outlier detection for compositional data using robust methods 
-   Principal component analysis for compositional data using robust methods
-   Factor analysis for compositional data using robust methods 
-   Discriminant analysis for compositional data (Fisher rule) using robust methods
-   Robust regression with compositional predictors 
-   Anderson-Darling normality tests for compositional data
-   log-ratio transformations (addLR, cenLR, isomLR, and their inverse transformations).
-   In addition, visualisation and diagnostic tools are implemented as well as high and low-level plot functions for the 
             ternary diagram. 


Goals
-----

-   never use classical statistical methods on raw compositional data again.


Getting Started
---------------

### Dependencies

The package has dependencies on 

	R (>= 2.10), utils, robustbase, rrcov, car (>= 2.0-0), MASS, pls



### Installation

Installion of `robCompositions` is really easy for registered users (when the R-tools are installed). Just use 

    library(devtools)
    install_github("robCompositions", "matthias-da")


Examples
--------

#### k nearest neighbor imputation
data(expenditures)

expenditures[1,3]

expenditures[1,3] <- NA

impKNNa(expenditures)$xImp[1,3]

#### iterative model based imputation
data(expenditures)

x <- expenditures

x[1,3]

x[1,3] <- NA

xi <- impCoda(x)$xImp

xi[1,3]

s1 <- sum(x[1,-3])

impS <- sum(xi[1,-3])

xi[,3] * s1/impS


xi <- impKNNa(expenditures)

xi

summary(xi)

plot(xi, which=1)

plot(xi, which=2)

plot(xi, which=3)

#### pca
data(expenditures)

p1 <- pcaCoDa(expenditures)

p1

plot(p1)

#### outlier detection
data(expenditures)

oD <- outCoDa(expenditures)

oD

plot(oD)

#### transformations
data(arcticLake)

x <- arcticLake

x.alr <- addLR(x, 2)

y <- addLRinv(x.alr)

addLRinv(addLR(x, 3))

data(expenditures)

x <- expenditures

y <- addLRinv(addLR(x, 5))

head(x)

head(y)

addLRinv(x.alr, ivar=2, useClassInfo=FALSE)


data(expenditures)

eclr <- cenLR(expenditures)

inveclr <- cenLRinv(eclr)

head(expenditures)

head(inveclr)

head(cenLRinv(eclr$x.clr))


require(MASS)

Sigma <- matrix(c(5.05,4.95,4.95,5.05), ncol=2, byrow=TRUE)

z <- isomLRinv(mvrnorm(100, mu=c(0,2), Sigma=Sigma))
