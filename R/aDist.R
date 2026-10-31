#' Aitchison distance
#' 
#' Computes the Aitchison distance between two compositions, between the
#' corresponding rows of two data sets, between all rows of two data sets, or
#' within the observations of one data set. Parts can be weighted (weighted
#' Aitchison geometry), and missing values, zeros and cell weights (e.g. from a
#' cellwise-robust estimator) can be taken into account via the available
#' pairwise log-ratios.
#' 
#' @details
#' The Aitchison distance between two compositions \eqn{x} and \eqn{y} with
#' \eqn{D} parts is the Euclidean distance of their clr coordinates,
#' \deqn{d_a(x, y)^2 = \sum_{j=1}^D \left(\mathrm{clr}(x)_j - \mathrm{clr}(y)_j\right)^2
#'   = \frac{1}{D}\sum_{j<k}\left(\ln\frac{x_j}{x_k} - \ln\frac{y_j}{y_k}\right)^2.}{%
#'   d_a(x, y)^2 = sum_j (clr(x)_j - clr(y)_j)^2 = 1/D * sum_{j<k} (ln(x_j/x_k) - ln(y_j/y_k))^2.}
#' It accounts for the relative scale of compositional data: it does not change
#' if a composition is multiplied by a positive constant.
#' 
#' \strong{Output types.} If \code{y} is \code{NULL}, the distance matrix
#' between all observations of \code{x} is returned as a \code{"dist"} object
#' and \code{type} is ignored. Otherwise \code{type} selects the output:
#' \describe{
#'   \item{\code{"sum"}}{(default) the sum of the distances between the
#'     corresponding rows of \code{x} and \code{y}; for two single
#'     compositions this is simply their distance.}
#'   \item{\code{"rows"}}{the vector of distances between the corresponding
#'     rows of \code{x} and \code{y}, e.g. to see which observations were imputed
#'     badly.}
#'   \item{\code{"cross"}}{the \eqn{n \times m}{n x m} matrix of distances
#'     between every row of \code{x} and every row of \code{y}; \code{y} may have
#'     any number of rows, e.g. to find nearest neighbours of new observations in
#'     a reference data set.}
#' }
#' For \code{"sum"} and \code{"rows"}, \code{x} and \code{y} must have the same
#' dimensions; for \code{"cross"} the same number of columns.
#' 
#' \strong{Weighted parts.} With \code{weights} \eqn{p = (p_1, \dots, p_D)},
#' the parts get different importance through a change of the reference measure
#' of the simplex (Egozcue and Pawlowsky-Glahn, 2016; Hron et al., 2022):
#' \deqn{d_p(x, y)^2 = \sum_{j=1}^D p_j \left(u_j - \bar u_p\right)^2,
#'   \qquad u = \ln x - \ln y, \quad \bar u_p = \frac{\sum_j p_j u_j}{\sum_j p_j},}{%
#'   d_p(x, y)^2 = sum_j p_j (u_j - ubar_p)^2, u = ln x - ln y, ubar_p = sum_j p_j u_j / sum_j p_j,}
#' i.e. the weighted Euclidean distance of the weighted clr coordinates. The
#' weights are not normalised: \eqn{p = (1, \dots, 1)} gives the ordinary
#' Aitchison distance, and multiplying all weights by \eqn{c} multiplies the
#' distance by \eqn{\sqrt{c}}. Reducing weights never increases the distance
#' (Egozcue and Pawlowsky-Glahn, 2016), so down-weighting a part acts like a
#' soft version of dropping it from the composition. This is the discrete
#' counterpart of \code{\link{fcenLRp}} for densities.
#' 
#' \strong{Missing values, zeros and cell weights.} By default all values must be
#' strictly positive and finite. With \code{na.rm = TRUE}, cells that are missing,
#' zero or negative are left out. With \code{cellweights}, each cell gets a weight
#' \eqn{w \ge 0}, for example a weight from a cellwise-robust method that
#' down-weights outlying cells. For a comparison of two rows the parts receive
#' the weights \eqn{v_j = w^x_j w^y_j} (0 for a cell left out), each log-ratio
#' between parts \eqn{j} and \eqn{k} the weight \eqn{v_j v_k}, and the sum of
#' squared log-ratio differences is estimated by the weighted mean over the
#' available log-ratios times the number of all \eqn{D(D-1)/2} log-ratios,
#' as in Gower's distance for incomplete data (Gower, 1971). Without part
#' weights, and with \eqn{u = \ln x - \ln y} and \eqn{V = \sum_j v_j}, this gives
#' \deqn{d^2 = (D-1)\,\frac{V \sum_j v_j (u_j - \bar u_v)^2}{V^2 - \sum_j v_j^2},
#'   \qquad \bar u_v = \frac{1}{V}\sum_j v_j u_j.}{%
#'   d^2 = (D-1) * V * sum_j v_j (u_j - ubar)^2 / (V^2 - sum_j v_j^2), ubar = sum_j v_j u_j / V.}
#' With all weights equal to one this is exactly the Aitchison distance; with
#' \eqn{a} available parts and the others left out it is
#' \eqn{\frac{D-1}{a-1}\sum_{j \in A}(u_j - \bar u_A)^2}{(D-1)/(a-1) * sum_{j in A} (u_j - ubar_A)^2},
#' the Aitchison distance of the available subcomposition scaled up to \eqn{D}
#' parts. If fewer than two parts carry weight, the distance is \code{NA}.
#' Together with part weights \eqn{p}, the log-ratio weights are
#' \eqn{p_j v_j p_k v_k} and the estimate is scaled to the weighted distance
#' \eqn{d_p}; with all cell weights equal to one it is exactly \eqn{d_p}.
#' 
#' @aliases aDist iprod
#' @param x a vector (one composition), matrix or data frame with the
#'   compositions in the rows.
#' @param y \code{NULL}, or a vector, matrix or data frame of compositions to
#'   compare with \code{x}; see Details for the required dimensions.
#' @param type character, one of \code{"sum"} (default), \code{"rows"} or
#'   \code{"cross"}; see Details. Ignored if \code{y} is \code{NULL}.
#' @param weights optional vector of \eqn{D} positive part weights for the
#'   weighted Aitchison distance; see Details. \code{NULL} (default) means equal
#'   weights of one.
#' @param na.rm logical. If \code{TRUE}, missing, zero and negative values are
#'   left out and the distance is computed from the remaining log-ratios. If
#'   \code{FALSE} (default), such values give an error.
#' @param cellweights optional non-negative cell weights: a matrix with the
#'   dimensions of \code{x}, which then also applies to \code{y} if \code{y} is
#'   \code{NULL}, or a list with elements \code{x} and \code{y} giving weight
#'   matrices for both data sets (either may be \code{NULL} for weights of one).
#' @return For \code{y = NULL}, an object of class \code{"dist"}. Otherwise,
#'   depending on \code{type}: a single number (\code{"sum"}; for two named
#'   single compositions or two one-row data frames a 1 x 1 matrix with their
#'   names as dimnames), a vector with one distance per row (\code{"rows"}), or
#'   a matrix with \code{nrow(x)} rows and \code{nrow(y)} columns
#'   (\code{"cross"}). Distances are \code{NA} where fewer than two parts are
#'   available.
#' @author Matthias Templ, Bernhard Meindl
#' @export
#' @seealso \code{\link{cenLR}}, \code{\link{pivotCoord}}, \code{\link{ced}},
#'   \code{\link{aDistDens}}, \code{\link{cellPcaCoDa}}
#' @references Aitchison, J. (1986) \emph{The Statistical Analysis of
#' Compositional Data} Monographs on Statistics and Applied Probability.
#' Chapman and Hall Ltd., London (UK). 416p.
#' 
#' Aitchison, J. and Barcelo-Vidal, C. and Martin-Fernandez, J.A. and
#' Pawlowsky-Glahn, V. (2000) Logratio analysis and compositional distance.
#' \emph{Mathematical Geology}, \bold{32}, 271-275.
#' 
#' Egozcue, J.J. and Pawlowsky-Glahn, V. (2016) Changing the reference measure
#' in the simplex and its weighting effects. \emph{Austrian Journal of
#' Statistics}, \bold{45} (4), 25-44. \doi{10.17713/ajs.v45i4.126}
#' 
#' Gower, J.C. (1971) A general coefficient of similarity and some of its
#' properties. \emph{Biometrics}, \bold{27} (4). \doi{10.2307/2528823}
#' 
#' Hron, K. and Templ, M. and Filzmoser, P. (2010) Imputation of missing values
#' for compositional data using classical and robust methods.
#' \emph{Computational Statistics and Data Analysis}, \bold{54} (12),
#' 3095-3107. \doi{10.1016/j.csda.2009.11.023}
#' 
#' Hron, K., Menafoglio, A., Palarea-Albaladejo, J., Filzmoser, P., Talska, R.
#' and Egozcue, J.J. (2022) Weighting of parts in compositional data analysis:
#' advances and applications. \emph{Mathematical Geosciences}, \bold{54} (1),
#' 71-93. \doi{10.1007/s11004-021-09952-y}
#' @keywords math arith
#' @examples
#' data(expenditures)
#' x <- xOrig <- expenditures
#' ## Aitchison distance between two observations:
#' aDist(x[1, ], x[2, ])
#' aDist(as.numeric(x[1, ]), as.numeric(x[2, ]))
#' ## distance matrix of x:
#' aDist(x)
#' 
#' ## distance between an original and an imputed data set:
#' x[1,3] <- x[3,5] <- x[2,4] <- x[5,3] <- x[8,3] <- NA
#' xImp <- impCoda(x, method="ltsReg")$xImp
#' aDist(xOrig, xImp)
#' ## ... and per observation: which rows were imputed worst?
#' round(aDist(xOrig, xImp, type = "rows"), 3)
#' 
#' ## distances of three new observations to all observations of a data set
#' aDist(xOrig[1:3, ] * 1.1, xOrig, type = "cross")[, 1:6]
#' 
#' ## weighted Aitchison distance: give the first part (housing) double weight
#' aDist(xOrig[1, ], xOrig[2, ], weights = c(2, 1, 1, 1, 1))
#' 
#' ## incomplete data: use the available log-ratios
#' aDist(x[1:4, ], na.rm = TRUE)
#' 
#' ## down-weight a suspicious cell (weight 0 = leave it out)
#' w <- matrix(1, nrow(xOrig), ncol(xOrig))
#' w[1, 2] <- 0
#' aDist(xOrig[1:2, ], xImp[1:2, ], type = "rows", cellweights = w[1:2, ])
aDist <- function(x, y = NULL, type = c("sum", "rows", "cross"),
                  weights = NULL, na.rm = FALSE, cellweights = NULL) {
  type <- match.arg(type)
  stopifnot(is.logical(na.rm), length(na.rm) == 1L, !is.na(na.rm))
  
  # two single compositions (named vectors or one-row data frames) keep their
  # names as dimnames of a 1 x 1 result (type = "sum" only)
  labels <- NULL
  if (!is.null(y) && type == "sum") {
    if (is.vector(x) && is.vector(y)) {
      labels <- list(names(x), names(y))
    } else if (is.data.frame(x) && is.data.frame(y)) {
      labels <- list(rownames(x), rownames(y))
    }
  }
  
  x <- .adist_as_matrix(x, "x")
  if (is.null(cellweights) || !is.list(cellweights)) {
    wx <- cellweights
    wy <- if (is.null(y)) cellweights else NULL
  } else {
    wx <- cellweights$x
    wy <- cellweights$y
  }
  wx <- .adist_weights(x, wx, na.rm, "x")
  if (!is.null(weights)) {
    if (!is.numeric(weights) || length(weights) != ncol(x) ||
        any(!is.finite(weights)) || any(weights <= 0))
      stop("'weights' must be a vector of ", ncol(x), " positive numbers")
    weights <- as.numeric(weights)
  }
  
  if (is.null(y)) {
    if (is.null(wx) && is.null(weights)) {
      return(dist(cenLR(x)$x.clr))
    }
    lx <- .adist_log(x, wx)
    d <- vapply(seq_len(nrow(x)), function(j)
      .adist_core(lx, lx[rep(j, nrow(x)), , drop = FALSE],
                  wx, if (!is.null(wx)) wx[rep(j, nrow(x)), , drop = FALSE],
                  weights),
      numeric(nrow(x)))
    dimnames(d) <- list(rownames(x), rownames(x))
    return(as.dist(d))
  }
  
  y <- .adist_as_matrix(y, "y")
  if (type == "cross") {
    if (ncol(x) != ncol(y))
      stop("'x' and 'y' must have the same number of columns")
  } else if (!identical(dim(x), dim(y))) {
    stop("'x' and 'y' must have the same dimensions")
  }
  wy <- .adist_weights(y, wy, na.rm, "y")
  lx <- .adist_log(x, wx)
  ly <- .adist_log(y, wy)
  
  if (type == "cross") {
    n <- nrow(x)
    d <- vapply(seq_len(nrow(y)), function(j)
      .adist_core(lx, ly[rep(j, n), , drop = FALSE],
                  wx, if (!is.null(wy)) wy[rep(j, n), , drop = FALSE], weights),
      numeric(n))
    d <- matrix(d, nrow = n, ncol = nrow(y),
                dimnames = list(rownames(x), rownames(y)))
    return(d)
  }
  
  d <- .adist_core(lx, ly, wx, wy, weights)
  if (type == "rows") {
    names(d) <- rownames(x)
    return(d)
  }
  out <- sum(d)
  if (identical(lengths(labels), c(1L, 1L))) {
    out <- matrix(out, nrow = 1, ncol = 1, dimnames = labels)
  }
  out
}

## ---- internal helpers of aDist() ---------------------------------------------

#' Coerce a composition or a data set to a numeric matrix (rows = compositions)
#' @noRd
.adist_as_matrix <- function(x, arg) {
  if (is.vector(x) && !is.list(x)) {
    x <- matrix(x, nrow = 1, dimnames = list(NULL, names(x)))
  }
  x <- as.matrix(x)
  if (!is.numeric(x)) stop("'", arg, "' must be numeric")
  storage.mode(x) <- "double"
  x
}

#' Cell weights of a data set: NULL if every cell has weight 1, otherwise a
#' matrix of non-negative weights with 0 for cells left out (na.rm = TRUE)
#' @noRd
.adist_weights <- function(x, w, na.rm, arg) {
  ok <- is.finite(x) & x > 0
  if (!na.rm && !all(ok)) {
    stop("'", arg, "' contains missing, zero, negative or infinite values; ",
         "use na.rm = TRUE to compute the distance from the available parts")
  }
  if (!is.null(w)) {
    w <- as.matrix(w)
    if (!identical(dim(w), dim(x)))
      stop("the cell weights for '", arg, "' must have the same dimensions as '",
           arg, "'")
    if (!is.numeric(w) || any(!is.finite(w)) || any(w < 0))
      stop("cell weights must be finite and non-negative")
    w <- w * ok
  } else if (!all(ok)) {
    w <- ok * 1
  }
  w
}

#' Logarithms of a data set, with 0 for cells that carry no weight
#' @noRd
.adist_log <- function(x, w) {
  l <- suppressWarnings(log(x))
  if (!is.null(w)) l[w == 0] <- 0
  l
}

#' Row-wise (weighted) Aitchison distances between log-compositions lx and ly
#' (same dimensions), with optional cell weights wx, wy and part weights p
#' (see Details of aDist)
#' @noRd
.adist_core <- function(lx, ly, wx = NULL, wy = NULL, p = NULL) {
  u <- lx - ly
  if (is.null(wx) && is.null(wy)) {
    if (is.null(p)) return(sqrt(rowSums((u - rowMeans(u))^2)))
    ubar <- drop(u %*% p) / sum(p)
    return(sqrt(drop((u - ubar)^2 %*% p)))
  }
  v <- if (is.null(wy)) wx else if (is.null(wx)) wy else wx * wy
  if (is.null(p)) p <- rep(1, ncol(u))
  q <- sweep(v, 2, p, "*")
  Sq <- rowSums(q)
  ubar <- rowSums(q * u) / Sq
  Q <- rowSums(q * (u - ubar)^2)
  denom <- Sq^2 - rowSums(q^2)
  d2 <- (sum(p)^2 - sum(p^2)) / sum(p) * Sq * Q / denom
  d2[!(denom > 0)] <- NA_real_
  sqrt(pmax(d2, 0))
}

#' @rdname aDist
#' @export
#' @examples 
#' data("expenditures") 
#' aDist(expenditures)  
#' x <- expenditures[, 1]
#' y <- expenditures[, 2]
#' aDist(x, y)
#' aDist(expenditures, expenditures)
iprod <- function(x, y){
  warning("wrong formula, has to be fixed.")
  D <- length(x)
  if(D != length(y)) stop("x and y should have the same length")
  ip <- 1 / D * sum(log(as.numeric(x[1:(D-1)]) / as.numeric(x[2:D])) * 
                      log(as.numeric(y[1:(D-1)]) / as.numeric(y[2:D])))
  return(ip)
}
	
