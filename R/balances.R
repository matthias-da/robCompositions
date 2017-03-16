#' Balance calculation
#'
#' Given a D-dimensional compositional data set and a sequential binary partition,
#' the function bal calculates the balances in order to express the given data
#' in the (D-1)-dimensional real space.
#'
#'
#' @param x data frame or matrix, typically compositional data
#' @param y binary partition
#' @details The sequential binary partition constructs an orthonormal basis in the (D-1)-dimensional hyperplane
#' in real space.
#' @export
#' @author Veronika Pintar, Karel Hron, Matthias Templ
#' @rdname balances
#' @return \item{balances}{The balances represent orthonormal coordinates which allow an interpretation in sense of groups of compositional parts.
#'                         Output is a matrix, the D-1 colums contain balance coordinates of the observations in the rows.}
#'         \item{V}{A Dx(D-1) contrast matrix associated with the orthonormal basis, corresponding to the sequential binary partition (in clr coordinates).}
#' @references (Egozcue, J.J., Pawlowsky-Glahn, V. (2005) Groups of parts and their balances in compositional data analysis. Mathematical Geology, 37 (7), 795???828.)
#' @examples
#' data(expenditures, package = "robCompositions")
#' y1 <- data.frame(c(1,1,1,-1,-1),c(1,-1,-1,0,0),
#'                  c(0,+1,-1,0,0),c(0,0,0,+1,-1))
#' y2 <- data.frame(c(1,-1,1,-1,-1),c(1,0,-1,0,0),
#'                  c(1,-1,1,-1,1),c(0,-1,0,1,0))
#' y3 <- data.frame(c(1,1,1,1,-1),c(-1,-1,-1,+1,0),
#'                  c(-1,-1,+1,0,0),c(-1,1,0,0,0))
#' y4 <- data.frame(c(1,1,1,-1,-1),c(0,0,0,-1,1),
#'                  c(-1,-1,+1,0,0),c(-1,1,0,0,0))
#' y5 <- data.frame(c(1,1,1,-1,-1),c(-1,-1,+1,0,0),
#'                  c(0,0,0,-1,1),c(-1,1,0,0,0))
#' b1 <- balances(expenditures, y1)
#' b2 <- balances(expenditures, y5)
#' b1$balances
#' b2$balances
#'
#' data(machineOperators)
#' sbp <- data.frame(c(1,1,-1,-1),c(-1,+1,0,0),
#'                  c(0,0,+1,-1))
#' balances(machineOperators, sbp)
#'
balances <- function(x, y) {
  #function to check if partition matrix is valid
  validate <- function(y) {

  #check matrix size and entries
  if (any(dim(y) != c(dim(x)[2],dim(x)[2] - 1)) ||
      any(abs(y) > 1))
    stop("Size of partition matrix does not match or invalid entry!")
  
  act <- 1:nrow(y)
  for (i in 1:ncol(y)) {
    # find col with active variables nonzero
    if (any(y[act,i] == 0)) {
      tmp <- which(apply(y[act,], 2,function(x)
        all(x != 0)))
      tmp <- tmp[tmp > i]
      # error if no or more than one col exist
      if (length(tmp) != 1)
        stop("Binary Partition not valid!")
      # sort binary partition colwise
      y[, c(i, tmp)] <- y[, c(tmp, i)]
    }
    
    #Error if all entries same or not active variable nonzero
    if (length(unique((y[act, i]))) == 1 ||
        any(y[-act, i] != 0))
      stop("Binary Partition not valid!")
    
    #sort binary partition rowwise
    y[act,] <- y[act[order(y[act, i], decreasing = TRUE)],]
    
    # find active variables in current column:
    count <- 0
    act <- 1
    for (j in 2:nrow(y)) {
      # if entry equal -> add as active variable
      if (all(y[(j - 1), 1:i] == y[j, 1:i])) {
        count <- count + 1
        act <- c(act, j)
        # if entry not equal and count == 0 -> take this variable active
      } else if (count == 0)
        act <- j
      # if entry not equal and count != 0 -> all remaining variables not active
      else
        break
    }
  }
  return(TRUE)
}

# Create base from given partition matrix
createv <- function(x) {
  for (i in 1:ncol(x)) {
    #check group membership
    p_ind <- which(x[,i] > 0)
    m_ind <- which(x[,i] < 0)
    #determine m_k and p_k
    p <- length(p_ind)
    m <- length(m_ind)
    #calculate matrix V
    x[p_ind, i] <- 1 / p * sqrt((m * p) / (m + p))
    x[m_ind, i] <- (-1 / m * sqrt((m * p) / (m + p)))
  }
  colnames(x) <- paste("v", 1:ncol(x))
  return(x)
}

#Calculate balances with given function cenLR
if (validate(y)) {
  V <- as.matrix(createv(y))
  balance <- as.matrix(cenLR(x)$x.clr) %*% V
  colnames(balance) <- paste("z", 2:ncol(x) - 1)
}
res <- list(balances = balance, V = as.matrix(V))
return(res)
}