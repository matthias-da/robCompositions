#' cubeCoord
#' 
#' @name cubeCoord
#' @rdname cubeCoord
#' @importFrom tidyr unite
#' @importFrom tidyr spread
#' @importFrom graphics boxplot
#' @title Coordinate representation of a compositional cube and of a sample of compositional cubes
#' @aliases cubeCoord 
#' @aliases cubeCoordWrapper
#' @importFrom tidyr unite
#' @importFrom tidyr spread
#' @author Kamila Facevicova
#' @references Facevicova, K., Filzmoser, P. and K. Hron (2019) Compositional Cubes: Three-factorial Compositional Data. Under review.
#' @description cubeCoord computes a system of orthonormal coordinates of a compositional cube. 
#' Computation of either pivot coordinates or a coordinate system based on the given SBP is possible.
#' 
#' @param x a data frame containing variables representing row, column and slice factors of the respective compositional cube and variable with the values of the composition.
#' @param row.factor name of the variable representing the row factor. Needs to be stated with the quotation marks.
#' @param col.factor name of the variable representing the column factor. Needs to be stated with the quotation marks.
#' @param slice.factor name of the variable representing the slice factor. Needs to be stated with the quotation marks.
#' @param value name of the variable representing the values of the composition. Needs to be stated with the quotation marks.
#' @param SBPr an \eqn{I-1\times I} array defining the sequential binary partition of the values of the row factor, where I is the number of the row factor levels. The values assigned in the given step to the + group are marked by 1, values from  the - group by -1 and the rest by 0. If it is not provided, the pivot version of coordinates is constructed automatically.
#' @param SBPc an \eqn{J-1\times J} array defining the sequential binary partition of the values of the column factor, where J is the number of the column factor levels. The values assigned in the given step to the + group are marked by 1, values from  the - group by -1 and the rest by 0. If it is not provided, the pivot version of coordinates is constructed automatically.
#' @param SBPs an \eqn{K-1\times K} array defining the sequential binary partition of the values of the slice factor, where K is the number of the slice factor levels. The values assigned in the given step to the + group are marked by 1, values from  the - group by -1 and the rest by 0. If it is not provided, the pivot version of coordinates is constructed automatically.
#' @param pivot logical, default is FALSE. If TRUE, or one of the SBPs is not defined, its pivot version is used.
#' @param print.res logical, default is FALSE. If TRUE, the output is displayed in the Console.
#' @details This transformation moves the IJK-part compositional cubes from the simplex into a (IJK-1)-dimensional real space isometrically with respect to its three-factorial nature.
#' @keywords multivariate
#' @export
#' @seealso 
#' \code{\link{tabCoord}} 
#' \code{\link{tabCoordWrapper}} 
#' @return 
#' \item{Coordinates}{an array of orthonormal coordinates.} 
#' \item{Grap.rep}{graphical representation of the coordinates. 
#' Parts denoted by + form the groups in the numerator of the respective computational formula, 
#' parts - form the denominator and parts . are not involved in the given coordinate.} 
#' \item{Row.balances}{an array of row balances.}
#' \item{Column.balances}{an array of column balances.}
#' \item{Slice.balances}{an array of slice balances.}
#' \item{Row.column.OR}{an array of row-column OR coordinates.}
#' \item{Row.slice.OR}{an array of row-slice OR coordinates.}
#' \item{Column.slice.OR}{an array of column-slice OR coordinates.}
#' \item{Row.col.slice.OR}{an array of coordinates describing the mutual interaction between all three factors.}
#' \item{Contrast.matrix}{contrast matrix.}
#' \item{Log.ratios}{an array of pure log-ratios between groups of parts without the normalizing constant.}
#' \item{Coda.cube}{cube form of the given composition.}
#' \item{Bootstrap}{array of sample means, standard deviations and bootstrap confidence intervals.}
#' \item{Cubes}{Cube form of the given compositions.}
#' @examples 
#' ###################
#' ### Coordinate representation of a CoDa Cube
#' \dontrun{
#' ### example from Fa\v cevicov\'a (2019)
#' data(employment2)
#' CZE <- employment2[which(employment2$Country == 'CZE'), ]
#' 
#' # pivot coordinates
#' cubeCoord(CZE, "Sex", 'Contract', "Age", 'Value')
#' 
#' # coordinates with given SBP
#' 
#' r <- t(c(1,-1))
#' c <- t(c(1,-1))
#' s <- rbind(c(1,-1,-1), c(0,1,-1))
#' 
#' cubeCoord(CZE, "Sex", 'Contract', "Age", 'Value', r,c,s)
#' }
cubeCoord <- function(x, row.factor=NULL, col.factor=NULL, slice.factor=NULL, value=NULL, SBPr=NULL, SBPc=NULL, SBPs=NULL, pivot=FALSE, print.res=FALSE)
{                                                                             
  
  # Control and subsidiary parameters setting
  
  if(is.null(row.factor)) stop('Name of the row factor is not defined!')
  if(is.null(col.factor)) stop('Name of the column factor is not defined!')
  if(is.null(slice.factor)) stop('Name of the slice factor is not defined!')
  if(is.null(value)) stop('Name of the value variable is not defined!')
  
  x[,row.factor] <- as.factor(x[,row.factor])
  x[,col.factor] <- as.factor(x[,col.factor])
  x[,slice.factor] <- as.factor(x[,slice.factor])
  I <- nlevels(x[,row.factor]) # number of row factor levels
  J <- nlevels(x[,col.factor]) # number of column factor levels
  K <- nlevels(x[,slice.factor]) # number of slice factor levels
  y <- x[,c(row.factor, col.factor, slice.factor, value)]
  
  x_vec <- y[order(y[,1], y[,2], y[,3]),4] # vectorized cube according to Facevicova19
  
  if(!identical(as.numeric(table(x[,c(row.factor, col.factor)])),as.numeric(rep(K,I*J)))) stop('The CoDa Cube x is not defined properly, some values are missing!')
  if(!is.null(SBPr)&(nrow(SBPr)!= (I-1)||ncol(SBPr)!=I)) 
  {warning('The row SBP is not defined properly, pivot coordinates are used!')
    SBPr <- NULL}
  if(!is.null(SBPc)&(nrow(SBPc)!= (J-1)||ncol(SBPc)!=J)) 
  {warning('The column SBP is not defined properly, pivot coordinates are used!')
    SBPc <- NULL}
  if(!is.null(SBPs)&(nrow(SBPs)!= (K-1)||ncol(SBPs)!=K)) 
  {warning('The slice SBP is not defined properly, pivot coordinates are used!')
    SBPs <- NULL}
  
  ### Definition of pivot SBP (if necessary) 
  
  if(is.null(SBPr)||pivot==TRUE)
  {
    SBPr <- numeric()
    for(j in 1:(I-1))
    {
      novy <- c(rep(0,j-1), 1, rep(-1, I-j))
      SBPr <- rbind(SBPr, novy)
    }
    #print("SBP of row factor is not defined, its pivot version is used!")
  }
  rownames(SBPr) <- NULL
  
  
  if(is.null(SBPc)||pivot==TRUE)
  {
    SBPc <- numeric()
    for(j in 1:(J-1))
    {
      novy <- c(rep(0,j-1), 1, rep(-1, J-j))
      SBPc <- rbind(SBPc, novy)
    }
    #print("SBP of column factor is not defined, its pivot version is used!")
  }
  rownames(SBPc) <- NULL
  
  if(is.null(SBPs)||pivot==TRUE)
  {
    SBPs <- numeric()
    for(j in 1:(K-1))
    {
      novy <- c(rep(0,j-1), 1, rep(-1, K-j))
      SBPs <- rbind(SBPs, novy)
    }
    #print("SBP of slice factor is not defined, its pivot version is used!")
  }
  rownames(SBPs) <- NULL
  
  log_kontrasty <- function(x){
    r <- length(which(x==1))
    s <- length(which(x==-1))
    koef1 <- sqrt((r*s)/(r+s))*(1/r)
    koef2 <- -sqrt((r*s)/(r+s))*(1/s)
    log_kontrast <- rep(0, length(x))
    log_kontrast[which(x==1)] <- koef1
    log_kontrast[which(x==-1)] <- koef2
    return(log_kontrast)
  }
  
  norm_const_balance  <- function(x){ #based on vector of contrasts
    r <- length(which(x>0))
    s <- length(which(x<0))
    koef <- sqrt((r*s)/(r+s))
    return(koef)
  }
  
  norm_const_OR <- function(x){ #based on vector of contrasts
    kladne = table(x[which(x>0)])
    celkem = length(which(x!=0))
    if(dim(kladne)==2)
      koef=sqrt((kladne[1]*kladne[2])/celkem)
    else
      koef=sqrt((kladne*kladne/4)/celkem)
    names(koef) = NULL
    return(koef)
  }
  
  norm_const_ORR <- function(x,y,z){ #based on SBPr, SBPc, SBPs
    A = length(which(x>0))*length(which(y>0))*length(which(z>0))
    H = length(which(x<0))*length(which(y<0))*length(which(z<0))
    vse = length(which(x!=0))*length(which(y!=0))*length(which(z!=0))
    koef=sqrt(A*H/vse)
    return(koef)
  }
  
  ### expansion of SBP to the whole table
  
  SBPr_cele <- matrix(SBPr[,rep(c(1:I), each=J*K)], ncol=I*J*K)
  SBPc_cele <- matrix(SBPc[,rep(rep(c(1:J), each=K), I)], ncol=I*J*K)
  SBPs_cele <- matrix(SBPs[,rep(c(1:K),I*J)], ncol=I*J*K)
  
  ### Generating vectors of:
  
  # balances
  LCr <- t(apply(SBPr_cele, 1, FUN=log_kontrasty))
  LCc <- t(apply(SBPc_cele, 1, FUN=log_kontrasty))
  LCs <- t(apply(SBPs_cele, 1, FUN=log_kontrasty))
  
  # pairwise interaction coordinates
  OR_deleni_r_c <- NULL
  for(i in 1:(I-1))
  {
    for(j in 1:(J-1))
    {
      novy <- LCr[i,]*LCc[j,] 
      OR_deleni_r_c <- rbind(OR_deleni_r_c, novy)
    }
  }
  
  rownames(OR_deleni_r_c) <- NULL
  
  OR_deleni_r_s <- NULL
  for(i in 1:(I-1))
    
  {
    for(k in 1:(K-1))
    {
      novy <- LCr[i,]*LCs[k,] 
      OR_deleni_r_s <- rbind(OR_deleni_r_s, novy)
    }
  }
  
  rownames(OR_deleni_r_s) <- NULL
  
  OR_deleni_s_c <- NULL
  for(k in 1:(K-1))
  {
    for(j in 1:(J-1))
    {
      novy <- LCs[k,]*LCc[j,] 
      OR_deleni_s_c <- rbind(OR_deleni_s_c, novy)
    }
  }
  
  rownames(OR_deleni_s_c) <- NULL
  
  # full interaction coordinates
  
  OR_deleni_r_c_s <- NULL
  norm.constants.ORR <- NULL
  for(i in 1:(I-1))
  {
    for(j in 1:(J-1))
    {
      for(k in 1:(K-1))
      {
        novy <- LCr[i,]*LCc[j,]*LCs[k,] 
        OR_deleni_r_c_s <- rbind(OR_deleni_r_c_s, novy)
        
        const.nova <- norm_const_ORR(SBPr[i,],SBPc[j,],SBPs[k,])
        norm.constants.ORR <- c(norm.constants.ORR, const.nova)
      }
    }
  }
  
  rownames(OR_deleni_r_c_s) <- NULL
  
  OR_deleni <- rbind(OR_deleni_r_c, OR_deleni_r_s, OR_deleni_s_c, OR_deleni_r_c_s)
  
  ### matrix with generating vectors. Important for the back-transformation!
  
  normovani <- function(x){x/(norm(as.matrix(x), type="f"))}
  OR_contrasts <- t(apply(OR_deleni, 1, FUN=normovani))
  contrasts <- rbind(LCr, LCc, LCs, OR_contrasts)
  
  coord.names <- c(paste('z', 1:(I-1), '^r', sep=''), paste('z', 1:(J-1), '^c', sep=''), paste('z', 1:(K-1), '^s', sep=''),
                   paste('z', sort(outer(c(1:(I-1)),c(1:(J-1)), FUN=function(x,y)paste(x,y,sep=''))), '^rc', sep=''),
                   paste('z', sort(outer(c(1:(I-1)),c(1:(K-1)), FUN=function(x,y)paste(x,y,sep=''))), '^rs', sep=''),
                   paste('z', sort(outer(c(1:(J-1)),c(1:(K-1)), FUN=function(x,y)paste(x,y,sep=''))), '^cs', sep=''),
                   paste('z', sort(outer(outer(c(1:(I-1)),c(1:(J-1)), FUN=function(x,y)paste(x,y,sep='')),c(1:(K-1)), FUN=function(x,y)paste(x,y,sep=''))), '^rcs', sep=''))
  
  rownames(contrasts) <- coord.names
  colnames(contrasts) <- c(paste('x', sort(outer(outer(c(1:(I)),c(1:(J)), FUN=function(x,y)paste(x,y,sep='')),c(1:(K)), FUN=function(x,y)paste(x,y,sep=''))), sep=''))
  
  ### Coordinates
  
  souradnice <- contrasts%*%log(x_vec)
  rownames(souradnice) <- coord.names
  
  ### Pure log-ratios between groups of parts (without normalizing constant)
  norm.constants.balance = apply(contrasts[1:(I+J+K-3), ], 1, norm_const_balance)
  norm.constants.OR = apply(contrasts[(I+J+K-2):(I*J+I*K+K*J-I-J-K), ], 1, norm_const_OR)
  # norm. constants for ORR coordinates were already computed with these coordinates
  norm.constants = c(norm.constants.balance, norm.constants.OR, norm.constants.ORR)
  
  log.ratios = souradnice/norm.constants
  
  ### Table form of the CoDa table
  tab0 <- unite(y, slice.factor, col.factor, col='col_slice')
  tab <-  spread(tab0, 'col_slice', value)[,-1]
  colnames(tab) <- levels(as.factor(tab0[,'col_slice']))
  rownames(tab) <- levels(tab0[,row.factor])
  
  ### Graphical representation of groups within table:
  grap.rep <- list()
  permutation <- order(tab0[,1])
  for(i in 1:nrow(contrasts))
  {
    grap.r <- rep(".",ncol(contrasts))
    grap.r[which(contrasts[i,]>0)] <- "+"
    grap.r[which(contrasts[i,]<0)] <- "-"
    grap.r <- data.frame(tab0[permutation,c(1,2)], grap.r)
    grap.r.tab  <- spread(grap.r, 'col_slice', grap.r)[,-1]
    row.names(grap.r.tab) <- levels(tab0[,row.factor])
    grap.rep[[i]] <- grap.r.tab
  }
  
  names(grap.rep) <- coord.names
  
  ### Result:
  
  if(print.res==TRUE)
  {
    print("Row balances:")
    print(souradnice[c(1:(I-1))])
    print(grap.rep[c(1:(I-1))])
    
    print("Column balances:")
    print(souradnice[c(I:(I+J-2))])
    print(grap.rep[c(I:(I+J-2))])
    
    print("Slice balances:")
    print(souradnice[c((I+J-1):(I+J+K-3))])
    print(grap.rep[c((I+J-1):(I+J+K-3))])
    
    print("Row and Column odds ratio coordinates:")
    print(souradnice[c((I+J+K-2):(I*J+K-2))])
    print(grap.rep[c((I+J+K-2):(I*J+K-2))])
    
    print("Row and Slice odds ratio coordinates:")
    print(souradnice[c((I*J+K-1):(I*J+I*K-I-1))])
    print(grap.rep[c((I*J+K-1):(I*J+I*K-I-1))])
    
    print("Column and Slice odds ratio coordinates:")
    print(souradnice[c((I*J+I*K-I):(I*J+I*K+K*J-I-J-K))])
    print(grap.rep[c((I*J+I*K-I):(I*J+I*K+K*J-I-J-K))])
    
    print("Row, Column and Slice odds ratio coordinates:")
    print(souradnice[c((I*J+I*K+K*J-I-J-K+1):(I*J*K-1))])
    print(grap.rep[c((I*J+I*K+K*J-I-J-K+1):(I*J*K-1))])
  }
  
  result <- list("Coordinates"=souradnice, "Grap.rep" = grap.rep, "Row.balances"=souradnice[c(1:(I-1)),1],
                 "Column.balances"=souradnice[c(I:(I+J-2)),1], "Slice.balances"=souradnice[c((I+J-1):(I+J+K-3)),1],
                 "Row.column.OR"=souradnice[c((I+J+K-2):(I*J+K-2)),1], "Row.slice.OR"=souradnice[c((I*J+K-1):(I*J+I*K-I-1)),1],
                 "Column.slice.OR"=souradnice[c((I*J+I*K-I):(I*J+I*K+K*J-I-J-K)),1], "Row.col.slice.OR"=souradnice[c((I*J+I*K+K*J-I-J-K+1):(I*J*K-1)),1],
                 'Log.ratios'=log.ratios, "Contrast.matrix" = contrasts, 'Coda.cube'=tab)
  return(result)
  
}
#' @rdname cubeCoord
#' @param X a data frame containing variables representing row, column and slice factors 
#' of the respective compositional cubes, variable with the values 
#' of the composition and variable distinguishing the observations.
#' @param obs.ID name of the variable distinguishing the observations. Needs to be stated with the quotation marks.
#' @param test logical, default is FALSE. If TRUE, the bootstrap analysis of coordinates is provided.
#' @param n.boot number of bootstrap samples.
#' @description Wrapper (cubeCoordWrapper): For each compositional cube in the sample cubeCoordWrapper computes 
#' a system of orthonormal coordinates and provide a simple descriptive analysis. 
#' Computation of either pivot coordinates or a coordinate system based on the 
#' given SBP is possible.
#' @details Wrapper (cubeCoordWrapper): Each of n IJK-part compositional cubes from the sample is 
#' with respect to its three-factorial nature isometrically transformed 
#' from the simplex into a (IJK-1)-dimensional real space. 
#' Sample mean values and standard deviations are computed and using 
#' bootstrap an estimate of 95 \% confidence interval is given. 
#' @export
#' @examples 
#' 
#' ###################
#' ### Analysis of a sample of CoDa Cubes
#' \dontrun{
#' ### example from Fa\v cevicov\'a (2019)
#' data(employment2)
#' ### Compositional tables approach,
#' ### analysis of the relative structure.
#' ### An example from Facevi\v cov\'a (2019)
#' 
#' # pivot coordinates
#' cubeCoordWrapper(employment2, 'Country', 'Sex', 'Contract', 'Age', 'Value',  
#' test=TRUE)
#' 
#' # coordinates with given SBP (defined in the paper)
#' 
#' r <- t(c(1,-1))
#' c <- t(c(1,-1))
#' s <- rbind(c(1,-1,-1), c(0,1,-1))
#' 
#' res <- cubeCoordWrapper(employment2, 'Country', 'Sex', 'Contract', 
#' "Age", 'Value', r,c,s, test=TRUE)
#' 
#' ### Classical approach,
#' ### generalized linear mixed effect model.
#' 
#' library(lme4)
#' employment2$y <- round(employment2$Value*1000)
#' glmer(y~Sex*Age*Contract+(1|Country),data=employment2,family=poisson)
#' 
#' ### other relations within cube (in the log-ratio form)
#' ### e.g. ratio between women and man in the group FT, 15to24
#' ### and ratio between age groups 15to24 and 55plus
#' 
#' # transformation matrix
#' T <- rbind(c(1,rep(0,5), -1, rep(0,5)), c(rep(c(1/4,0,-1/4), 4)))
#' T %*% t(res$Contrast.matrix) %*%res$Bootstrap[,1]
#' }
cubeCoordWrapper <- function(X, obs.ID=NULL, row.factor=NULL, col.factor=NULL, slice.factor=NULL, 
                             value=NULL, SBPr=NULL, SBPc=NULL, SBPs=NULL, pivot=FALSE, 
                             test=FALSE, n.boot=1000){
  
  # Control and subsidiary parameters setting
  if(is.null(obs.ID)) stop('Name of the observation ID variable is not defined!')
  if(is.null(row.factor)) stop('Name of the row factor is not defined!')
  if(is.null(col.factor)) stop('Name of the column factor is not defined!')
  if(is.null(slice.factor)) stop('Name of the slice factor is not defined!')
  if(is.null(value)) stop('Name of the value variable is not defined!')
  
  
  X[,obs.ID] <- as.factor(X[,obs.ID])
  X[,row.factor] <- as.factor(X[,row.factor])
  X[,col.factor] <- as.factor(X[,col.factor])
  X[,slice.factor] <- as.factor(X[,slice.factor])
  
  N <- nlevels(X[,obs.ID])
  I <- nlevels(X[,row.factor]) # number of row factor levels
  J <- nlevels(X[,col.factor]) # number of column factor levels
  K <- nlevels(X[,slice.factor]) # number of slice factor levels
  
  
  if(!identical(as.numeric(table(X[,c(row.factor,obs.ID)])),as.numeric(rep(J*K,(I*N))))) stop('The CoDa Cubes are not defined properly, some values are missing!')
  if(!identical(as.numeric(table(X[,c(col.factor,obs.ID)])),as.numeric(rep(I*K,(J*N))))) stop('The CoDa Cubes are not defined properly, some values are missing!')
  if(!is.null(SBPr)&(nrow(SBPr)!= (I-1)||ncol(SBPr)!=I)) 
  {warning('The row SBP is not defined properly, pivot coordinates are used!')
    SBPr <- NULL}
  if(!is.null(SBPc)&(nrow(SBPc)!= (J-1)||ncol(SBPc)!=J)) 
  {warning('The column SBP is not defined properly, pivot coordinates are used!')
    SBPc <- NULL}
  if(!is.null(SBPs)&(nrow(SBPs)!= (K-1)||ncol(SBPs)!=K)) 
  {warning('The slice SBP is not defined properly, pivot coordinates are used!')
    SBPs <- NULL}
  
  
  Coordinates <- NULL
  Log.ratios <- NULL
  Row.balances <- NULL
  Column.balances <- NULL
  Slice.balances <- NULL
  Row.column.OR <- NULL
  Row.slice.OR <- NULL
  Column.slice.OR <- NULL
  Row.col.slice.OR <- NULL
  Tables <- array(NA, c(nlevels(X[,row.factor]), nlevels(X[,col.factor])*nlevels(X[,slice.factor]), N))
  
  for(i in 1:N)
  {
    obs <- which(X[,obs.ID]==levels(X[,obs.ID])[i])
    new <- cubeCoord(x=X[obs,], row.factor=row.factor, col.factor=col.factor, slice.factor=slice.factor, value=value, SBPr=SBPr, SBPc=SBPc, SBPs=SBPs, pivot=pivot, print.res=FALSE)
    Coordinates <- cbind(Coordinates, new$Coordinates)
    Log.ratios <- cbind(Log.ratios, new$Log.ratios)
    Row.balances <- cbind(Row.balances, new$Row.balances)
    Column.balances <- cbind(Column.balances, new$Column.balances)
    Slice.balances <- cbind(Slice.balances, new$Slice.balances)
    Row.column.OR <- cbind(Row.column.OR, new$Row.column.OR)
    Row.slice.OR <- cbind(Row.slice.OR, new$Row.slice.OR)
    Column.slice.OR <- cbind(Column.slice.OR, new$Column.slice.OR)
    Row.col.slice.OR <- cbind(Row.col.slice.OR, new$Row.col.slice.OR)
    Tables[,,i] <- as.matrix(new$Coda.cube)
    
  }
  Coordinates <- t(Coordinates)
  
  rownames(Coordinates) <- levels(X[,obs.ID])
  colnames(Log.ratios) <- levels(X[,obs.ID])
  colnames(Row.balances) <- levels(X[,obs.ID])
  colnames(Column.balances) <- levels(X[,obs.ID])
  colnames(Slice.balances) <- levels(X[,obs.ID])
  colnames(Row.column.OR) <- levels(X[,obs.ID])
  colnames(Row.slice.OR) <- levels(X[,obs.ID])
  colnames(Column.slice.OR) <- levels(X[,obs.ID])
  colnames(Row.col.slice.OR) <- levels(X[,obs.ID])
  dimnames(Tables)[[1]] <- levels(X[,row.factor])
  dimnames(Tables)[[2]] <- colnames(new$Grap.rep[[1]])
  dimnames(Tables)[[3]] <- levels(X[,obs.ID])
  
  res <- list('Coordinates'=Coordinates, 'Log.ratios'=t(Log.ratios), 'Row.balances'=t(Row.balances),
              'Column.balances'=t(Column.balances), 'Slice.balances'=t(Slice.balances),
              'Row.column.OR'=t(Row.column.OR), 'Row.slice.OR'=t(Row.slice.OR), 'Column.slice.OR'=t(Column.slice.OR),
              'Row.col.slice.OR'=t(Row.col.slice.OR),
              'Grap.rep'=new$Grap.rep, 'Contrast.matrix'=new$Contrast.matrix, 'Cubes'=Tables )
  
  if(test==TRUE)
  {
    # sample characteristics
    mean <- apply(Coordinates, 2, mean)
    sd <- apply(Coordinates, 2, sd)
    
    #set.seed(123)
    
    I <- nlevels(X[,row.factor]) # number of row factor values
    J <- nlevels(X[,col.factor]) # number of column factor values
    K <- nlevels(X[,slice.factor]) # number of slice factor values
    
    # pocet opakovani bootstrapu
    opakovani <- n.boot
    
    xlab <- c(paste('z', 1:(I-1), '^r', sep=''), paste('z', 1:(J-1), '^c', sep=''), paste('z', 1:(K-1), '^s', sep=''),
              paste('z', sort(outer(c(1:(I-1)),c(1:(J-1)), FUN=function(x,y)paste(x,y,sep=''))), '^rc', sep=''),
              paste('z', sort(outer(c(1:(I-1)),c(1:(K-1)), FUN=function(x,y)paste(x,y,sep=''))), '^rs', sep=''),
              paste('z', sort(outer(c(1:(J-1)),c(1:(K-1)), FUN=function(x,y)paste(x,y,sep=''))), '^cs', sep=''),
              paste('z', sort(outer(outer(c(1:(I-1)),c(1:(J-1)), FUN=function(x,y)paste(x,y,sep='')),c(1:(K-1)), FUN=function(x,y)paste(x,y,sep=''))), '^rcs', sep=''))
    
    boxplot(Coordinates, notch=TRUE, names=xlab)
    abline(a=0, b=0, lty="dashed")
    
    means <- t(replicate(opakovani,apply(Coordinates[sample(N,replace=TRUE),],2,mean)))
    CIl <- apply(means,2,quantile,0.025)
    CIu <- apply(means,2,quantile,0.975)
    Bootstrap <- cbind(mean, sd, CIl, CIu)
    
    res <- list('Coordinates'=Coordinates, 'Log.ratios'=t(Log.ratios), 'Row.balances'=t(Row.balances),
                'Column.balances'=t(Column.balances), 'Slice.balances'=t(Slice.balances),
                'Row.column.OR'=t(Row.column.OR), 'Row.slice.OR'=t(Row.slice.OR), 'Column.slice.OR'=t(Column.slice.OR),
                'Row.col.slice.OR'=t(Row.col.slice.OR),
                'Grap.rep'=new$Grap.rep, 'Contrast.matrix'=new$Contrast.matrix, 'Cubes'=Tables, 'Bootstrap'=Bootstrap)
  }
  
  
  return(res)
}