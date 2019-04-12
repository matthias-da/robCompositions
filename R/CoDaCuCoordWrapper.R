#' CoDaCuCoordWrapper
#' 
#' @title Coordinate representation of a sample of compositional cubes
#' @importFrom tidyr unite
#' @importFrom tidyr spread
#' @importFrom graphics boxplot
#' @author Kamila Facevicova
#' @references Facevicova, K., Filzmoser, P. and K. Hron (2019) Compositional Cubes: Three-factorial Compositional Data. Under review.
#' @description For each compositional cube in the sample CoDaCuCoordWrapper computes 
#' a system of orthonormal coordinates and provide a simple descriptive analysis. 
#' Computation of either pivot coordinates or a coordinate system based on the 
#' given SBP is possible.
#' @param X a data frame containing variables representing row, column and slice factors 
#' of the respective compositional cubes, variable with the values 
#' of the composition and variable distinguishing the observations.
#' @param obs.ID name of the variable distinguishing the observations. Needs to be stated with the quotation marks.
#' @param row.factor name of the variable representing the row factor. Needs to be stated with the quotation marks.
#' @param col.factor name of the variable representing the column factor. Needs to be stated with the quotation marks.
#' @param slice.factor name of the variable representing the slice factor. Needs to be stated with the quotation marks.
#' @param value name of the variable representing the values of the composition. Needs to be stated with the quotation marks.
#' @param SBPr an \eqn{I-1\times I} array defining the sequential binary partition of the values of the row factor, where I is the number of the row factor levels. The values assigned in the given step to the + group are marked by 1, values from  the - group by -1 and the rest by 0. If it is not provided, the pivot version of coordinates is constructed automatically.
#' @param SBPc an \eqn{J-1\times J} array defining the sequential binary partition of the values of the column factor, where J is the number of the column factor levels. The values assigned in the given step to the + group are marked by 1, values from  the - group by -1 and the rest by 0. If it is not provided, the pivot version of coordinates is constructed automatically.
#' @param SBPs an \eqn{K-1\times K} array defining the sequential binary partition of the values of the slice factor, where K is the number of the slice factor levels. The values assigned in the given step to the + group are marked by 1, values from  the - group by -1 and the rest by 0. If it is not provided, the pivot version of coordinates is constructed automatically.
#' @param pivot logical, default is FALSE. If TRUE, or one of the SBPs is not defined, its pivot version is used.
#' @param test logical, default is FALSE. If TRUE, the bootstrap analysis of coordinates is provided.
#' @param n.boot number of bootstrap samples.
#' @details Each of n IJK-part compositional cubes from the sample is 
#' with respect to its three-factorial nature isometrically transformed 
#' from the simplex into a (IJK-1)-dimensional real space. 
#' Sample mean values and standard deviations are computed and using 
#' bootstrap an estimate of 95 \% confidence interval is given. 
#' @keywords multivariate, coordinates
#' @export
#' @seealso 
#' \code{\link{CoDaTaCoord}} 
#' \code{\link{CoDaTaCoordWrapper}} 
#' \code{\link{CoDaCuCoord}}
#' @return 
#' \item{Coordinates}{an array of orthonormal coordinates. }
#' \item{Row.balances}{an array of row balances.}
#' \item{Column.balances}{an array of column balances.}
#' \item{Slice.balances}{an array of slice balances.}
#' \item{Row.column.OR}{an array of row-column OR coordinates.}
#' \item{Row.slice.OR}{an array of row-slice OR coordinates.}
#' \item{Column.slice.OR}{an array of column-slice OR coordinates.}
#' \item{Row.col.slice.OR}{an array of coordinates describing the mutual interaction between all three factors.}
#' \item{Log.ratios}{an array of log-ratios between two/four/eight groups parts corresponding to coordinates without the normalising constant.}
#' \item{Grap.rep}{graphical representation of the coordinates. Parts denoted by + form the groups in the numerator of the respective computational formula, parts \verb{-} form the denominator and parts \verb{.} are not involved in the given coordinate.}
#' \item{Contrast.matrix}{contrast matrix.}
#' \item{Bootstrap}{array of sample means, standard deviations and bootstrap confidence intervals.}
#' \item{Cubes}{Cube form of the given compositions.}
#' @examples 
#' ### example from Fa\v cevicov\'a (2019)
#' data(employment2)
#' ### Compositional tables approach,
#' ### analysis of the relative structure.
#' ### An example from Facevi\v cov\'a (2019)
#' 
#' # pivot coordinates
#' CoDaCuCoordWrapper(employment2, 'Country', 'Sex', 'Contract', 'Age', 'Value',  
#' test=TRUE)
#' 
#' # coordinates with given SBP (defined in the paper)
#' 
#' r <- t(c(1,-1))
#' c <- t(c(1,-1))
#' s <- rbind(c(1,-1,-1), c(0,1,-1))
#' 
#' res <- CoDaCuCoordWrapper(employment2, 'Country', 'Sex', 'Contract', 
#' "Age", 'Value', r,c,s, test=TRUE)
#' 
#' ### Classical approach,
#' ### generalized linear mixed effect model.
#' 
#' \dontrun{
#' library(lme4)
#' employment2$y <- round(employment2$Value*1000)
#' glmer(y~Sex*Age*Contract+(1|Country),data=employment2,family=poisson)
#' }
#' 
#' ### other relations within cube (in the log-ratio form)
#' ### e.g. ratio between women and man in the group FT, 15to24
#' ### and ratio between age groups 15to24 and 55plus
#' 
#' # transformation matrix
#' T <- rbind(c(1,rep(0,5), -1, rep(0,5)), c(rep(c(1/4,0,-1/4), 4)))
#' T\%*\%t(res$Contrast.matrix)\%*\%res$Bootstrap[,1]
CoDaCuCoordWrapper <- function(X, obs.ID=NULL, row.factor=NULL, col.factor=NULL, slice.factor=NULL, 
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
    new <- CoDaCuCoord(x=X[obs,], row.factor=row.factor, col.factor=col.factor, slice.factor=slice.factor, value=value, SBPr=SBPr, SBPc=SBPc, SBPs=SBPs, pivot=pivot, print.res=FALSE)
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

########################
### Example from facevicova19

# load("employment.rda")

### pivot coordinates
# CoDaCuCoord2(employment, 'Country', "Sex", 'Contract', "Age", 'Value',  test=TRUE)

### coordinates with given SBP

#r <- t(c(1,-1))
#c <- t(c(1,-1))
#s <- rbind(c(1,-1,-1), c(0,1,-1))

# res <- CoDaCuCoord2(employment, 'Country', "Sex", 'Contract', "Age", 'Value', r,c,s, test=TRUE)
# res

### glm
# library(lme4)
# employment$y <- round(employment$Value*1000)
# employment$Sex2 <- relevel(employment$Sex, "MALE")
# m <- glmer(y~Sex2*Age*Contract+(1|Country),data=employment,family=poisson)
# summary(m)

### other relations within cube (in the log-ratio form)
### e.g. ratio between women and man in the group FT, 15to24
### and ratio between age groups 15to24 and 55plus

### transformation matrix
# T <- rbind(c(1,rep(0,5), -1, rep(0,5)), c(rep(c(1/4,0,-1/4), 4)))
# T%*%t(res$Contrast.matrix)%*%res$Bootstrap[,1]
