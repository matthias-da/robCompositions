#' CoDaTaCoordWrapper
#' 
#' @title Coordinate representation of a sample of compositional tables
#' @author Kamila Facevicova
#' @references Facevicova, K., Hron, K., Todorov, V. and M. Templ (2018) 
#' General approach to coordinate representation of compositional tables. 
#' Scandinavian Journal of Statistics, 45(4), 879--899.
#' @description For each compositional table in the sample \code{CoDaTaCoord2} 
#' computes a system of orthonormal coordinates and provide a simple descriptive analysis. 
#' Computation of either pivot coordinates or a coordinate system based on the given SBP is possible.
#' 
#' @param X a data frame containing variables representing row and column factors of the respective compositional tables, variable with the values 
#' of the composition and variable distinguishing the observations.
#' @param obs.ID name of the variable distinguishing the observations. Needs to be stated with the quotation marks.
#' @param row.factor name of the variable representing the row factor. Needs to be stated with the quotation marks.
#' @param col.factor name of the variable representing the column factor. Needs to be stated with the quotation marks.
#' @param value name of the variable representing the values of the composition. Needs to be stated with the quotation marks.
#' @param SBPr an \eqn{I-1\times I} array defining the sequential binary partition of the values of the row factor, where I is the number of the row factor levels. The values assigned in the given step to the + group are marked by 1, values from  the - group by -1 and the rest by 0. If it is not provided, the pivot version of coordinates is constructed automatically.
#' @param SBPc an \eqn{J-1\times J} array defining the sequential binary partition of the values of the column factor, where J is the number of the column factor levels. The values assigned in the given step to the + group are marked by 1, values from  the - group by -1 and the rest by 0. If it is not provided, the pivot version of coordinates is constructed automatically.
#' @param pivot logical, default is FALSE. If TRUE, or one of the SBPs is not defined, its pivot version is used.
#' @param test logical, default is \code{FALSE}. If \code{TRUE}, the bootstrap analysis of coordinates is provided.
#' @param n.boot number of bootstrap samples.
#' @details Each of n IJ-part compositional tables from the sample is with 
#' respect to its two-factorial nature isometrically transformed from the simplex 
#' into a (IJ-1)-dimensional real space. Sample mean values and standard deviations are 
#' computed and using bootstrap an estimate of 95 \% confidence interval is given. 
#' @keywords multivariate, coordinates
#' @export
#' @seealso 
#' \code{\link{CoDaCuCoord}} 
#' \code{\link{CoDaTaCoord}} 
#' \code{\link{CoDaCuCoordWrapper}}
#' @return 
#' \item{Coordinates}{an array of orthonormal coordinates.}
#' \item{Log.ratios}{an array of pure log-ratios between groups of parts without the normalizing constant.
#' \item{Ind.coord}{an array of row and column balances. Coordinate representation of the independent part of the tables.}
#' \item{Int.coord}{an array of OR coordinates. Coordinate representation of the interactive part of the tables.}
#' \item{Grap.rep}{graphical representation of the coordinates. Parts denoted by \code{+} form the groups in the numerator of the respective computational formula, parts \verb{-} form the denominator and parts \verb{.} are not involved in the given coordinate.}
#' \item{Contrast.matrix}{contrast matrix.}
#' \item{Bootstrap}{array of sample means, standard deviations and bootstrap confidence intervals.}
#' \item{Tables}{Table form of the given compositions.}
#' @examples 
#' # example from Fa\v cevicov\'a (2018):
#' data(manu_abs)
#' 
#' ### Compositional tables approach,
#' ### analysis of the relative structure.
#' ### An example from Facevi\v cov\'a (2018)
#' 
#' manu_abs$output <- factor(manu_abs$output, levels=c('LAB', 'SUR', 'INP'))
#' 
#' # pivot coordinates
#' CoDaTaCoordWrapper(manu_abs, obs.ID='country',
#' row.factor = 'output', col.factor = 'isic', value='value')
#' 
#' # SBPs defined in paper
#' r <- rbind(c(-1,-1,1), c(-1,1,0))
#' c <- rbind(c(-1,-1,-1,-1,1), c(-1,-1,-1,1,0), 
#' c(-1,-1,1,0,0), c(-1,1,0,0,0))
#' CoDaTaCoordWrapper(manu_abs, obs.ID='country',row.factor = 'output', 
#' col.factor = 'isic', value='value', SBPr=r, SBPc=c, test=TRUE)
#' 
#' ### Classical approach,
#' ### generalized linear mixed effect model.
#' 
#' \dontrun{
#' library(lme4)
#' glmer(value~output*as.factor(isic)+(1|country),data=manu_abs,family=poisson)
#' }
CoDaTaCoordWrapper <- function(X, obs.ID=NULL, row.factor=NULL, col.factor=NULL, value=NULL, 
                               SBPr=NULL, SBPc=NULL, pivot=FALSE, test=FALSE, n.boot=1000)
{
  # Control and subsidiary parameters setting
  if(is.null(obs.ID)) stop('Name of the observation ID variable is not defined!')
  if(is.null(row.factor)) stop('Name of the row factor is not defined!')
  if(is.null(col.factor)) stop('Name of the column factor is not defined!')
  if(is.null(value)) stop('Name of the value variable is not defined!')
  
  X[,obs.ID] <- as.factor(X[,obs.ID])
  X[,row.factor] <- as.factor(X[,row.factor])
  X[,col.factor] <- as.factor(X[,col.factor])
  N <- nlevels(X[,obs.ID])
  I <- nlevels(X[,row.factor]) # number of row factor levels
  J <- nlevels(X[,col.factor]) # number of column factor levels
  
  if(!identical(as.numeric(table(X[,c(row.factor,obs.ID)])),as.numeric(rep(J,(I*N))))) stop('The CoDa Tables are not defined properly, some values are missing!')
  if(!is.null(SBPr)&(nrow(SBPr)!= (I-1)||ncol(SBPr)!=I)) 
  {warning('The row SBP is not defined properly, pivot coordinates are used!')
    SBPr <- NULL}
  if(!is.null(SBPc)&(nrow(SBPc)!= (J-1)||ncol(SBPc)!=J)) 
  {warning('The column SBP is not defined properly, pivot coordinates are used!')
    SBPc <- NULL}
  
  Coordinates <- NULL
  Log.ratios <- NULL
  Ind.coord <- NULL
  Int.coord <- NULL
  Tables <- array(NA, c(I, J, N))
  for(i in 1:N)
  {
    obs <- which(X[,obs.ID]==levels(X[,obs.ID])[i])
    new <- CoDaTaCoord(x=X[obs,], row.factor=row.factor, col.factor=col.factor, value=value, SBPr=SBPr, SBPc=SBPc, pivot=pivot, print.res=FALSE)
    Coordinates <- cbind(Coordinates, new$Coordinates)
    Log.ratios <- cbind(Log.ratios, new$Log.ratios)
    Ind.coord <- cbind(Ind.coord, new$Ind.coord)
    Int.coord <- cbind(Int.coord, new$Int.coord)
    Tables[,,i] <- as.matrix(new$Coda.table)
    
  }
  Coordinates <- t(Coordinates)
  
  rownames(Coordinates) <- levels(X[,obs.ID])
  colnames(Log.ratios) <- levels(X[,obs.ID])
  colnames(Ind.coord) <- levels(X[,obs.ID])
  colnames(Int.coord) <- levels(X[,obs.ID])
  dimnames(Tables)[[1]] <- levels(X[,row.factor])
  dimnames(Tables)[[2]] <- levels(X[,col.factor])
  dimnames(Tables)[[3]] <- levels(X[,obs.ID])
  
  res <- list('Coordinates'=(Coordinates), 'Log.ratios'=t(Log.ratios), 'Ind.coord'=t(Ind.coord), 'Int.coord'=t(Int.coord),
              'Grap.rep'=new$Grap.rep, 'Contrast.matrix'=new$Contrast.matrix, 'Tables'=Tables)
  
  if(test==TRUE)
  {
    # sample characteristics
    mean <- apply(Coordinates, 2, mean)
    sd <- apply(Coordinates, 2, sd)
    
    #set.seed(123)
    
    # pocet opakovani bootstrapu
    opakovani <- n.boot
    I <- nlevels(X[,row.factor])
    J <- nlevels(X[,col.factor])
    xlab <- c(paste('z', 1:(I-1), '^r', sep=''), paste('z', 1:(J-1), '^c', sep=''), paste('z', sort(outer(c(1:(I-1)),c(1:(J-1)), FUN=function(x,y)paste(x,y,sep=''))), '^OR', sep=''))
    boxplot(Coordinates, notch=TRUE, names=xlab)
    abline(a=0, b=0, lty="dashed")
    
    means <- t(replicate(opakovani,apply(Coordinates[sample(N,replace=TRUE),],2,mean)))
    CIl <- apply(means,2,quantile,0.025)
    CIu <- apply(means,2,quantile,0.975)
    Bootstrap <- cbind(mean, sd, CIl, CIu)
    
    res <- list('Coordinates'=(Coordinates), 'Log.ratios'=t(Log.ratios), 'Ind.coord'=t(Ind.coord), 'Int.coord'=t(Int.coord),
                'Grap.rep'=new$Grap.rep, 'Contrast.matrix'=new$Contrasts, 'Bootstrap'=Bootstrap, 'Tables'=Tables)
  }
  
  
  return(res)
}


################################################
### Example - manufacturing USA - facevicova18

# load('manu_abs.rda')

### change of the reference level according to the paper
# manu_abs$output <- factor(manu_abs$output, levels=c('LAB', 'SUR', 'INP'))

### pivot coordinates
# CoDaTaCoord2(manu_abs, obs.ID='country',row.factor = 'output', col.factor = 'isic', value='value')

### SBPs defined in paper
# r <- rbind(c(-1,-1,1), c(-1,1,0))
# c <- rbind(c(-1,-1,-1,-1,1), c(-1,-1,-1,1,0), c(-1,-1,1,0,0), c(-1,1,0,0,0))

# CoDaTaCoord2(manu_abs, obs.ID='country',row.factor = 'output', col.factor = 'isic', value='value', SBPr=r, SBPc=c, test=T)

### glmer
# library(lme4)
# m <- glmer(value~output*as.factor(isic)+(1|country),data=manu_abs,family=poisson)
# summary(m)

