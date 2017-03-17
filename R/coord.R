#' Coordinate representation of CoDa tables
#' 
#' General approach to orthonormal coordinates for compositional tables
#' 
#' @aliases coord print.coord
#' @param x an object of class \dQuote{table}, \dQuote{data.frame} or \dQuote{matrix}
#' @param SBPr sequential binary partition for rows
#' @param SBPc sequential binary partition for columns
#' @param ... further arguments passed to the print function
#' @details A contingency or propability table is by definition a compositional data set. This approach consider
#' the related special properties of compositional tables. It constructs orthonomal coordinates for compositional tables using
#' the isometric log-ratio approach for given sequential binary partitions on rows and columns.
#' @author Kamila Facevicova, and minor adaption by Matthias Templ
#' @return Row and column balances, odds ratios, particularly
#' \item{row_balances}{row balances}
#' \item{row_bin}{binary partition for rows}
#' \item{col_balances}{column balances}
#' \item{col_bin}{binary parition for columns}
#' \item{odds_ratios_coord}{odds ratio coordinates} 
#' @references 
#' Kamila Facevicova, Karel Hron, Valentin Todorov, Matthias Templ (201x)
#' General approach to coordinate representation of compositional tables. 
#' Submitted to \emph{JRSS B}.
#' @export
#' @examples 
#' x <- rbind(c(1,5,3,6,8,4),c(6,4,9,5,8,12),c(15,2,68,42,11,6),
#'            c(20,15,4,6,23,8),c(11,20,35,26,44,8))
#' x
#' SBPc <- rbind(c(1,1,1,1,-1,-1),c(1,-1,-1,-1,0,0),c(0,1,1,-1,0,0),
#'               c(0,1,-1,0,0,0),c(0,0,0,0,1,-1))
#' SBPc
#' SBPr <- rbind(c(1,1,1,-1,-1),c(1,1,-1,0,0),c(1,-1,0,0,0),c(0,0,0,1,-1))
#' SBPr
#' result <- coord(x, SBPr,SBPc)
#' result
#' data(socExp)
#' 
coord <- function(x, SBPr, SBPc)
{
  I <- nrow(x)
  J <- ncol(x)
  row_geom_mean <- gmean(x, margin = 1)
  col_geom_mean <- gmean(x, margin = 2)
  ## intitialize
  z_row <- numeric(I-1) #rep(NA,I-1)
  z_col <- numeric(J-1) #rep(NA,J-1)
  z_OR <- numeric((I-1)*(J-1)) #rep(NA,(I-1)*(J-1))
  grap.rep.row <- grap.rep.col <- grap.rep.OR <- list() #as.list(rep(NA,I-1))
  #grap.rep.col <- as.list(rep(NA,J-1))
  #grap.rep.OR <- as.list(rep(NA,(I-1)*(J-1)))
  
  index <- 1
  for(i in 1:(I-1))
  {
    rpos <- which(SBPr[i,] == 1)
    rneg <- which(SBPr[i,] == -1)
    rposn <- length(rpos)
    rnegn <- length(rneg)
    z_row[i] <- sqrt(J * rposn * rnegn / (rposn + rnegn)) *
      log(gmean(row_geom_mean[rpos], margin = 3) / gmean(row_geom_mean[rneg], margin = 3))
    grap.rep.row[[i]] <- matrix(0,nrow=I,ncol=J)
    grap.rep.row[[i]][rpos,] <- "+"
    grap.rep.row[[i]][rneg,] <- "-"
    
    for(j in 1:(J-1))
    {
      cpos <- which(SBPc[i,] == 1)
      cneg <- which(SBPc[i,] == -1)
      cposn <- length(cpos)
      cnegn <- length(cneg)      
      z_OR[index] <- sqrt(rposn * cposn * rnegn * cnegn / (length(which(SBPr[i,]%in%c(-1,1)))*length(which(SBPc[j,]%in%c(-1,1)))))*
        log(gmean(x[rpos, cpos], margin = 3) * gmean(x[rneg, cneg], margin = 3) / 
              (gmean(x[rpos, cneg], margin = 3) * gmean(x[rneg, cpos], margin = 3)))
      grap.rep.OR[[index]] <- matrix(0,nrow=I,ncol=J)
      grap.rep.OR[[index]][rpos, cpos] <- "+"
      grap.rep.OR[[index]][rneg, cneg] <- "+"
      grap.rep.OR[[index]][rpos, cneg] <- "-"
      grap.rep.OR[[index]][rneg, cpos] <- "-"
      
      z_OR[index] <- sqrt(rposn*cposn*rnegn*cnegn/(length(which(SBPr[i,]%in%c(-1,1)))*length(which(SBPc[j,]%in%c(-1,1)))))*
        log(gmean(x[rpos,cpos], margin = 3)*gmean(x[rneg,cneg], margin = 3)/(gmean(x[rpos,cneg], margin = 3)*gmean(x[rneg,cpos], margin = 3)))
      grap.rep.OR[[index]] <- matrix(0,nrow=I,ncol=J)
      grap.rep.OR[[index]][rpos,cpos] <- "+"
      grap.rep.OR[[index]][rneg,cneg] <- "+"
      grap.rep.OR[[index]][rpos,cneg] <- "-"
      grap.rep.OR[[index]][rneg,cpos] <- "-"
      
      index <- index + 1
      
      z_col[j] <- sqrt(I*length(which(SBPc[j,]==1))*length(which(SBPc[j,]==-1))/(length(which(SBPc[j,]==1))+length(which(SBPc[j,]==-1))))*
        log(gmean(col_geom_mean[which(SBPc[j,]==1)], margin = 3)/gmean(col_geom_mean[which(SBPc[j,]==-1)], margin = 3))
      grap.rep.col[[j]] <- matrix(0,nrow=I,ncol=J)
      grap.rep.col[[j]][,which(SBPc[j,]==1)] <- "+"
      grap.rep.col[[j]][,which(SBPc[j,]==-1)] <- "-"
    }
    
  }
  
  # return:
  res <- list("row_balances" = z_row,
              "row_bin" = grap.rep.row,
              "col_balances" = z_col,
              "col_bin" = grap.rep.row,
              "odds_ratios_coord" = z_OR,
              "odds_bin" = grap.rep.OR)
  class(res) <- "coord"
  return(res)
}

#' @rdname coord
#' @method print coord
#' @export
print.coord <- function(x,...){
  cat("--------------------------------------")
  cat("\nCoordinate representation of CoDa table\n")
  cat("\n- row balances: \n")
  print(x$row_balances)
  cat("\n- column balances: \n")
  print(x$col_balances)
  cat("\n- odds ratios from coordinates: \n")
  print(x$odds_ratios_coord)
  cat("--------------------------------------\n")
}
