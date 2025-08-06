#' Function calculating a set of (D-1) principal balances based on PLS.
#'
#' @param Xcoda a matrix of raw compositional data with "n" rows and "D" columns/components
#' @param ycoda a response variable; can be continuous (PLS regression) or binary (PLS-DA)
#' @param version a parameter determining whether the balances are ordered according to max. covariance (default) or max. correlation
#'
#' @importFrom pls plsr
#'
#' @return A list with the following components:
#' \describe{
#'   \item{\code{bal}}{A matrix of (D-1) principal balances.}
#'   \item{\code{cov}}{Covariance of each balance with the response variable.}
#' }
#'
#' @details
#' The function creates a set of (D-1) principal balances based on PLS. The procedure builds on the method building principal balances based on PCA, introduced in Martin-Fernandez et al. (2018)
#' For detailed information regarding PLS principal balances, see Nesrstová et al. (2023).
#'
#' @author Viktorie Nesrstová
#'
#' @references
#' J. A. Martín-Fernández, V. Pawlowsky-Glahn, J. J. Egozcue, and R. Tolosona-Delgado. Advances in principal balances for compositional data. Mathematical Geosciences, 50(3):273–298, 2018. Available at:
#' \url{https://link.springer.com/article/10.1007/s11004-017-9712-z}
#' DOI: \doi{10.1007/s11004-017-9712-z}
#'
#' Nesrstová, V, Wilms, I, Palarea-Albaladejo, J, et al. Principal balances of compositional data for regression and classification using partial least squares. Journal of Chemometrics. 2023; 37(12):e3518..  Available at:
#' \url{https://analyticalsciencejournals.onlinelibrary.wiley.com/doi/full/10.1002/cem.3518}
#' DOI: \doi{10.1002/cem.3518}
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   if (requireNamespace("MASS", quietly = TRUE)) {
#'
#' # 1. Generate sample data ------------------------------------------------------
#' n <- 100              # observations
#' D <- 15               # parts/variables
#' Sig <- diag(D-1)      # positive-definite symmetric matrix -> covariance matrix
#' mu <- c(rep(0, D-1))  # means of variables
#'
#' set.seed(123)
#' # ilr coordinates
#' Z <- MASS::mvrnorm(n,mu,Sigma = Sig)
#'
#' # Z -> CoDa X
#' V <- compositions::ilrBase(D = D)  # ilrBase() in library(compositions)
#' X <- as.matrix(as.data.frame(acomp(exp(Z%*%t(V)))))
#'
#' # Response y:
#' beta <- runif(D-1,0.1,1)
#' eps <- rnorm(n)
#' y <- Z%*%beta+eps
#'
#' # 2. Calculate PLS PBs
#'
#' PLS_balances <- fBalChip_PLS(X,y,version = "cov")     # version = "cov" -> max. covariance
#' balances <- PLS_balances$bal
#'   }
#' }
#'
pls_pb <-function(Xcoda, ycoda, version = "cov"){

  numbal=ncol(Xcoda)-1

  # call the recursive function
  res<-fBPMaxOrthNewChip_PLS(Xcoda,ycoda,version=version)
  Bres<-res$bal
  balname<-paste("bal",1:nrow(Bres),sep="")
  rownames(Bres)<-balname
  colnames(Bres)<-colnames(Xcoda)
  Vres<-res$varbal

  # sort by expl var
  vopt<-res$varbal
  # sort variance
  vsopt<-sort(vopt,decreasing = TRUE,index.return=TRUE)
  #
  # assign variance explained already ordered
  Vres<-vsopt$x
  #
  # assign balances same order
  Bres<-Bres[vsopt$ix,]
  #
  # return results: balances and variances

  if(version=="cov"){
    return(list(bal=Bres,cov=Vres))
  } else if(version=="cor"){
    return(list(bal=Bres,cor=Vres))
  }
}

# Helper function 1
#'
#'
#' @param C a matrix of raw compositional data with "n" rows and "D" columns/components
#' @param r2 a response variable; can be continuous (PLS regression) or binary (PLS-DA)
#' @param angle if "angle=FALSE" the Max the var of scores
#' @param version a parameter determining whether the balances are ordered according to max. covariance (default) or max. correlation
#'
#' @importFrom pls plsr
#'
#' @return A list with the following components:
#' \describe{
#'   \item{\code{bal}}{}
#'   \item{\code{varbal}}{}
#' }
#'
#'
#'
fBalChipman_PLS<-function(C,r2,angle=TRUE, version = "cov"){
  
  # columns
  col<-dim(C)[2]
  nbal<-col-1
  
  # clr-transfo
  clrC_1 <-log(C) - rowMeans(log(C))
  clrC <- scale(clrC_1, center=TRUE, scale = FALSE)
  
  yC <- r2 - mean(r2)
  
  # PCs
  #
  
  pcClr<-pls::plsr(yC~clrC,method="simpls",center=F)
  #first PC: PC1
  pcClr1 <- pcClr$loadings[,1]
  
  balsig<-sign(pcClr1)
  bal<-matrix(0,nbal,col)
  colnames(bal)<-colnames(C)
  # balances associated to the PCs
  # first bal
  bal[1,pcClr1==max(pcClr1)]<-1
  bal[1,pcClr1==min(pcClr1)]<--1
  numbal=1
  
  # other bal
  if (col>2){
    numbal=numbal+1
    while (numbal<col){
      bal[numbal,]<-bal[numbal-1,]
      useonly<-(bal[numbal-1,]==0)
      bal[numbal,abs(pcClr1)==max(abs(pcClr1[useonly]))]<-balsig[abs(pcClr1)==max(abs(pcClr1[useonly]))]
      numbal=numbal+1
    }#end while
  }#end if
  
  # OUTPUT: matrix "(D-1) x D" with -1,0,1
  
  # coefficients & angle
  VarSBP<-rep(0,nbal)
  for (f in 1:nbal) {
    den<-sum(bal[f,]==-1)
    num<-sum(bal[f,]==1)
    bal[f,bal[f,]==1]<-sqrt(den/((den+num)*num))
    bal[f,bal[f,]==-1]<--sqrt(num/((den+num)*den))
    # variance of the balance:
    
    if (version == "cov"){
      VarSBP[f] <- abs(cov(r2,as.matrix(log(C))%*%t(bal)[,f]))
    } else if (version=="cor") {
      VarSBP[f] <- abs(cor(r2,as.matrix(log(C))%*%t(bal)[,f]))
    }
    
  }
  # log-transform
  lC<-as.matrix(log(C))
  
  ##---------------------------------------------------
  if (version=="cov"){
    mvar = abs(cov(r2,as.vector(lC%*%bal[VarSBP==max(VarSBP),])))
  } else if (version == "cor") {
    mvar = abs(cor(r2,as.vector(lC%*%bal[VarSBP==max(VarSBP),])))
  }
  ##---------------------------------------------------
  if (!angle) {
    
    # calculate variance in the balance direction
    VarSBP<-rep(0,nbal)
    
    for (i in 1:nbal)
    {
      Proj<-as.vector(lC%*%(bal[i,]))
      ##---------------------------------------------------
      if(version=="cov"){
        VarSBP[i] <- cov(r2,Proj)
      } else if(version=="cor"){
        VarSBP[i] <- cor(r2,Proj)
      }
      ##---------------------------------------------------
      
    }# end for
    mvar=max(VarSBP)
  }# end if
  
  # return results
  return(list(bal=bal[VarSBP==max(VarSBP),],varbal=mvar))
  
  
}


# Helper function 2
#'
#'
#' @param Y a matrix of raw compositional data with "n" rows and "D" columns/components
#' @param r1 a response variable; can be continuous (PLS regression) or binary (PLS-DA)
#' @param angle if "angle=FALSE" the Max the var of scores
#' @param version a parameter determining whether the balances are ordered according to max. covariance (default) or max. correlation
#'
#' @importFrom pls plsr
#'
#' @return A list with the following components:
#' \describe{
#'   \item{\code{bal}}{}
#'   \item{\code{varbal}}{}
#' }
#'
#'
fBPMaxOrthNewChip_PLS<-function(Y,r1,angle=TRUE, version = "cov")
{
  
  
  numpart=ncol(Y)
  numbal=ncol(Y)-1
  
  B=c()
  V=c()
  
  #first optimal in data set Y
  res<-fBalChipman_PLS(Y,r1,angle=angle,version=version)
  B<-res$bal
  V<-res$varbal
  # if necessary GO UP to complete
  if (sum(B==0)>0){
    res<-fBPUpChi_PLS(Y,r1,B,version = version)
    B=rbind(B,res$bal)
    V=cbind(V,res$varbal)
  }
  # control number of balances added
  if (is.vector(B)) B<-matrix(B,1,length(B))
  numbaladd<-nrow(B)-1
  
  ### GO DOWN THE CURRENt LIST AND THE FIRST
  ## first go down from the first optimal balance
  
  usenum<-(B[1,]>0)
  useden<-(B[1,]<0)
  # GO DOWN from numerator of the first optimal balance
  if(sum(usenum)>1){
    resP<-fBPMaxOrthNewChip_PLS(Y[,usenum],r1,angle=angle, version = version)
    Bx<-matrix(0,length(resP$varbal),numpart)
    Bx[,usenum]<-resP$bal
    B<-rbind(B,Bx)
    V<-cbind(V,resP$varbal)
  }# end if
  # GO DOWN from denominator of the first optimal balance
  if(sum(useden)>1){
    resP<-fBPMaxOrthNewChip_PLS(Y[,useden],r1,angle=angle, version = version)
    Bx<-matrix(0,length(resP$varbal),numpart)
    Bx[,useden]<-resP$bal
    B<-rbind(B,Bx)
    V<-cbind(V,resP$varbal)
  }# end if
  
  # REVISIT list of balances added GO UP so as to complete the SBP if necessary GO DOWN by the POSITIVE
  
  if (numbaladd > 0){
    for (k in 2:(1+numbaladd)){
      usepos=(B[k,]>0)
      if (sum(usepos)>1) {
        resP<-fBPMaxOrthNewChip_PLS(Y[,usepos],r1,angle=angle, version = version)
        Bx<-matrix(0,length(resP$varbal),numpart)
        Bx[,usepos]<-resP$bal
        B<-rbind(B,Bx)
        V<-cbind(V,resP$varbal)
      }#end if2
    }# end for
  }# end if1
  
  # return results
  #
  V<-as.matrix(V,1,length(V))
  #
  return(list(bal=B,varbal=V))
  
}


# Helper function 3
#' Title
#'
#' @param Yp a matrix of raw compositional data with "n" rows and "D" columns/components
#' @param r3 a response variable; can be continuous (PLS regression) or binary (PLS-DA)
#' @param b a given balance constructed during the procedure (contains some zero value(s))
#' @param version a parameter determining whether the balances are ordered according to max. covariance (default) or max. correlation
#'
#' @return A list with the following components:
#' \describe{
#'   \item{\code{bal}}{}
#'   \item{\code{varbal}}{}
#' }
#'
#'
fBPUpChi_PLS<-function(Yp,r3,b,version="cov")
{
  
  # given coda set Yp
  # and given a balance with some zero
  # return list of PARENT principal balances basis
  # that maximizises the variance
  # searching by the NO-FULL {0, -1,+1} and COMPLETING the
  # SBP using a loop (UP) scheme by the CHIPMAN procedure
  
  npart=ncol(Yp)
  nbal=ncol(Yp)-1
  
  # to save balances and variances
  Bal=c()
  VarB=c()
  
  usezero<-sum(b==0)
  
  #log-transfo data
  lYp=as.matrix(log(Yp))
  # while it is not the full balance go up
  k=0
  while (usezero>0){
    # new balance
    k<-k+1
    # non-zero in the will be in the denominator
    den<-sum(b!=0)
    # for only one zero we get the full
    if (usezero==1){
      
      b[b!=0]<--sqrt(1/((den+1)*den))
      b[b==0]<-sqrt(den/(den+1))
      
      ##-------------------------------------------
      if(version=="cov"){
        VarB <- cbind(VarB, abs(cov(r3,lYp%*%b)))
      }else if (version=="cor"){
        VarB <- cbind(VarB, abs(cor(r3,lYp%*%b)))
      }
      ##-------------------------------------------
      Bal<-rbind(Bal,b)
      usezero<-0
    }
    # for more than one zero we explore other {0,+1} combinations
    else{
      # create the combination by CHIPMAN procedure
      # search the maximum balance
      
      clrC_1 <-log(Yp[,b==0]) - rowMeans(log(Yp[,b==0]))
      clrC <- scale(clrC_1, center=TRUE, scale = FALSE)
      
      yC <- r3 - mean(r3)
      # PCs
      #
      pcClr<-pls::plsr(yC~clrC,method="simpls",center=F)
      
      #first PC: PC1
      bx<-pcClr$loadings[,1]
      
      #
      # look for change of sign
      if (abs(min(bx))>max(bx)){bx<--bx}
      # force zeros to the other sign
      bx[bx<0]<-0
      # matrix of {0,+1} possibilities
      M<-matrix(0,sum(bx>0),length(bx))
      # sort
      bxsort<-sort(bx,decreasing = TRUE,index.return=TRUE)
      # index
      col<-bxsort$ix
      # create M
      for (i in 1:nrow(M)){
        M[i,col[1:i]]<-abs(bx[col[1:i]])
      }
      # sign
      M<-sign(M)
      
      # create a balance
      balax<-b
      # old non-zero to denominator
      balax[b!=0]<--1
      # search the max variance
      VarSBPx<-matrix(0,1,nrow(M))
      balsx<-c()
      for (i in 1:nrow(M))
      {
        # take one possibility
        balax[b==0]<-M[i,]
        # create the coefficients
        num<-sum(balax==1)
        balax[balax==1]<-sqrt(den/((den+num)*num))
        balax[balax==-1]<--sqrt(num/((den+num)*den))
        balsx=rbind(balsx,balax)
        
        ##-------------------------------------------#
        if(version=="cov"){
          VarSBPx[i]<-abs(cov(r3,lYp%*%balax))
        } else if(version=="cor"){
          VarSBPx[i]<-abs(cor(r3,lYp%*%balax))
        }
        ##-------------------------------------------
      }
      VarB=cbind(VarB,max(VarSBPx))
      Bal=rbind(Bal,balsx[VarSBPx==VarB[k],])
      
      rm(M)
      usezero<-sum(Bal[k,]==0)
      b<-Bal[k,]
      # end else GO UP
    }
    
    # end GO UP while
  }
  # return results
  return(list(bal=Bal,varbal=VarB))
  # end function
}


