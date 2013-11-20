adtestWrapper=function(x,alpha=0.05,R=1000, robustEst=FALSE){
if(robustEst == TRUE ) robust <- "robust" else robust <- "standard"
z=isomLR(x)
n=ncol(z)
if(ncol(z)==1){
  res<-info<-list()
  res[[1]]=adtest(z,R,locscatt=robust)
  info[[1]]=paste(1)
  check<- logical(1)
}
if(ncol(z)==2){
  res<-info<-list()
  res[[1]]=adtest(z[,1],R,locscatt=robust)
  res[[2]]=adtest(z[,2],R,locscatt=robust) 
  res[[3]]=adtest(z,R,locscatt=robust)
  info[[1]]=paste(1)
  info[[2]]=paste(2)
  info[[3]]=paste(3)
  check <- logical(3)
}
if(ncol(z)>2){
  res<-info<-list()
  for(i in 1:ncol(z)){
    res[[i]]=adtest(z[,i],R,locscatt=robust)
    info[[i]]=paste(i)
  }
  index=1
  for(i in 1:(ncol(z)-1)){
    for(j in (i+1):ncol(z)){     
      res[[n+index]]=adtest(z[,c(i,j)],R,locscatt=robust)
      info[[n+index]]=paste(i,j,collapse=":")
      index=index+1
    }
  }
  res[[n+index]]=adtest(z,R,locscatt=robust)
  info[[n+index]]=paste("all")
  check <- logical(n+index)  
}

for(i in 1:length(check)){  
  check[i] <- ifelse(res[[i]]$p.value > alpha, TRUE, FALSE)
}
output=list(res=res, check=check, alpha=alpha, info=info, est=robust)
class(output)="adtestWrapper"
invisible(output)
}