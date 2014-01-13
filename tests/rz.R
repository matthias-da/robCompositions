require(robCompositions)
data(expenditures)
xOrig <- x <- expenditures

## DL as negative values
x[x < 300] <- 0


## DL given:
for(i in 1:ncol(xOrig)){
  xOrig[,i] <- as.numeric(xOrig[,i]) 
}
x2 <- impRZilr(xOrig, dl=rep(300,5), method="lm")
x2 <- impRZilr(xOrig, dl=rep(300,5), method="pls",nComp=rep(2,ncol(xOrig)))
xr <- x2$x
head(expenditures,3)
head(xr,3)
