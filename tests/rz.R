require(robCompositions)
data(expenditures)
xOrig <- x <- expenditures

## DL as negative values
x[x < 350] <- - 350

#impCoda2(x)

## DL given:
#impCoda2(xOrig, dl=c(0, 350, 350, 350, 350))


#imp <- impCoda(x, method='roundedZero')
#imp2 <- alrEM(x, pos=2, dl=rep(5,3))$xImp


