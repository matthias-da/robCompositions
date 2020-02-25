#library(robCompositions)
# SepalLengthCm <- iris$Sepal.Length
# Species <- iris$Species
#  
# iris1 <- SepalLengthCm[iris$Species==levels(iris$Species)[1]]
# h1 <- hist(iris1, nclass = 12, plot = FALSE)
# 
# midx1 <- h1$mids
# midy1 <- matrix(h1$density, nrow=1, ncol = length(h1$density), byrow=TRUE)
# knots <- 7 
# sol1 <- smoothSplines(k=3,l=2,alpha=1000,midy1,midx1,knots)
# expect_equal(class(sol1), "smoothSpl")
# 
# plot(sol1)
# h1 <- hist(iris1, freq = FALSE, nclass = 12, xlab = "Sepal Length     [cm]", main = "Iris setosa")
# # black line: kernel method; red line: smoothSplines result
# lines(density(iris1), col = "black", lwd = 1.5)
# xx1 <- seq(sol1$Xcp[1],tail(sol1$Xcp,n=1),length.out = sol1$NumPoints)
# lines(xx1,sol1$Y[1,], col = 'red', lwd = 2)
# 
# print("All is working! :)")
