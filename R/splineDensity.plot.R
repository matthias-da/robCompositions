#' @rdname plot.smoothSpl
#' @name plot.smoothSpl
#' @title plot smoothSpl
#' @description plot densities of objects of class smoothSpl
#' @param x class smoothSpl object
#' @param y ignored
#' @param by stepsize
#' @param n length of sequence to plot
#' @param index optinally the sequence instead of by and n
#' @param ... further arguments passed by
#' @method plot smoothSpl
#' @author Alessia Di Blasi, Federico Pavone, Gianluca Zeni
#' @export
plot.smoothSpl <- function(x, y, ..., by = 1 , n = 10, index = NULL){
  xx <- seq(x$Xcp[1],utils::tail(x$Xcp,n=1),length.out = x$NumPoints)
  n <- min(n,dim(x$Y)[1])
  cols <- grDevices::rainbow(min(n,30))
  if(is.null(index)) {
    whitch <- seq(1,n,by=by)
  } else {
    whitch <- index
  }
  # Plotting in the clr space fitted curves
  graphics::plot.default(xx,x$Y_clr[1,],ylim=c(min(x$Y_clr[whitch,]),max(x$Y_clr[whitch,])),
               type = "n", xlab = "", ylab = "")
  graphics::title("Smoothing splines in clr-transformed space")
  for(i in whitch){
    graphics::lines(xx,x$Y_clr[i,], col = cols[i%%length(cols) + 1])
  }

  # Plotting densities in orginal space
  
  graphics::plot.default(xx,x$Y[1,],ylim=c(min(x$Y[whitch,]),max(x$Y[whitch,])),
               type = "n", xlab = "", ylab = "")
  graphics::title("Density")
  for(i in whitch){
    graphics::lines(xx,x$Y[i,], col = cols[i%%length(cols) + 1])
  }
}
