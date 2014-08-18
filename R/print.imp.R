print.imp <- function(x, ...){
  cat("\n --------------------------------------- \n")
  if( x$w > 1 ){
    print(paste(x$w, "missing values were imputed"))
  } else{
    print(paste(x$w, "missing value was imputed"))
  }

  if( length(x$criteria) > 0 ){
    if( x$iter > 1 ){
      print(paste(x$iter, "iterations were needed"))
    } else{
      print(paste(x$iter, "iteration was needed"))
    }
    print(paste("the last change was", round(x$criteria,4)))
  }
  cat(" --------------------------------------- \n")
}