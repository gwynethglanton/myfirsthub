#' @title my maximum likelihood function
#'
#' @param lfun the l function
#' @param x the x axis defined
#' @param param the parameters
#' @param ... more functions can be added
#'
#' @return a maximum likelihood estimate
#' @export
#' @importFrom graphics axis points
#'
#' @examples \dontrun{mymaxlik(lfun, x, param,...)}
mymaxlik=function(lfun, x, param,...){

  #finds number of parameters
  np=length(param)

  #a matrix of the product of x and param
  z=outer(x,param,lfun)

  #creates a new matrix with the sum of each row
  y=apply(z,2,sum)

  #plots new matrix
  plot(param,y,col="Blue",type="l",lwd=2,...)

  #produces the index and plots a vertical line at the max value of y
  i=max(which(y==max(y)))
  abline(v=param[i],lwd=2,col="Red")

  #plots where the max likelihood is
  points(param[i],y[i],pch=19,cex=1.5,col="Black")
  axis(3,param[i],round(param[i],2))

  #plots slope
  ifelse(i-3>=1 & i+2<=np, slope<-(y[(i-2):(i+2)]-y[(i-3):(i+1)])/(param[(i-2):(i+2)]-param[(i-3):(i+1)]),slope<-"NA")
  return(list(i=i,parami=param[i],yi=y[i],slope=slope))
}
