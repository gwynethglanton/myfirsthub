#' @title Read csv
#'
#' @param csv file name
#' @param dird directory to data
#'
#' @return reading a file
#' @export
#' @importFrom utils read.table
#' @examples
#' \dontrun{myread(csv,dird)}
myread=function(csv, dird){
  fl=paste(dird,csv,sep="")
  read.table(fl,header=TRUE,sep=",")
}
