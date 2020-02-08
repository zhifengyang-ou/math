#' Read csv file into table frame
#'
#' @param csv a csv file in a defined dird
#'
#' @return a table frame
#' @export
#'
#' @examples dird="C:\\Users\\stew9983\\OneDrive - University of Oklahoma\\DATAxls\\";
#' mpg.df=myread("EPAGAS.csv")


myread=function(csv){
  fl=paste(dird,csv,sep="")
  read.table(fl,header=TRUE,sep=",")
}
