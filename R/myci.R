#' Calculate the confidence interval of a small size of sample
#'
#' @param d data array contains data, the data size <30
#' @param conf.int the confidence,default 95%
#'
#' @return a confidence interval
#' @export
#'
#' @examples set.seed(23);x = rnorm(30,mean=10,sd=12)
#' yourpackage::myci(x)

my_ci=function(d,conf.int=0.95){
  t=qt(1-(1-conf.int)/2,df=length(d)-1)
  ci=c()
  ci[1]=mean(d)-t*sd(d)/sqrt(length(d))
  ci[2]=mean(d)+t*sd(d)/sqrt(length(d))
  ci
}
