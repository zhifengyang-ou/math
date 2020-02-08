#' make a function of fitted result for quad.lm
#'
#' @param x a number, when it is not setted, the function return a function could be used in curve() to draw the fitted line.
#'
#' @return a number predicted by the quad.lm or used in curve()
#' @export
#'
#' @examples curve(myplot, lwd=2, col="steelblue")

myplot=function(x){
  quad.lm$coef[1] +quad.lm$coef[2]*x  + quad.lm$coef[3]*x^2
}
