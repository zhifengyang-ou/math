#' plot the normal density and shade the area of P(x<a)
#'
#' @param mu the mean of normal distribution
#' @param sigma the sd of normal distribution
#' @param a for calulate the P(x<a)
#'
#' @return a plot contain the normal density and shading area, also, the probability of x< a is also printed
#' @export
#'
#' @examples mycurve(1,2,a=3)
myncurve = function(mu, sigma,a=a){
  curve(dnorm(x,mean=mu,sd=sigma), xlim = c(mu-3*sigma, mu + 3*sigma),ylab ="Normal density")
  # x values corresponding to the x - cords of points on the curve
  xcurve=seq(mu-3*sigma,a,length=1000)

  # Y values corresponding t0 the x values
  ycurve=dnorm(xcurve,mean=mu,sd=sigma)

  # Fill in the polygon with the given vertices
  polygon(c(mu-3*sigma,xcurve,a),c(0,ycurve,0),col="Red")
  # Put in the text with the appropriate area
  # Area
  prob=pnorm(a,mean=mu,sd=sigma)-pnorm(mu-3*sigma,mean=mu,sd=sigma)
  prob=round(prob,4)
  # add probability on plot
  print(list(area=prob))
}
