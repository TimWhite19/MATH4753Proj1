#' Normal Probability Distribution
#'
#' This function takes an input mean, standard deviation, and an a value and makes a normal curve with the lower tail area below a shaded in. The area of this shaded region is determined and displayed.
#'
#' @param mu mean
#' @param sigma standard deviation
#' @param a probability
#'
#' @return Normal plot with shaded area probability
#' @export
#'
#' @examples
#' \dontrun{myncurve(mu = 3, sigma = 5, a = 6)}
#'
myncurve = function(mu = 3, sigma = 5, a = 6) {
  graphics::curve(stats::dnorm(x,mean=mu,sd=sigma), xlim = c(mu-3*sigma, mu + 3*sigma))

  #Shades desired area
  xcurve = seq(-100, a, length = 1000)
  ycurve = stats::dnorm(xcurve, mu, sigma)
  graphics::polygon(c(-100, xcurve, a), c(0,ycurve,0), col = "Purple")

  # Calculates area with 4 decimal places
  area = round(stats::pnorm(a, mu, sigma), 4)
  paste("Area = ", area)
  graphics::text(x = a+sigma, y = max(ycurve)/8, paste("Area = ", area, sep = ""))
}
