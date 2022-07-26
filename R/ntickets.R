#' nticket function
#'
#' This function computes and plots the ideal number of tickets to sell to optimize the number of seats filled based on number of tickets sold. 2 plots are made, one depicting a discrete distribution, and one depicting a continuous distribution.
#'
#' @param N Number of seats available on the plane
#' @param gamma probability that a plan will be truly overbooked
#' @param p probability that an individual will show up for their flight
#'
#' @return 2 plots and a list containing nd, nc, N, p, and gamma
#' @export
#'
#' @examples ntickets(N = 200, gamma = 0.02, p = 0.95)
ntickets = function(N = 200, gamma = 0.02, p = 0.95){
  #discrete distribution
  n <- N:round(N*1.15,0)

  fn <- 1 - gamma - stats::pbinom(N, n, p)
  obj <- abs(fn)

  root <- which.min(obj)
  nd = n[root]

  plot(n,fn,main = c(paste("Ojective Vs n for optimal number of tickets sold (n = ", nd, ")"),paste(" gamma = ",gamma, " N = ",N, " Discrete Case")), ylab = "Objective", xlab = "n",ylim = c(-.1,1), xlim=c(N,N+20))
  lines(n,fn, xlim = (range(n)), ylim = range(fn))
  graphics::abline(h = 0,v = nd, col = "red")

  #normal distribution
  n <- seq(N, N*1.5 , length = 500)
  fn <- 1 - gamma - stats::pnorm(N+0.5, n*p, sqrt(n*p*(1-p)))

  obj <- abs(fn)
  root <- which.min(obj)
  nc <- n[root]

  plot(n,fn,main = c(paste("Ojective Vs n for optimal number of tickets sold (n = ", round(nc,9), ")"),paste(" gamma = ",gamma, " N = ",N, " Continuous Case")),ylab = "Objective", xlab = "n",ylim = c(-.1,1), xlim=c(N,N+20), type = "l", lwd = 2)
  graphics::abline(h = 0,v = nc, col = "red")

  print(c(nd = nd, nc = nc, N = N, p = p, gamma = gamma))
}

