#' Binomial Simulation
#'
#' This function produces a bar plot and table of the proportions of each potential outcome when given a number of iterations, iter; the number of samples per iteration, n; and the probability of success per sample, p.
#' Those outcomes will range from 0 successes in an iteration to n successes in an iteration.
#'
#' @param iter Number of iterations
#' @param n Number of samples taken per iteration
#' @param p Probability of success per sample
#'
#' @return Bar plot & table of proportions
#' @export mybin
#'
#' @examples mybin()
#'
mybin = function(iter = 1000,n = 10, p = 0.5){
  # make a matrix to hold the samples
  #initially filled with NA's
  sam.mat = matrix(NA,nrow = n,ncol = iter, byrow = TRUE) #'by row' fills matrix by going from left to right through rows top to bottom
  #Make a vector to hold the number of successes in each trial
  succ = c()
  for( i in 1:iter){
    #Fill each column with a new sample
    sam.mat[,i] = sample(c(1,0),n,replace = TRUE, prob = c(p,1-p))
    #Calculate a statistic from the sample (this case it is the sum)
    succ[i] = sum(sam.mat[,i])
  }
  #Make a table of successes
  succ.tab = table(factor(succ,levels = 0:n))
  #Make a barplot of the proportions
  graphics::barplot(succ.tab / (iter), col = grDevices::rainbow(n + 1), main = "Binomial simulation", xlab = "Number of successes")
  succ.tab/iter
}
