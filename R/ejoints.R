






#' Sample from empirical joint density
#' @param n number of samples required
#' @param input data in matrix form
#' @param sigma the standard deviation used to construct the empirical density
#' @export
rejoint <- function(n, input, sigma = 1){

  input <- as.matrix(input)
  m <- dim(input)[1]

  sample_i <- sample.int(m, n, replace = TRUE)

  output <- apply(input[sample_i,], 1, mvrnorm_ejoint, sigma = sigma)

  return(t(output))
}


#' Sample from empirical joint density
#' @param x the point to be evaluated
#' @param input data in matrix form
#' @param sigma the standard deviation used to construct the empirical
#' @export
dejoint <- function(x, input, sigma = 1){

  x <- as.vector(x)

  multi <- ifelse(length(x) == 1, FALSE, TRUE)
  input <- as.matrix(input)

  stopifnot(length(x) == dim(input)[2])

  output <- apply(input, 1, dmvnorm_ejoint, x = x, sigmai = sigma, multi = multi, support = NULL)

  return(sum(output)/dim(input)[1])
}
