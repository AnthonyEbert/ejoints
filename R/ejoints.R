


create_ejoint <- function(input, sigma){
  output <- list(input = input, sigma = sigma)
  return(output)
}


mvrnorm_ejoint <- function(mu, sigma){
  mu <- as.vector(mu)

  output <- MASS::mvrnorm(
    n = 1,
    mu = mu,
    Sigma = sigma * diag(length(mu))
  )
  return(output)
}

dmvnorm_ejoint <- function(mu, sigmai, x, threshold = 50){
  x <- as.vector(x)
  mu <- as.vector(mu)

  indic <- stats::dist(mu - x)/sigmai < threshold

  if(indic){
    output <- mvtnorm::dmvnorm(x, mean = mu, sigma = sigmai * diag(length(mu)))
  } else {
    output <- 0
  }

  return(output)
}

dnorm_ejoint <- function(mu, sigmai, x){
  x <- as.vector(x)
  mu <- as.vector(mu)


  output <- dnorm(x, mean = mu, sd = sigmai)

  return(output)
}


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
  input <- as.matrix(input)

  stopifnot(length(x) == dim(input)[2])

  output <- apply(input, 1, dmvnorm_ejoint, x = x, sigmai = sigma)

  return(sum(output)/dim(input)[1])
}

#' Sample from empirical joint density
#' @param x the point to be evaluated
#' @param input data in matrix form
#' @param sigma the standard deviation used to construct the empirical
#' @export
dejoint_1 <- function(x, input, sigma = 1){

  x <- as.vector(x)

  output <- sapply(input, dnorm_ejoint, x = x, sigmai = sigma)

  return(rowSums(output)/length(input))
}

dejoint2 <- function(x, y, input, sigma = 1){

  input <- as.matrix(input)


  output <- apply(input, 1, dmvnorm_ejoint, x = c(x,y), sigmai = sigma)

  return(sum(output)/dim(input)[1])
}
