


mvrnorm_ejoint <- function(mu, sigma){
  mu <- as.vector(mu)

  output <- MASS::mvrnorm(
    n = 1,
    mu = mu,
    Sigma = sigma * diag(length(mu))
  )
  return(output)
}

dmvnorm_ejoint <-
  function(mu, sigmai, x, threshold = 50, multi, support) {
    x <- as.vector(x)
    mu <- as.vector(mu)

    indic <-
      ifelse(multi,
             stats::dist(mu - x) / sigmai < threshold,
             (mu - x) / sigmai < threshold)

    if (multi) {
      dens_fun <- function(...) {
        mvtnorm::dmvnorm(mean = mu, sigma = sigmai * diag(length(mu)), ...)
      }
    } else {
      dens_fun <- function(...) {
        dnorm_cs(mean = mu,
                 sd = sigmai,
                 support = support)
      }
    }

    if (indic) {
      output <-
        mvtnorm::dmvnorm(x, mean = mu, sigma = sigmai * diag(length(mu)))
    } else {
      output <- 0
    }

    return(output)
  }



dnorm_cs <- function(x, mean, sd, support){

  output <- dnorm(x, mean, sd)

  if(is.null(support)){
    weights <- 1
  } else {
    weights <- w_h(x, sd)
  }

  return(output*weights)
}




w_h <- function(x, h){
  if(x < h){
    output <- 1/(pnorm(1) - pnorm(-x/h))
  }

  if(x > 1 - h){
    output <- 1/(pnorm((1-x)/h) - pnorm(-1))
  }

  if((x >= h) & (x < 1-h)){
    output <- 1
  }

  return(output)
}









compute_weights <- function(mu, sigmai, x, threshold = 50, support = support){




