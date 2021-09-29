# do_calibration.R

#' Get Nash-Sutcliffe Efficiency
#'
#' @param x_obs Vector of observations
#' @param x_sim Vector of simulated data
#' @return
#' Returns the NSE between \code{x_obs} and \code{x_sim}
#' @examples
#' set.seed(100)
#' x_obs <- 1:10 + rnorm(10)
#' x_sim <- 1:10
#' get_NSE(x_obs, x_sim)
#' get_NSE(x_obs, x_sim - 5)
get_NSE <- function(x_obs, x_sim) {
  x_obs_mean <- mean(x_obs)
  SSE <- sum((x_obs - x_sim)^2)
  SST <- sum((x_obs - x_obs_mean)^2)

  NSE <- 1 - SSE / SST
  return(NSE)
}
