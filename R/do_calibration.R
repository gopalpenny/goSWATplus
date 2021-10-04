# do_calibration.R

#' Get Nash-Sutcliffe Efficiency
#'
#' @param x_obs Vector of observations
#' @param x_sim Vector of simulated data
#' @export
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


#' Calibrate DDS
#'
#' Calibrate via dynamically dimensioned search (DDS)
#' @param params_df A data.frame containing parameterinfo (see details)
#' @param objective_function Objective function to be minimized (see details)
#' @param ... Additional inputs to \code{objective_function}
#' @param r Variables are updated with a normal distribution and sd = r * N(1,0)
#' @param m Number of iterations over which to calibrate
#' @param best_only Boolean, indicates whether to filter output (see Return)
#' @export
#' @details
#' This function executes the Dynamically dimensioned search algorithm
#' by Tolson and Shoemaker (2007), which uses a random approach to variable
#' selection and updates for calibration purposes. A random selection of
#' parameters are varied for each calibration step. At the beginning of the
#' algorithm, most parameters are varied whereas when the algorithm approaches
#' the upper bound, fewer parameters are updated.
#'
#' Input \code{param_df} contains the necessary information to run the DDS
#' calibration algorithm. It contains columns:
#' \itemize{
#' \item \code{param_name}
#' \item \code{min}
#' \item \code{max}
#' \item \code{values}: Initial values for calibration
#' }
#' The \code{objective_function} should take as inputs a data.frame containing
#' columns for \code{param_names} and \code{values}. Additional arguments will
#' be passed through \code{...}.
#'
#' Tolson, B.A. and Shoemaker, C.A., 2007. Dynamically dimensioned search
#' algorithm for computationally efficient watershed model calibration. Water
#' Resources Research, 43(1).
#' @return
#' Returns a \code{tibble} with parameter values for each calibration step. If
#' \code{best_only} is \code{TRUE}, only rows where the calibration improved
#' are returned.
#' @examples
#' params_df <- tibble(param_names = c("x","y","z"),
#'                     values = c(1.1, 1.2, 3.4),
#'                     min = c(0.5, 1, 2.5), max = c(1.5, 3, 3.5))
#' example_objective_function <- function(params_df, vals = 1:3) {
#'   return(1 - get_NSE(params_df$values, vals))
#' }
#' example_objective_function(params_df)
#'
#' set.seed(100)
#' dds_output <- calibrate_DDS(params_df, example_objective_function, m = 100)
#' View(dds_output)
calibrate_DDS <- function(params_df, objective_function, ..., r = 0.2, m = 10, best_only = TRUE) {

  n_params <- nrow(params_df)
  params_df$sigma <- (params_df$max - params_df$min) * r

  # start with initial run
  # params_df$values <- params_df$initial
  obj_value <- objective_function(params_df)
  params_df$best <- params_df$values
  obj_best <- obj_value

  dds_outcomes <- params_df %>% dplyr::select(c("param_names", "values")) %>%
    tidyr::pivot_wider(names_from = "param_names", values_from = "values") %>%
    dplyr::bind_cols("i" = 0, "obj_value" = obj_value)

  for (i in 1:m) {
    update_params_bool <- runif(n_params) > log(i) / log(m) # select which params to update
    if (!any(update_params_bool)) { # if none are set to update, select one
      update_params_bool[sample(1:n_params, 1)] <- TRUE
    }

    params_df$new_val <- params_df$best + params_df$sigma * rnorm(n_params, mean = 0, sd = 1)
    # set to extreme if there is a double reflection
    params_df$new_val <- with(params_df, ifelse(new_val < min - (max - min), min, new_val))
    params_df$new_val <- with(params_df, ifelse(new_val > max + (max - min), max, new_val))
    # mirror if single reflection
    params_df$new_val <- with(params_df, ifelse(new_val < min, min + (min - new_val), new_val))
    params_df$new_val <- with(params_df, ifelse(new_val > max, max - (new_val - max), new_val))
    # update values
    params_df$values <- with(params_df, ifelse(update_params_bool, new_val, values))

    # re-calculate objective function
    obj_value <- objective_function(params_df)

    new_outcome <- params_df %>% dplyr::select(c("param_names", "values")) %>%
      tidyr::pivot_wider(names_from = "param_names", values_from = "values") %>%
      dplyr::bind_cols("i" = i, "obj_value" = obj_value, "new_best" = obj_value < obj_best)

    if (obj_value < obj_best) {
      params_df$best <- params_df$values
      obj_best <- obj_value
    }

    dds_outcomes <- dds_outcomes %>% dplyr::bind_rows(new_outcome)
  }

  if(best_only) {
    dds_outcomes <- dds_outcomes %>% dplyr::filter(new_best)
  }
  return(dds_outcomes)
}
