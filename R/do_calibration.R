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
#' @param print_progress Either \code{"none"}, \code{"bar"} for txtProgressBar,
#'  \code{"iter"} to print iteration, or \code{"iter_dt"} to include datetime
#' @param save_path If used, path to directory to save output as "calibrate_dds_outcomes.csv"
#' @param debug print calibrate_df each simulation if TRUE.
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
#' \item \code{val}: Initial values for calibration
#' }
#' The \code{objective_function} should take as inputs a data.frame containing
#' columns for \code{param_names}, which identify the parameters and
#' \code{val}, which contain the updated values. Additional arguments will
#' be passed through \code{...} and the \code{data.frame} will contain
#' all of the original columns.
#'
#' Note that only parameters with \code{params_df$max > params_df$min} will be
#' calibrated, meaning that it is possible to inclue parameters that will be
#' skipped by the algorithm.
#'
#' `save_path_csv` is used to save the results of the calibration, including
#' the iteration number, parameters, and objective function value. If `save_path_csv`
#' is specified, the simulation will check for prior simulations when
#' the function is called, and will re-start where it left off if
#' earlier simulations exist.
#'
#' Tolson, B.A. and Shoemaker, C.A., 2007. Dynamically dimensioned search
#' algorithm for computationally efficient watershed model calibration. Water
#' Resources Research, 43(1).
#' @return
#' Returns a \code{tibble} with parameter val for each calibration step. If
#' \code{best_only} is \code{TRUE}, only rows where the calibration improved
#' are returned.
#' @examples
#' library(tibble)
#' # note: calibrate x, y, z. parameter n is not calibrated as min == max
#' params_df <- tibble(param_names = c("x","y","z","n"),
#'                     val = c(1.1, 1.2, 3.4, 4.1),
#'                     min = c(0.5, 1, 2.5, 4.1), max = c(1.5, 3, 3.5, 4.1))
#' example_objective_function <- function(params_df, vals) {
#'   return(1 - get_NSE(params_df$val, vals))
#' }
#' example_objective_function(params_df, vals = 1:4)
#'
#' set.seed(100)
#' dds_output <- calibrate_DDS(params_df, example_objective_function, vals = 1:4, m = 100)
#' View(dds_output)
calibrate_DDS <- function(params_df, objective_function, ..., r = 0.2, m = 10, best_only = TRUE,
                          print_progress = "none", save_path = NULL, debug = FALSE) {

  # if prev_run is true
  prev_run <- FALSE
  if (!is.null(save_path)) {
    save_path_csv <- file.path(save_path, "calibrate_DDS_outcomes.csv")
    if(file.exists(save_path_csv)) {
      prev_run <- TRUE
    }
  }

  calibration_params_idx_bool <- params_df$max > params_df$min
  calibration_params_idx <- which(calibration_params_idx_bool)
  n_calibrate <- length(calibration_params_idx)
  n_params <- nrow(params_df)
  params_df$sigma <- (params_df$max - params_df$min) * r

  if (prev_run) {

    # stop("LOAD PREV RUN NOT WORKING. REMOVE CURRENT OUTPUT AND RERUN. SEE CODE COMMENTS")
    # Right now this section of code needs to be fixed.
    dds_outcomes <- readr::read_csv(save_path_csv)
    start_i <- nrow(dds_outcomes) # note: first row is i=0, so nrow() is i + 1
    best_rows <- dds_outcomes %>%
      dplyr::filter(dds_outcomes$new_best)
    current_values <- best_rows[nrow(best_rows),
                                -which(names(best_rows) %in% c("i","obj_value","new_best"))] %>%
      tidyr::pivot_longer(dplyr::everything(), names_to = "param_names", values_to = "best")

    params_df <- params_df %>% dplyr::left_join(current_values, by = "param_names") %>%
      dplyr::mutate(val = best)
    obj_best <- best_rows$obj_value[nrow(best_rows)]

  } else {
    # start with initial run
    # params_df$val <- params_df$initial
    if (print_progress == "bar") {
      pb = utils::txtProgressBar(min = 0, max = m, initial = 0)
    } else if (print_progress == "iter") {
      cat("iteration: 0\n\n")
    } else if (print_progress == "iter_dt") {
      cat("iteration: 0 --",date(),"\n\n")
    }
    obj_value <- objective_function(params_df, ..., iter = 0)
    params_df$best <- params_df$val
    obj_best <- obj_value

    dds_outcomes <- params_df %>% dplyr::select(c("param_names", "val")) %>%
      tidyr::pivot_wider(names_from = "param_names", values_from = "val") %>%
      dplyr::bind_cols("i" = 0, "obj_value" = obj_value, new_best = TRUE)

    start_i <- 1
  }


  for (i in start_i:m) {
    if (print_progress == "bar") {
      utils::setTxtProgressBar(pb, value = i)
    } else if (print_progress == "iter") {
      cat("iteration:",i,"\n\n")
    } else if (print_progress == "iter_dt") {
      cat("iteration:",i,"--",date(),"\n\n")
    }


    update_params_bool <- runif(n_params) > log(i) / log(m) # select which params to update
    update_params_bool[!calibration_params_idx_bool] <- FALSE
    if (!any(update_params_bool)) { # if none are set to update, select one
      update_params_bool <- rep(FALSE, n_params)
      update_params_bool[sample(calibration_params_idx, 1)] <- TRUE
    }

    params_df$new_val <- params_df$best + params_df$sigma * rnorm(n_params, mean = 0, sd = 1)
    # set to extreme if there is a double reflection
    params_df$new_val <- with(params_df, ifelse(new_val < min - (max - min), min, new_val))
    params_df$new_val <- with(params_df, ifelse(new_val > max + (max - min), max, new_val))
    # mirror if single reflection
    params_df$new_val <- with(params_df, ifelse(new_val < min, min + (min - new_val), new_val))
    params_df$new_val <- with(params_df, ifelse(new_val > max, max - (new_val - max), new_val))
    # update values
    params_df$val <- with(params_df, ifelse(update_params_bool, new_val, best))

    # for debugging
    if(debug) {
      print(i)
      print(m)
      print(params_df)
      print(update_params_bool)
    }

    # re-calculate objective function
    obj_value <- objective_function(params_df, ..., iter = i)

    new_outcome <- params_df %>% dplyr::select(c("param_names", "val")) %>%
      tidyr::pivot_wider(names_from = "param_names", values_from = "val") %>%
      dplyr::bind_cols("i" = i, "obj_value" = obj_value, "new_best" = obj_value < obj_best)

    if (obj_value < obj_best) {
      params_df$best <- params_df$val
      obj_best <- obj_value
    }

    dds_outcomes <- dds_outcomes %>% dplyr::bind_rows(new_outcome)

    if(!is.null(save_path)) {
      write.csv(dds_outcomes, save_path_csv, row.names = FALSE)
    }

    # function_after_each_sim()
  }

  if(best_only) {
    dds_outcomes <- dds_outcomes %>% dplyr::filter(new_best)
  }
  return(dds_outcomes)
}
