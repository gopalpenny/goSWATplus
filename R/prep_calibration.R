# prep_calibration.R

write_calibration_cal <- function(swatTxtInOut_path, calibration_df) {
  calibration_df$VAL <- calibration_df$VAL %>%
    sprintf("%.15s", .)

  col_format <- c("%-8s", "%8s", "%16s", rep("%8s", 8))

  col_names <- names(calibration_df) %>%
    sprintf(col_format, .) %>%
    paste(., collapse = "")

  calibration_write <- map2_df(calibration_df, col_format, ~sprintf(.y, .x)) %>%
    apply(., 1, paste, collapse = "") %>%
    c("Number of parameters:", sprintf("%2d",length(.)), col_names, .)

  # write_lines(calibration_write, file.path(swatTxtInOut_path,"calibration.cal"))
  print(calibration_write)
}

#' Add calibration parameter
#'
#' @param calibration_df \code{NULL} or tibble from previous \code{add_cal_parm}
#' @param param Parameter name
#' @param change_type One of absval, abschng, pctchng,
#' @param val Value of change
#' @param conds Not implemented
#' @param layers Numeric vector length two, first to last soil layer
#' @param dates Date vector of length two, first to last date for param to apply
#' @details
#' This function is used to build a dataframe for calibration, specifically
#' to write the calibration file calibration.cal using write_calibration_cal().
#' The function is pipe-friendly for adding multiple parameters.
#' @examples
#' calibration_df <- add_cal_parm(param = "cn2", change_type = "pctchng", val = 10)
#' calibration_df2 <- calibration_df %>%
#'   add_cal_parm(param = "cn3", change_type = "pctchng", val = 20)
add_cal_parm <- function(calibration_df = NULL, param, change_type, val,
                         conds = NULL, layers = NULL, dates = NULL) {
  if (!is.null(dates)) {
    years <- as.integer(strftime(dates), "%Y")
    days <- as.integer(strftime(dates), "%j")
  } else {
    years <- c(0, 0)
    days <- c(0, 0)
  }
  if (!is.null(layers)) {
    lyr <- layers
  } else {
    lyr <- c(0, 0)
  }
  new_row <- tibble(NAME = param,
                    CHG_TYPE = change_type,
                    VAL = val,
                    CONDS = 0,
                    LYR1 = lyr[1], LYR2 = lyr[2],
                    YEAR1 = years[1], YEAR2 = years[2],
                    DAY1 = days[1], DAY2 = days[2],
                    OBJ_TOT = 0)
  if (is.null(calibration_df)) {
    new_calibration_df <- new_row
  } else {
    new_calibration_df <- calibration_df %>%
      bind_rows(new_row)
  }
  return(new_calibration_df)
}

# write_calibration_cal("hi", calibration_df2)
