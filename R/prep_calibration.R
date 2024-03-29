# prep_calibration.R

#' Write calibration file
#'
#' @param swatTxtInOut_path Path to SWAT scenario directory
#' @param calibration_df data.frame with calibration data (see details)
#' @export
#' @details
#' This function writes the calibration file calibration.cal in the directory
#' \code{swatTxtInOut_path}.
#'
#' The input parameter \code{calibration_df} should contain columns created by
#' \code{add_cal_parm}, i.e.:
#' \itemize{
#' \item NAME
#' \item CHG_TYPE - one of "absval", "abschg", "pctchg"
#' \item VAL
#' \item CONDS
#' \item LYR1
#' \item LYR2
#' \item YEAR1
#' \item YEAR2
#' \item DAY1
#' \item DAY2
#' \item OBJ_TOT
#' }
#' @importFrom purrr map2_df
#' @importFrom readr write_lines
#' @examples
#' calibration_df <-
#'   add_cal_parm(param = "cn2", change_type = "pctchg", val = 10) %>%
#'   add_cal_parm(param = "cn3", change_type = "pctchg", val = 20)
#' write_calibration_cal(tempdir(), calibration_df)
#' readLines(file.path(tempdir(), "calibration.cal"))
write_calibration_cal <- function(swatTxtInOut_path, calibration_df) {

  permittable_change_types <- c("absval", "abschg", "pctchg")
  if(!all(calibration_df$CHG_TYPE %in% permittable_change_types)) {
    stop('CHG_TYPE must be one of "absval", "abschg", or "pctchg"')
  }

  calibration_cal_path <- file.path(swatTxtInOut_path, "calibration.cal")
  calibration_df$VAL <- sprintf("%.15s", calibration_df$VAL)

  col_format <- c("%-8s", "%8s", "%16s", rep("%8s", 8))

  col_names <- sprintf(col_format, names(calibration_df)) %>%
    paste(collapse = "")

  calibration_text <- purrr::map2_df(calibration_df, col_format, ~sprintf(.y, .x)) %>%
    apply(1, paste, collapse = "")
  calibration_write <- c("Number of parameters:", sprintf("%2d",length(calibration_text)), col_names, calibration_text)

  write_lines(calibration_write, calibration_cal_path)
  # print(calibration_write)
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
#' @export
#' @details
#' This function is used to build a dataframe for calibration, specifically
#' to write the calibration file calibration.cal using write_calibration_cal().
#' The function is pipe-friendly for adding multiple parameters.
#' @examples
#' calibration_df <- add_cal_parm(param = "cn2", change_type = "pctchg", val = 10)
#' calibration_df2 <- calibration_df %>%
#'   add_cal_parm(param = "cn3", change_type = "pctchg", val = 20)
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

  permittable_change_types <- c("absval", "abschg", "pctchg")
  if(!(change_type %in% permittable_change_types)) {
    stop('change_type must be one of "absval", "abschg", or "pctchg"')
  }

  new_row <- tibble::tibble(NAME = param,
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
      dplyr::bind_rows(new_row)
  }
  return(new_calibration_df)
}

