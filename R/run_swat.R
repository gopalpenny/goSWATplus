# run_swat.R

# swat_functions.R


#' Run SWAT executable file
#'
#' @param scenario_path Path containing TxtInOut directory with SWAT executable
#' @details Runs the first executable file in \code{scenario_path}
#' @export
run_swat <- function(scenario_path, params_df = NULL) {

  if(!is.null(params_df)) {
    for (i in 1:nrow(params_df)) {
      if (i == 1) {
        calibration_df <-
          add_cal_parm(param = params_df$param_name[i],
                       change_type = params_df$change_type[i],
                       val = params_df$values[i])
      } else{
        calibration_df <- calibration_df %>%
          add_cal_parm(param = params_df$param_name[i],
                       change_type = params_df$change_type[i],
                       val = params_df$values[i])
      }
    }
    write_calibration_cal(scenario_path, calibration_df)
  }


  # print.prt - write to csv?
  # time.sim - start and end dates
  # scenario_path <- "/Documents and Settings/gopenny/Documents/SWAT models/gandak/gandak/Scenarios/calibrate/TxtInOut"
  current_wd <- getwd()
  setwd(scenario_path)
  swat_exe <- list.files(scenario_path, pattern = "\\.exe")
  if (length(swat_exe) == 0) {
    stop("No executable files found in the SWAT scenario_path")
  } else if (length(swat_exe) > 1) {
    warning("Multiple executable files found in the SWAT scenario_path. Using",swat_exe[1],"\n")
  }

  cat("Running", swat_exe[1], "in", scenario_path,"...")
  system(swat_exe[1])
  cat("Done.\n")
  setwd(current_wd)
}

#' Read SWAT+ output data
#'
#' @param filename Name of SWAT+ output file
#' @param path Path to SWAT+ output files
#' @param vars Variables to read from the file. See details
#' @param swat_units Vector of spatial units to select from the "unit" column
#' @param date_range Date vector (length 2) specifying the date range to read
#' @export
#' @importFrom utils read.table
#' @importFrom data.table `:=`
#' @details
#' This function is used to read SWAT+ output data into a data.frame format.
#' Data is read from the \code{filename} in the \code{path} directory. The
#' \code{vars} parameter specifies the columns to read and the \code{swat_units}
#' specifies the units to select.
#'
#' If \code{vars} is set to \code{"names"}, the function returns a vector of
#' variable names contained in the file.
#' @return
#' The function returns a \code{data.table} object, which is useful for large
#' datasets. If desired, it can easily be converted to a \code{data.frame} using
#' \code{as.data.frame}.
read_swat_data <- function(filename, path, vars = "all", swat_units = "all", date_range = "all") {
  # path <- "/Documents and Settings/gopenny/Documents/SWAT models/gandak/gandak/Scenarios/Default/TxtInOut/"
  # filename <- "lsunit_wb_day"
  # filename <- "aquifer_mon"
  # vars <- c("flo_out")
  # units <- 87
  # date_range <- c("1980-01-01", "1984-12-31")
  yr <- mon <- day <- ..vars_date <- unit <- NULL


  if (!grepl("\\.txt$",filename)) {
    filename <- paste0(filename,".txt")
  }
  vars_date <- c("date", "unit", vars)

  dt_names <- as.character(read.table(file.path(path, filename), skip = 1, nrows = 1, header = FALSE))
  if (vars[1] == "names") {
    return(dt_names)
  }
  dt <- data.table::fread(file.path(path, filename), header = FALSE, skip = 3)

  if (dim(dt)[2] != length(dt_names)) {
    dt_units <- as.character(read.table(file.path(path, filename), skip = 2, nrows = 1, header = FALSE))

    # fix for SWAT+59.3
    dt_additional_names <- dt_units[1:(dim(dt)[2] - length(dt_names))]
    dt_names <- c(dt_additional_names,dt_names)
  }
  names(dt) <- dt_names
  dt[,`:=`(gis_id = NULL,
           name = NULL,
           datestr = paste(yr, mon, day, sep = "-"))]
  dt[,`:=`(date = as.Date(datestr, format = "%Y-%m-%d"))]


  dt_select <- dt
  if (vars[1] != 'all') {
    dt_select <- dt_select[, .SD, .SDcols=vars_date]
  }
  if (swat_units[1] != 'all') {
    dt_select <- dt_select[unit %in% swat_units]
  }
  if (date_range[1] != 'all') {
    dt_select <- dt_select[date >= date_range[1] & date <= date_range[2]]
  }


  return(dt_select[])
}

#' Run SWAT NSE
#'
#'


# # swat_lsu_shp_path <- "C:/Users/gopenny/Documents/SWAT models/gandak/gandak/Watershed/Shapes/lsus1.shp"
# # lsu_wb_mon_path <- file.path(swat_txtinout_path,"lsunit_wb_mon.txt")
# # get_lsu_basin_wb(lsu_wb_mon_path, swat_lsu_shp_path)
# get_lsu_basin_wb <- function(lsu_wb_mon_path, swat_lsu_shp_path) {
#
#   # read lsu shapefile
#   swat_lsus_sf <- sf::read_sf(swat_lsu_shp_path)
#
#   # read lsu_wb_mon
#   lsu_names <- as.character(read.table(lsu_wb_mon_path, skip = 1, nrows = 1, header = FALSE))
#   lsu <- read_table(lsu_wb_mon_path, skip = 3, col_names = FALSE) %>%
#     setNames(lsu_names) %>%
#     dplyr::filter(unit %in% devghat_lsus_sf$Channel) %>%
#     mutate(date = as.Date(paste(yr, mon, day, sep = "-")),
#            yearless_date = as.Date(paste("0000", mon, day, sep = "-")),
#            year  = yr) %>%
#     left_join(swat_lsus_sf %>% st_set_geometry(NULL) %>% select(area_ha = Area, unit = Channel, Subbasin), by= "unit")
#   # dplyr::select(all_of(water_balance_vars))N
#
#
#   wb_subbasin <- lsu %>%
#     pivot_longer(precip:latq_ls, names_to = "variable", values_to = "value_mm") %>%
#     mutate(variable = factor(variable, levels = lsu_names, labels = paste0(lsu_names,"_mm"))) %>%
#     group_by(date, jday, mon, day, yr, variable, Subbasin) %>%
#     summarize(value_mm = weighted.mean(value_mm, area_ha),
#               area_ha = sum(area_ha))#
#
#   wb_subbasin_wide <- wb_subbasin %>%
#     pivot_wider(names_from = variable, values_from = value_mm)
#
#   return(wb_subbasin_wide)
# }

