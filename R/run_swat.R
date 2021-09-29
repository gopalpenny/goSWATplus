# run_swat.R

# swat_functions.R


#
run_swat <- function(scenario_path) {
  # print.prt - write to csv?
  # time.sim - start and end dates
  # scenario_path <- "/Documents and Settings/gopenny/Documents/SWAT models/gandak/gandak/Scenarios/calibrate/TxtInOut"
  current_wd <- getwd()
  setwd(scenario_path)
  swat_exe <- list.files(scenario_path, pattern = "\\.exe")
  system(swat_exe)
  setwd(current_wd)
}

# absval
# abschg
# pctchg

write_calibration <- function(cal_params_df) {

}



#' Update the calibration file structure with the parameter set of the current
#' simulation run_i
#'
#' @param thread_path Path to the current parallel thread 'thread_i'
#' @param parameter Model parameters as named vector or tibble
#' @param calibration Template table structure of the calibration file
#' @param i_run Index of the i_th simulation run
#'
#' @importFrom dplyr %>%
#' @importFrom purrr map2_df map_dbl
#' @importFrom readr write_lines
#'
#' @keywords internal
#'
# write_calibration <- function(thread_path, parameter, calibration, run_index,
#                               i_run) {
#   calibration$VAL <- parameter$values[run_index[i_run],] %>%
#     map_dbl(., ~.x) %>%
#     set_names(., parameter$definition$parameter) %>%
#     .[calibration$NAME] %>%
#     sprintf("%.15s", .)
#
#   col_format <- c("%-8s", "%8s", "%16s", rep("%8s", 8))
#
#   col_names <- names(calibration) %>%
#     sprintf(col_format, .) %>%
#     paste(., collapse = "")
#
#   calibration <- map2_df(calibration, col_format, ~sprintf(.y, .x)) %>%
#     apply(., 1, paste, collapse = "") %>%
#     c("Number of parameters:", sprintf("%2d",length(.)), col_names, .)
#
#   write_lines(calibration, thread_path%//%"calibration.cal")
# }

#
read_swat_data <- function(filename, path, vars = "all", swat_units = "all", date_range = "all") {
  # path <- "/Documents and Settings/gopenny/Documents/SWAT models/gandak/gandak/Scenarios/Default/TxtInOut/"
  # filename <- "lsunit_wb_day"
  # filename <- "aquifer_mon"
  # vars <- c("flo_out")
  # units <- 87
  # date_range <- c("1980-01-01", "1984-12-31")
  if (!grepl("\\.txt$",filename)) {
    filename <- paste0(filename,".txt")
  }
  vars_date <- c("date", "unit", vars)

  dt_names <- as.character(read.table(file.path(path, filename), skip = 1, nrows = 1, header = FALSE))
  if (vars[1] == "names") {
    return(dt_names)
  }
  dt <- data.table::fread(file.path(path, filename), header = FALSE, skip = 3)

  names(dt) <- dt_names
  dt[,`:=`(gis_id = NULL,
           name = NULL,
           date = as.Date(paste(yr, mon, day, sep = "-")))]


  dt_select <- dt
  if (vars[1] != 'all') {
    dt_select <- dt_select[, ..vars_date]
  }
  if (swat_units[1] != 'all') {
    dt_select <- dt_select[unit %in% swat_units]
  }
  if (date_range[1] != 'all') {
    dt_select <- dt_select[date >= date_range[1] & date <= date_range[2]]
  }


  return(dt_select[])
}

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

