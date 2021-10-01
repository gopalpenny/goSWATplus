

set.seed(100)
x_obs <- 1:10 + rnorm(10)
x_sim <- 1:10

test_that("get_NSE returns good values for test dataset", {
  expect_equal(round(get_NSE(x_obs, x_sim), 5), 0.9644)
  expect_equal(round(get_NSE(x_obs, x_sim - 5), 5), -2.15061)
})




params_df <- tibble::tibble(param_names = c("x","y","z"),
                            values = c(1.1, 1.2, 3.4),
                            min = c(0.5, 1, 2.5), max = c(1.5, 3, 3.5))
func_objective <- function(params_df, vals = 1:3) {
  return(1 - get_NSE(params_df$values, vals))
}
func_objective(params_df)

set.seed(100)
dds_output <- calibrate_DDS(params_df, func_objective, m = 100) %>%
  dplyr::mutate(across(where(is.numeric), function(x) round(x, 4))) #%>%
# ggp::print_data_frame_for_entry()

dds_output_expected <-
  tibble::tibble(x=c(0.7828, 0.6665, 0.7776, 0.7717, 0.7495, 0.8645, 0.8645),
                 y=c(1.1837, 1.4695, 1.738, 2.1795, 2.0041, 1.9865, 1.9963),
                 z=c(3.3338, 3.1687, 2.979, 3.1677, 3.0237, 3.1617, 3.1617),
                 i=c(1, 2, 3, 5, 8, 93, 94),
                 obj_value=c(0.2192, 0.129, 0.0486, 0.0388, 0.0244, 0.0169, 0.0169),
                 new_best=c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE))

test_that("Ensure calibrate_DDS outputs simple calibration", {
  expect_equal(dds_output, dds_output_expected)
})
