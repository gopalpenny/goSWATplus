

calibration_df <-
  add_cal_parm(param = "cn2", change_type = "pctchg", val = 10) %>%
  add_cal_parm(param = "cn3", change_type = "pctchg", val = 20)
write_calibration_cal(tempdir(), calibration_df)
cal_contents <- readLines(file.path(tempdir(), "calibration.cal"))
# paste(cal_contents, collapse = "','")
cal_results <-
  c('Number of parameters:',
    ' 2',
    'NAME    CHG_TYPE             VAL   CONDS    LYR1    LYR2   YEAR1   YEAR2    DAY1    DAY2 OBJ_TOT',
    'cn2       pctchg              10       0       0       0       0       0       0       0       0',
    'cn3       pctchg              20       0       0       0       0       0       0       0       0')
test_that("write_calibration_cal works for simple 2-line example", {
  expect_equal(cal_contents, cal_results)
})
