

set.seed(100)
x_obs <- 1:10 + rnorm(10)
x_sim <- 1:10

test_that("get_NSE returns good values for test dataset", {
  expect_equal(round(get_NSE(x_obs, x_sim), 5), 0.9644)
  expect_equal(round(get_NSE(x_obs, x_sim - 5), 5), -2.15061)
})
