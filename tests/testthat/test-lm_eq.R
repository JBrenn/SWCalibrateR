context("linear model equation")
library(SWCalibrateR)

test_that("Return object is class 'character'", {
  df = data.frame(Sample.VWC = c(1:10), Sensor.VWC = rep(c(1,2),5))
  result_lm <- lm_eq(df = df, method = 'lm')
  expect_is(result_lm, 'character')
  result_rlm <- lm_eq(df = df, method = 'rlm')
  expect_is(result_rlm, 'character')
})


