context("mm-Type estimation with robustbase::lmrob")
library(SWCalibrateR)

test_that("Return object is class 'lmrob'", {
  df <- data.frame(a = rep(c(1,2), 5), b = 1:10)  
  result <- fitSMDM(formula = a ~ b, data = df)
  expect_is(result, 'lmrob')
})

test_that("Error due to exact fit", {
  df <- data.frame(a = 1:10, b = 1:10)  
  expect_error(fitSMDM(formula = a ~ b, data = df))
})  

test_that("Resulting r^2 and coefficients", {
  df <- data.frame(a = rep(c(1,2), 5), b = 1:10)  
  result1 <- summary(fitSMDM(formula = a ~ b, data = df))
  result2 <- summary(robustbase::lmrob(a~b, df, setting = "KS2011", maxit.scale = 1000))
  expect_equal(result1$coefficients, result2$coefficients)
})    
