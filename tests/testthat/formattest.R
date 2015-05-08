test_that("incorrectly formatted predictors throw an error"), {
  
  x1 = rnorm(100, mean = 0, sd = 1)
  x2 = rnorm(100, mean = 0.1, sd = 1.1)
  x3 = rnorm(100, mean = -0.1, sd = 0.9)
  x4 = rnorm(100, mean = 0, sd = 1)
  x5 = rnorm(100, mean = 3, sd = 1)
  x6 = rnorm(100, mean = 0.5, sd = 2)
  x7 = rnorm(100, mean = -0.5, sd = 3)
  predictions = cbind(x1, x2, x3, x4, x5, x6, x7)
  
  expect_error(eigensml(predictions))
  expect_error(svdsml(predictions))
})