library(Lab7)
context("Ridge Regression")


test_that("coefficients can get ok",{
  
  expect_less_than(ridgereg(accel ~ dist,attenu, 7)$coef()[1]- (-0.06762113), 0.0001)
})

test_that("predict returns correct values",{
  expect_less_than(ridgereg(pressure ~ temperature,pressure, 2)$predict(data.frame(9)) - 1378.76, 0.01)
  expect_less_than(ridgereg(Sepal.Length ~ Species, iris, 3 )$predict()[1] - (-0.8049204) , 0.0001)
  
})
