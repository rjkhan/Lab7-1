library(Lab7)
context("Ridge Regression")


test_that("coefficients can get ok",{
  
  expect_less_than(ridgereg(accel ~ dist,attenu, 7)$coeff()[1]- (-0.06762113), 0.0001)
})

test_that("predict returns correct values",{
  expect_less_than(ridgereg(pressure ~ temperature,pressure, 2)$predict(data.frame( temperature = 1:9)) -
                     c(-153.1955, 0.0000,153.1955),0.001)
  expect_less_than(ridgereg(Sepal.Length ~ Species, iris, 3 )$predict()[1] - (-0.8049204) , 0.0001)
  
})
