
context("Ridge Regression")


test_that("",{
  
})

test_that("coefficients can get ok",{
  expect_equal(ridgereg(Sepal.Length ~ Species, iris, 3 )$coef(), c(5.7287582, 0.4189210, 0.7232219) )))
  expect_equal(ridgereg(accel ~ dist,attenu, 7)$coef(),c(0.14850794, -0.06762113))
})



test_that("pred returns correct values",{
  expect_equal(ridgereg(pressure ~ temperature,pressure, 2)$predict(), matrix(-132.516906,
                                                                                -105.293348,
                                                                                 -78.069790,
                                                                                 -50.846232,
                                                                                 -23.622675,
                                                                                   3.600883,
                                                                                  30.824441,
                                                                                  58.047999,
                                                                                  85.271556,
                                                                                 112.495114,
                                                                                 139.718672,
                                                                                 166.942230,
                                                                                 194.165788,
                                                                                 221.389345,
                                                                                 248.612903,
                                                                                 275.836461,
                                                                                 303.060019,
                                                                                 330.283577,
                                                                                 357.507134, ncol = 1))

  
})
