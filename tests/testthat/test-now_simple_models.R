test_that("linear models work1", {
  x <- c(1:105)
  y <- append(EuStockMarkets[,2][1:100], rep(NA,5))
  df <- data.frame(x,y)
  expect_equal(now_simple_models(y ~ x, model = "lm", df), 
               c(1694.610, 1694.428, 1694.246, 1694.064, 1693.883 ), 
               tolerance = 0.001)
})