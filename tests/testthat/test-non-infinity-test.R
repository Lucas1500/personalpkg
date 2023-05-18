test_that("return finite output", {
  expect_lt(log_summed_exps(2000), Inf)
})
