test_that("run_em_algorithm returns updated model", {
  skip_if_not_installed("DATA501AGM2")
  
  data <- matrix(c(
    5.1, NA, 2.0, 3.3,
    NA, 4.4, 2.2, 1.1,
    6.1, 4.3, 2.5, NA,
    5.0, 4.1, 2.0, 3.0
  ), byrow = TRUE, ncol = 4)
  
  model <- em_model(data)
  result <- run_em_algorithm(model, max_iter = 5)
  
  expect_s3_class(result, "em_model")
  expect_true(!is.null(result$parameters$mu))
  expect_length(result$loglik_history, result$early_stop$iterations)
  expect_true(is.matrix(result$imputed))
})