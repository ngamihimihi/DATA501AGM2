test_that("em_model constructor initializes correctly", {
  data <- matrix(c(1, NA, 4, NA, 5, 6), ncol = 2)
  model <- em_model(data)
  
  expect_s3_class(model, "em_model")
  expect_equal(model$data, data)
  expect_equal(model$method, "EM")
  expect_null(model$parameters$mu)
  expect_type(model$loglik_history, "double")
})
######
test_that("em_model errors on non-numeric matrix input", {
  bad_data <- matrix(c(1, NA, 3, "a", 5, 6), ncol = 2)
  expect_error(em_model(bad_data), "numeric matrix")
})
######
#Assign a with a value
a<-"n"
test_that("em_model errors on non-numeric matrix input", {
  bad_data <- matrix(c(1, NA, 3, a, 5, 6), ncol = 2)
  expect_error(em_model(bad_data), "numeric matrix")
})
######
test_that("em_model errors on data frame input", {
  bad_df <- data.frame(x = c(1, 2), y = c(3, "x"))
  expect_error(em_model(bad_df), "numeric matrix")
})
######
test_that("em_model errors on NULL input", {
  expect_error(em_model(NULL), "numeric matrix")
})

######
test_that("em_model errors when all values are NA", {
  all_na <- matrix(NA_real_, nrow = 3, ncol = 3)
  expect_error(em_model(all_na), "cannot contain only missing values")
})