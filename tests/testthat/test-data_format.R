loaded_data <- load_data()

test_that("data is correct format", {
  expect_type(loaded_data, 'list')
  expect_s3_class(loaded_data$data_curr, 'data.frame')
  expect_s3_class(loaded_data$regist_curr, 'data.frame')
  
  # ideally, colnames and types would be tested but this is difficult
  # b/c data.frame has many columns and are often used indirectly
})
