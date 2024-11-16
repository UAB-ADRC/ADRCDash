
loaded_data <- load_data()

test_that("utils enroll functions work", {

  expect_equal(
    target_enroll(10, expand.grid(1:12, 2020:2023)),
    c(0.83, 1.67)
  )
  
  enroll_df <- enroll_process_rate(
    loaded_data$regist_curr, 
    start_dt = as.Date('2022-03-01'),
    end_dt = as.Date('2022-06-01'), 
    study = "NACC ADRC Cohort" #study_choices[['name']][[1]]
  )
  enroll_df$Date <- as.numeric(enroll_df$dt_plot)
  target_df <- build_target_enroll_df(enroll_df)
  expect_s3_class(target_df, 'data.frame')
  expect_equal(
    sort(sapply(target_df, class)),
    sort(c(Date = 'numeric',  Target = 'factor', `Target Enrollment` = 'numeric'))
  )
})

test_that("utils misc functions work", {
  
  # df_slicer()
  # nacc_curr_list <- visit_read_in(token = "REDCAP_NACC_API_NEW", subtable_dict = NULL, .type = "nacc", synth=TRUE)
  # nacc_curr <- nacc_curr_list[["visits"]]
  # nacc_last <- df_slicer(nacc_curr, "last")
  
  expect_warning(date_find(tibble::tibble(date = 1)))
  expect_equal(date_find(tibble::tibble(date = 10, date_dt = 1)), 1)
  
  tbl <- cdfer(tibble::tibble(x = 1:10), 'x')
  expect_equal(
    tbl, 
    data.frame(length = 10, mean = mean(1:10), sd = sd(1:10), sem = 0.9574271, median = 5.5, mad = 3.7065, min = 1, max = 10)
  )
  
  expect_s3_class(
    count_table_maker(
      loaded_data$regist_curr,
      "refer_type",
      "Race"
    ),
    'data.frame'
  )
})


