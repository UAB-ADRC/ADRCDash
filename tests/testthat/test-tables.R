
loaded_data <- load_data()

test_that("bdcsf tables work", {
  comp <- compon_process(
    loaded_data$regist_curr, 
    start_dt = as.Date('2022-03-01'), 
    end_dt = as.Date('2022-06-01'), 
    study = "NACC ADRC Cohort", #study_choices[['name']][[1]],
    compon_var = 'Neuroimaging', #compon_choices[1], 
    group_var = 'Race' #compon_group_choices[1]
  )
  tab <- make_table_compon(comp)
  
  expect_s3_class(tab, 'data.frame')
  expect_equal(dim(tab), c(3, 6))
  expect_setequal(
    colnames(tab),
    c(' ', ' ', 'Total', 'Enrolled PET', 'Unlisted PET', 'Screen Fail PET')
  )
  expect_setequal(
    rownames(tab),
    c("White", "Black...AA", 'Other')
  )
})

test_that("explorer tables work", {
  expl <- explorer_process(
    loaded_data$data_curr,
    study = "NACC ADRC Cohort", #study_choices[['name']][[1]],
    vis_type = 'Baseline', #visit_choices[[1]], 
    study_restrict = FALSE #input$expl_study_restrict
  )
  tab <- make_table_explorer(
    expl,
    indvar_curr = 'AD Numeric Stage', #xvar_choices[[1]],
    depvar_curr = 'Count', #yvar_choices[[1]], 
    group_curr = 'AD Syndromal Stage', #group_choices[[1]], 
    vis_type = 'Baseline' #visit_choices[[1]]
  )
  
  expect_s3_class(tab, 'data.frame')
  expect_equal(dim(tab), c(6, 5))
  expect_setequal(
    colnames(tab),
    c('AD Numeric Stage', 'Cognitively Unimpaired', 'Mild Cognitive Impairment', 'Dementia', 'All Subjects')
  )
})

test_that("invent tables work", {
  
  tab <- make_table_inventory(
    loaded_data$data_curr, 
    study = "NACC ADRC Cohort", #study_choices[['name']][[1]], 
    group_var = 'Race' #inventory_group_select
  )
  
  expect_type(tab, 'list')
  expect_length(tab, 2)
  expect_setequal(
    names(tab),
    c('sum', 'mean')
  )
  expect_s3_class(tab$sum, 'data.frame')
  expect_s3_class(tab$mean, 'data.frame')
})

test_that("refer tables work", {
  tab <- make_table_refer_by_yr(
    loaded_data$regist_curr,
    study = "NACC ADRC Cohort", #study_choices[['name']][[1]], 
    group_curr = 'AD Syndromal Stage', #group_choices[[1]], 
    source_curr = 'Clinician Referral', #refer_choices[[1]],
    restrict_on_study = FALSE,
    start_dt = as.Date('2022-03-01'), 
    end_dt = as.Date('2022-06-01')
  )
  
  expect_shinytag(tab)
})
