# Building a Prod-Ready, Robust Shiny Application.
# 
# README: each step of the dev files is optional, and you don't have to 
# fill every dev scripts before getting started. 
# 01_start.R should be filled at start. 
# 02_dev.R should be used to keep track of your development during the project.
# 03_deploy.R should be used once you need to deploy your app.
# 
# 
###################################
#### CURRENT FILE: DEV SCRIPT #####
###################################

# Engineering

## Dependencies ----
## Add one line by package you want to add as dependency
usethis::use_package('ADRCDashHelper', min_version = T) # devtools::install_github('cfmurch/ADRCDashHelper')
usethis::use_package('bs4Dash', min_version = '2.2.1')
usethis::use_package("reactable", min_version = '0.4.3')
usethis::use_package("htmltools", min_version = '0.5.4')
usethis::use_package('waiter', min_version = '0.2.5')
usethis::use_package('shinyWidgets', min_version = '0.7.6')
usethis::use_package('shinybrowser', min_version = '1.0.0')
usethis::use_package("data.table", min_version = '1.14.6')
# usethis::use_package('vctrs', min_version = '0.5.2')
usethis::use_package('purrr', min_version = '1.0.1')
usethis::use_package('forcats', min_version = '1.0.0')
usethis::use_package('stringi', min_version = '1.7.12')
usethis::use_package('stringr', min_version = '1.5.0')
usethis::use_package('rlang', min_version = '1.0.6')
usethis::use_package('dplyr', min_version = '1.1.0')
usethis::use_package('tidyr', min_version = '1.3.0')
usethis::use_package('tibble', min_version = '3.1.8')
usethis::use_package('lubridate', min_version = '1.9.1')
usethis::use_package('glue', min_version = '1.6.2')
usethis::use_package('cli', min_version = '3.6.0')
usethis::use_package('tidyselect', min_version = '1.2.0')
usethis::use_package('RColorBrewer', min_version = '1.1.3')
usethis::use_package('colorspace', min_version = '2.1.0')
usethis::use_package('htmlwidgets', min_version = '1.6.1')
usethis::use_package('ggplot2', min_version = '3.4.0')
usethis::use_package('scales', min_version = '1.2.1')
usethis::use_package('gridExtra', min_version = '2.3')
usethis::use_package('plotly', min_version = '4.10.1')
usethis::use_package('echarts4r', min_version = '0.4.4')
# usethis::use_package('redcapAPI', min_version = T)
usethis::use_pipe()
usethis::use_package('testthat', min_version = '3.1.6', type = 'Suggests')


## Add modules ----
## Create a module infrastructure in R/
# golem::add_module( name = "name_of_module1" ) # Name of the module

## Add helper functions ----
## Creates ftc_* and utils_*
# golem::add_fct( "helpers" ) 
# golem::add_utils( "helpers" )

## External resources
## Creates .js and .css files at inst/app/www
# golem::add_js_file("ADRC")
# golem::add_js_handler( "handlers" )
golem::add_css_file("ADRC")

## Add internal datasets ----
## If you have data in your package
# usethis::use_data_raw( name = "data_temp", open = FALSE ) 

## Tests ----
## Add one line by test you want to create
# usethis::use_test( "app" )

# Documentation

## Vignette ----
# usethis::use_vignette("ADRCDash")
# devtools::build_vignettes()

## Code coverage ----
## (You'll need GitHub there)
# usethis::use_github()
# usethis::use_travis()
# usethis::use_appveyor()

# You're now set! ----
# go to dev/03_deploy.R
rstudioapi::navigateToFile("dev/03_deploy.R")

