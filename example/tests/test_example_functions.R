library(testthat)
library(dplyr)
library(tidyr)

source("../src/example_functions.R")

test_that("add_two_numbers works as expected, for small numbers", {
  input_1 = 1
  input_2 = 5
  
  expected = 6
  
  actual = add_two_numbers(input_1, input_2)
  
  expect_equal(expected, actual)
})

test_that("add_two_numbers works as expected, for large numbers", {
  input_1 = 10000000
  input_2 = 50000000
  
  expected = 60000000
  
  actual = add_two_numbers(input_1, input_2)
  
  expect_equal(expected, actual)
})




test_that("filter_section_timeband is working", {
  inputDataframe <- data.frame(
    Section = c("Section_5ai", "Section_3c", "Section_1a", "Section_5biv", "Section_5bii", "Section_5aiii", "Section_5bi"),
    time_band_max = c(66, NA, 99, 102, NA, 104, 65)
  )
  
  expectedDataframe <- data.frame(
    Section = c("Section_5ai", "Section_5biv", "Section_5aiii"),
    time_band_max = c(66, 102, 104)
  )
  
  actualDataframe <- filter_section_timeband(inputDataframe, "Section_5", ">", 65)
  
  actualDataframe <- data.frame(actualDataframe, row.names = NULL)
  
  expect_equal(expectedDataframe, actualDataframe)
  
})
