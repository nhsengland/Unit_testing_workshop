library(testthat)
library(tidyverse)
source("../src/example_functions.R")

test_that("add_two_numbers works as expected, for small numbers", {
  # Arrange
  input_1 = 1
  input_2 = 5
  
  expected = 6
  
  # Act
  actual = add_two_numbers(input_1, input_2)
  
  # Assert
  expect_equal(expected, actual)
})

test_that("add_two_numbers works as expected, for large numbers", {
  # Arrange
  input_1 = 10000000
  input_2 = 50000000
  
  expected = 60000000
  
  # Act
  actual = add_two_numbers(input_1, input_2)
  
  # Assert
  expect_equal(expected, actual)
})




test_that("filter_section_timeband is working", {
  
  # Arrange
  inputDataframe <- data.frame(
    Section = c("Section_5ai", "Section_3c", "Section_1a", "Section_5biv", "Section_5bii", "Section_5aiii", "Section_5bi"),
    time_band_max = c(66, NA, 99, 102, NA, 104, 65)
  )
  
  expectedDataframe <- data.frame(
    Section = c("Section_5ai", "Section_5biv", "Section_5aiii"),
    time_band_max = c(66, 102, 104)
  )
  
  # Act
  actualDataframe <- filter_section_timeband(inputDataframe, "Section_5", ">", 65)
  
  actualDataframe <- data.frame(actualDataframe, row.names = NULL)
  
  # Assert
  expect_equal(expectedDataframe, actualDataframe)
  
})



test_that("create_rule function is working for color scale with 2 colours", {

  
  #Arrange
  section_data <- tibble(col1= c("a","b","c","d","e","f"), 
                         col2= c(100,200,300,400,500,600),
                         col3= c(1,2,3,4,5,6))
  conditional_formatting_detail<- c("white","red")
  conditional_formatting_type<- "colorScale"
  
  expected_rule<-c(1,600)
  
  #Act
  rule<- create_rule(section_data, 
              conditional_formatting_type,
              conditional_formatting_detail)
  #Assert
  
  expect_equal(rule, expected_rule)
  
  
  })




test_that("create_rule function is working for color scale with 3 colours", {
  
  
  #Arrange
  section_data <- tibble(col1= c("a","b","c","d","e","f"), 
                         col2= c(10,20,30,40,50,60),
                         col3= c(1,2,3,4,5,6))
  conditional_formatting_detail<- c("white","pink","red")
  conditional_formatting_type<- "colorScale"
  
  expected_rule<-c(1,8,60)
  
  #Act
  rule<- create_rule(section_data, 
                     conditional_formatting_type,
                     conditional_formatting_detail)
  #Assert
  
  expect_equal(rule, expected_rule)
  
  
})


test_that("create_rule function is working for color scale with 3 colours with some missing values", {
  
  
  #Arrange
  section_data <- tibble(col1= c("a","b","c"), 
                         col2= c(10,20,30),
                         col3= c(1,NA,3))
  conditional_formatting_detail<- c("white","pink","red")
  conditional_formatting_type<- "colorScale"
  
  expected_rule<-c(1,10,30)
  
  #Act
  rule<- create_rule(section_data, 
                     conditional_formatting_type,
                     conditional_formatting_detail)
  #Assert
  
  expect_equal(rule, expected_rule)
  
  
})



test_that("create_rule function is working for arrows", {
  
  
  #Arrange
  section_data <-NA
  conditional_formatting_detail<- NA
  conditional_formatting_type<- "arrows"
  
  expected_rule <- c(-10000, 10,-0.5, 0.5, 10)
  
  #Act
  rule<- create_rule(section_data, 
                     conditional_formatting_type,
                     conditional_formatting_detail)
  #Assert
  
  expect_equal(rule, expected_rule)
  
  
})


test_that("create_rule function is working for traffic- numbers", {
  
  
  #Arrange
  section_data <-NA
  conditional_formatting_detail<- "number"
  conditional_formatting_type<- "traffic"
  
  expected_rule <- c(-10000, 0, 20)
  
  #Act
  rule<- create_rule(section_data, 
                     conditional_formatting_type,
                     conditional_formatting_detail)
  #Assert
  
  expect_equal(rule, expected_rule)
  
  
})


test_that("create_rule function is working for traffic- percent", {
  
  
  #Arrange
  section_data <-NA
  conditional_formatting_detail<- "percent"
  conditional_formatting_type<- "traffic"
  
  rule <- c(0, 0.6, 0.8)
  
  #Act
  rule<- create_rule(section_data, 
                     conditional_formatting_type,
                     conditional_formatting_detail)
  #Assert
  
  expect_equal(rule, expected_rule)
  
  
})