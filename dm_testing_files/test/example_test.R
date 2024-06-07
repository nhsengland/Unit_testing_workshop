install.packages("testthat")

#not sure if this is really testing the function - do I need another test vector?   
test_that("mean_calc_works", {
  # Arrange
  input_1 = vector
  
  expected = sum(vector)/length(vector)
  
  # Act
  actual = calculate_mean(vector)
  
  # Assert
  expect_equal(expected, actual)
})  
