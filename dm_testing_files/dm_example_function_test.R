install.packages("testthat")

vector <- c(3, 5, 2, 3, 1, 4)

calculate_mean <- function(vector){
  mean <- mean(vector)
}
print(calculate_mean (vector))

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

  