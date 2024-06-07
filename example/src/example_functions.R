# File containing the example functions to test

add_two_numbers <- function(a, b) {
  return(a + b)
}


filter_section_timeband <- function(data, section_string, comparison_operator, time_band_number) {
  # This function filters a data frame on a given section and time band
  # @param section_string (string)
  # @param comparison_operator e.g. >, >=, <, <=, == (string)
  # @param time_band_number (integer)
  
  # Returns error message if the 'comparison_operator' parameter isn't valid
  if (!(comparison_operator %in% c("<", "<=", ">", ">=", "=="))) {
    stop("The comparison_operator selected is not valid. Please enter one of the following:
         \"<\", \"<=\", \">\", \">=\", \"==\" \"!=\"")
  }
  
  # Filters the data on the 'Section' column
  filtered_data <- data[grepl(section_string, data$Section), ]
  
  # Drops NA from 'time_band_min' column
  filtered_data <- filtered_data[!is.na(filtered_data$time_band_max), ]
  
  # Filters the data based on the operator and time band
  filtered_data <- subset(filtered_data, eval(parse(text = paste("time_band_max", comparison_operator, time_band_number))))
  
  return(filtered_data)
  
}





create_rule <-
  function(section_data,
           conditional_formatting_type,
           conditional_formatting_detail) {
    #' create rule
    #'
    #' create rule for conditional formatting
    #'
    #' @param section_data data which will be conditionally formatted (tibble)
    #' @param conditional_formatting_type type of conditional formation
    #' either colorScale, arrows or traffic-
    #' others could be added but would need to add additional if statements
    #' @param conditional_formatting_detail extra information about conditional formatting
    #' for colourScale:
    #' - this is a vector of 2 or 3 colours as strings-
    #' -these will be the colours used in conditional formatting
    #' for arrows:
    #' - this will not be used- can be NA
    #' for traffic:
    #' - this should be "number" or "percent" based on whether the numbers to be
    #' conditionally formatted are numbers or percentages
    #'
    #' @return
    
    if (conditional_formatting_type == "colorScale") {
      if (length(conditional_formatting_detail) == 3) {
        #if colorScale with 3 colours- rule is min, median and max of data
        
        section_vector <-
          section_data %>% select(-1) %>% t() %>%  as.vector()## get into a vector
        min_point <- min(section_vector, na.rm = TRUE)
        max_point <- max(section_vector, na.rm = TRUE)
        median_point <- median(section_vector, na.rm = TRUE)
        rule <- c(min_point, median_point, max_point)
        
      } else if (length(conditional_formatting_detail) == 2) {
        #if colorScale with 2 colours- rule is min and max of data
        
        section_vector <-
          section_data %>% select(-1) %>% t() %>%  as.vector()## get into a vector
        min_point <- min(section_vector, na.rm = TRUE)
        max_point <- max(section_vector, na.rm = TRUE)
        rule <- c(min_point, max_point)
      }
    } else if (conditional_formatting_type == "arrows") {
      #if arrows- rule is set to below
      
      rule <- c(-10000, 10,-0.5, 0.5, 10)
    } else if (conditional_formatting_type == "traffic") {
      if (conditional_formatting_detail == "number") {
        #if traffic for numbers rule is set to below
        
        rule <- c(-10000, 0, 20)
        
      } else if (conditional_formatting_detail == "percent") {
        #if traffic for percentages rule is set to below
        
        rule <- c(0, 0.6, 0.8)
      }
    }
    return(rule)
  }

