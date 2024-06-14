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
 