source("tests.R")
get_var_info <- function(names) {
  check_data_var_names(names)
  info <- variable_table[match(names, variable_table$variable_name),]
  return(as.character(info$long_name))
  # return(as.character(info$standard_name))
}