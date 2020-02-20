e_var_nf <- errorCondition("Couldn't find variable name in variable table:")

# UTC Conversion
data$date <- format(ymd_hms(data$date, tz = "Europe/Berlin"), tz = "UTC")

check_data_var_names <- function(names){
  variable_table <- read.table("variable.csv", header = T, sep = ",")  # read variable names table from local folder
  # check for matching variables in data and names table
  # for (name in names) {
    info <- match(names, variable_table$variable_name)
    if (sum(is.na(info))) {
      stop(e_var_nf, names[is.na(info)])
    }
  # }
}