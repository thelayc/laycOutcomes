#' ged_pass()
#'
#' This function returns the number of participants who obtained their GED. It takes one dataframe as input. The data comes from the following ETO Results report: "[Admin] raw_enrollment_report".
#' @param enroll_data dataframe: a dataframe containing enrollment data."[Admin] raw_enrollment_report"
#' @param eto_programs character vector:  a vector of character containing the name of ETO programs to keep for analysis.
#' @return numeric
#' @export
#' @examples
#' enroll <- laycUtils::load_txt('./my_data_folder/enrollment.txt')
#' enroll <- laycUtils::format_data(enroll)
#' 
#' ged_pass(enroll_data = enroll)

ged_pass <- function(enroll_data, eto_programs = c("ss - ged", "pg - employment ged", "dc - wise ged"))
  {
  # Retrieve only records of participants who successfully completed GED
  df <- enroll_data[enroll_data$program_name %in% eto_programs, ]
  df <- df[df$successfully_completed == 'yes', ]
  # Return unduplicated count of participants with GED
  out <- length(unique(df$subject_id))
  return(out)
  }
