#' ged_enroll()
#'
#' This function returns the number of participants who enrolled in GED. It takes one dataframe as input. The data comes from the following ETO Results report: "[Admin] raw_enrollment_report".
#' @param enroll_data dataframe: a dataframe containing enrollment data."[Admin] raw_enrollment_report"
#' @param eto_programs character vector:  a vector of character containing the name of ETO programs to keep for analysis.
#' @param ...:  extra arguments. See laycEnrollment::get_enroll() for a detailed description
#' @return numeric
#' @export
#' @examples
#' enroll <- laycUtils::load_txt('./my_data_folder/enrollment.txt')
#' enroll <- laycUtils::format_data(enroll)
#'
#' ged_enroll(enroll_data = enroll)

ged_enroll <- function(enroll_data, eto_programs = c("ss - ged", "pg - employment ged", "dc - wise ged"), ...)
{
  # Retrieve only records of participants who enrolled in GED
  out <- laycEnrollment::get_enroll(enroll_data, eto_programs, ...)
  # Return unduplicated count of participants in GED

  return(out)
}
