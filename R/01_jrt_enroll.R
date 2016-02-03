#' jrt_enroll()
#'
#' This function returns the number of participants who enrolled in jrt. It takes one dataframe as input. The data comes from the following ETO Results report: "[Admin] raw_enrollment_report".
#' @param enroll_data dataframe: a dataframe containing enrollment data."[Admin] raw_enrollment_report"
#' @param eto_programs character vector:  a vector of character containing the name of ETO programs to keep for analysis.
#' @return numeric
#' @export
#' @examples
#' enroll <- laycUtils::load_txt('./my_data_folder/enrollment.txt')
#' enroll <- laycUtils::format_data(enroll)
#'
#' jrt_enroll(enroll_data = enroll)

jrt_enroll <- function(enroll_data, eto_programs = NULL)
{
  # Retrieve only records of participants who enrolled in jrt
  out <- laycEnrollment::get_enroll(enroll_data, eto_programs)
  # Return unduplicated count of participants in jrt
  return(out)
}
