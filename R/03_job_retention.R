#' job_retention()
#'
#' This function returns the number of participants who enrolled in job placement. It takes one dataframe as input. The data comes from the following ETO Results report: "[Admin] raw_enrollment_report".
#' @param enroll_data dataframe: a dataframe containing enrollment data."[Admin] raw_enrollment_report"
#' @param job_data dataframe: a dataframe containing employment data."[Admin] raw_job_report"
#' @param eto_programs character vector:  a vector of character containing the name of ETO programs to keep for analysis.
#' @param retention numeric: Minimum number of days to meet retention objective
#' @return numeric
#' @export
#' @examples
#' enroll <- laycUtils::load_txt('./my_data_folder/enrollment.txt')
#' enroll <- laycUtils::format_data(enroll)
#' job <- laycUtils::load_txt('./my_data_folder/job.txt')
#' job <- laycUtils::format_data(job)
#' 
#' job_retention(enroll_data = enroll, job_data = job)

job_retention <- function(enroll_data,
                       job_data,
                       eto_programs = c("ss - job placement", "pg - employment job placement", 
                                        "dc - wise job placement", "dc - wise job training", 
                                        "pg - employment job training", "pg - employment in school",
                                        "pg - employment case management", "pg - employment job placement",
                                        "ss - job readiness", "ss - ccorps projects"),
                       retention = 90) {
  
  # 1 - Identify youth with valid job records--------------------------------
  
  job_data <- job_data[job_data$program_start <= job_data$job_start, ]
  job_data <- job_data[job_data$days_employed >= retention, ]
  job_to_match <- unique(job_data$subject_id)
  
  # 2 - Identify all workforce particpants-----------------------------------
  
  enroll_data <- enroll_data[enroll_data$program_name %in% eto_programs, ]
  workforce_ids <- unique(enroll_data$subject_id)
  
  # 3 - Get number of workforce participants with valid job records----------

  out <- length(intersect(workforce_ids, job_to_match))
  
  # Return unduplicated count of participants who obtained a job
  return(out)
}