#' jrt_table()
#'
#' This function creates a summary table of JRT results.
#' @param enroll_data dataframe: a dataframe containing enrollment data."[Admin] raw_enrollment_report"
#' @param eto_programs character vector:  a vector of character containing the name of ETO programs to keep for analysis.
#' @param tp_data dataframe: a dataframe containing touchpoint data."[Admin] raw_touchpoint_report_detailed"
#' @return dataframe
#' @export
#' @examples
#' enroll <- laycUtils::load_txt('./my_data_folder/enrollment.txt')
#' enroll <- laycUtils::format_data(enroll)
#' tp <- laycUtils::load_txt('./my_data_folder/touchpoints.txt')
#' tp <- laycUtils::format_data(tp)
#'
#' jrt_table(enroll_data = enroll, tp_data = tp)

jrt_table <- function(enroll_data,
                      tp_data,
                      measure = c("enrolled in JRT", "assessed on soft skills", "gained soft-skills"),
                      target = c(enrolled = .75, assessed = .90, gained = .70),
                      eto_programs = c("ss - job readiness", "pg - employment job training", "dc - wise job training"),
                      workforce_programs = c("ss - ccorps projects", "ss - counseling",
                                              "ss - ged", "ss - job placement", "ss - job readiness",
                                              "pg - employment case management", "pg - employment ged",
                                              "pg - employment in school", "pg - employment job placement",
                                              "pg - employment job training", "dc - wise ged", "dc - wise job placement",
                                              "dc - wise job training"),
                      for_bullet = 'no')
{
  # Get workforce enrollment
  workforce <- laycEnrollment::get_enroll(enroll, eto_programs = workforce_programs)

  # Get JRT enrollment
  enrollment <- jrt_enroll(enroll_data)

  # Get JRT assessed on skills
  assessed <- jrt_gains(tp_data)[['total']]

  # Get JRT gained in skills
  gained <- jrt_gains(tp_data)[['positive']]

  # Create data frame
  n <- c(workforce, enrollment, assessed)
  value <- c(enrollment, assessed, gained)
  df <- data.frame(measure, target, value, n)

  # Format data frame
  df$value_scaled <- round(df$value / df$n, 3)
  df$y_title <- paste('%', df$measure, '\nn =', df$n)
  df$target_met[df$value_scaled < df$target / 2] <- 'low'
  df$target_met[df$value_scaled >= df$target / 2 & df$value_scaled < df$target] <- 'medium'
  df$target_met[df$value_scaled >= df$target] <- 'high'
  df$target_met <- ordered(df$target_met)
  df$target_met <- ordered(df$target_met, levels = c("low", "medium", "high"))


  # Return dataframe
  return(df)
}
