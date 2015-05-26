#' ged_table()
#'
#' This function creates a summary table of GED results.
#' @param enroll_data dataframe: a dataframe containing enrollment data."[Admin] raw_enrollment_report"
#' @param pos_data dataframe: a dataframe containing point of service data."[Admin] raw_pos_report_detailed"
#' @param measure character vector: Name of measures in the table
#' @param target numeric vector: Organization target for each measure in the table
#' @param eto_programs character vector:  a vector of character containing the name of ETO programs to keep for GED analysis.
#' @param workforce_programs character vector:  a vector of character containing the name of all workforce programs.

#' @return dataframe
#' @export
#' @examples
#' enroll <- laycUtils::load_txt('./my_data_folder/enrollment.txt')
#' enroll <- laycUtils::format_data(enroll)
#' tp <- laycUtils::load_txt('./my_data_folder/touchpoints.txt')
#' tp <- laycUtils::format_data(tp)
#'
#' ged_table(enroll_data = enroll, pos_data = tp)

ged_table <- function(enroll_data,
                      pos_data,
                      measure = c("enrolled in GED", "assessed on academic skills", "gained academic skills", "took GED", "passed GED"),
                      target = c(enrolled = .75, assessed = .90, gained = .70, took = 0.3, passed = 0.8 ),
                      eto_programs = c("ss - ged", "pg - employment ged", "dc - wise ged"),
                      workforce_programs = c("ss - ccorps projects", "ss - counseling",
                                              "ss - ged", "ss - job placement", "ss - job readiness",
                                              "pg - employment case management", "pg - employment ged",
                                              "pg - employment in school", "pg - employment job placement",
                                              "pg - employment job training", "dc - wise ged", "dc - wise job placement",
                                              "dc - wise job training"))
{
  # Get workforce enrollment
  workforce <- laycEnrollment::get_enroll(enroll, eto_programs = workforce_programs)

  # Get JRT enrollment
  enrollment <- ged_enroll(enroll_data)

  # Get GED assessed
  assessed <- ged_gains(pos_data)[['total']]

  # Get GED academic skills gained
  gained <- ged_gains(pos_data)[['positive']]

  # Get # participants who took the GED exam
  took <- ged_take(pos_data)

  # Get # participants who got GED
  passed <- ged_pass(enroll, eto_programs = eto_programs)


  # Create data frame
  n <- c(workforce, enrollment, assessed, enrollment, took)
  value <- c(enrollment, assessed, gained, took, passed)
  df <- data.frame(measure, target, value, n)

  # Format data frame
  df$value_scaled <- round(df$value / df$n, 3)
  df$value_scaled[df$value > df$n] <- NA
  df$y_title <- paste('%', df$measure, '\nn =', df$n)
  df$target_met[df$value_scaled < df$target / 2] <- 'low'
  df$target_met[df$value_scaled >= df$target / 2 & df$value_scaled < df$target] <- 'medium'
  df$target_met[df$value_scaled >= df$target] <- 'high'
  df$target_met <- ordered(df$target_met)
  df$target_met <- ordered(df$target_met, levels = c("low", "medium", "high"))



  # Return dataframe
  return(df)
}
