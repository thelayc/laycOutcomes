#' ged_take()
#'
#' This function returns the number of participants who took the GED exam. It takes one dataframe as input. The data comes from the following ETO Results report: "[Admin] raw_pos_report".
#' @param pos_data dataframe: a dataframe containing pos data."[Admin] raw_pos_report"
#' @param eto_programs character vector:  a vector of character containing the name of ETO programs to keep for analysis.
#' @return numeric
#' @export
#' @examples
#' pos <- laycUtils::load_txt('./my_data_folder/pos.txt')
#' pos <- laycUtils::format_data(pos)
#' 
#' ged_take(pos_data = pos)

ged_take <- function(pos_data, eto_programs = c("ss - ged", "pg - employment ged", "dc - wise ged"))
{
  # Retrieve only records of participants who enrolled in GED
  out <- pos_data[pos_data$program_name %in% eto_programs, ]
  # Retrieve pos records identifying participants who took the GED
  out$pos_name <- gsub('\\*', '', out$pos_name)
  out$pos_name <- gsub(' ', '_', out$pos_name)
  to_keep <- c("ged_total_score")
  
  out <- out[out$pos_name %in% to_keep, ]
  out$pos_value <- as.numeric(out$pos_value)
  out <- out[out$pos_value > 0, ]
  # Return unduplicated count of participants
  out <- length(unique(out$subject_id))
  
  return(out)
}
