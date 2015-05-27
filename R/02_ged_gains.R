#' ged_gains()
#'
#' This function returns the number of participants who increased their academic skills as measured by practice GED scores. It takes one dataframe as input. The data comes from the following ETO Results report: "[Admin] raw_pos_report".
#' @param pos_data dataframe: a dataframe containing pos data."[Admin] raw_pos_report"
#' @param eto_programs character vector:  a vector of character containing the name of ETO programs to keep for analysis.
#' @return numeric
#' @export
#' @examples
#' pos <- laycUtils::load_txt('./my_data_folder/pos.txt')
#' pos <- laycUtils::format_data(pos)
#'
#' ged_gains(pos_data = pos)

ged_gains <- function(pos_data,
                      eto_programs = c("ss - ged", "pg - employment ged", "dc - wise ged")){

  # CHECK: input validity
  assertthat::assert_that(is.data.frame(pos_data))
  assertthat::assert_that(is.character(eto_programs))

  # 1 - keep only relevant records for analysis -----------------------------
  pos_data <- pos_data[pos_data$program_name %in% eto_programs, ]
  pos_data <- pos_data[pos_data$pos_name == "practice ged *total* score", ]
  pos_data$pos_value <- as.numeric(gsub("[^0-9.-]+", "", as.character(pos_data$pos_value)))
  colnames(pos_data)[colnames(pos_data) == 'pos_value'] <- 'score'

  # 2 - Identify pre / post scores-------------------------------------------
  pos_data <- id_prepost(pos_data)

  # 3 - Compute change in scores---------------------------------------------
  pos_data <- compute_change(pos_data)
  out <- pos_data

  # 4 - Return basic statistics---------------------------------------------
  pos_data <- pos_data[!is.na(pos_data$change_ord), ]
  # Participants showing positive change
  pos <- length(unique(pos_data$subject_id[pos_data$change_ord == 'positive']))
  # Participants showing no change
  none <- length(unique(pos_data$subject_id[pos_data$change_ord == 'no change']))
  # Participants showing negative change
  neg <- length(unique(pos_data$subject_id[pos_data$change_ord == 'negative']))
  # Participants with matching pre post test
  total <- pos + none + neg

  # Return dataframe
  return(list(positive = pos, no_change = none, negative = neg, total = total, full_data = out))
}



