#' jrt_gains()
#'
#' This function computes soft skills gains as measured by the JRT prepost test. It returns the number of participants who showed positive, negative or no change. The data comes from the following ETO Results report: "[Admin] raw_touchpoint_report_detailed".
#' @param tp_data dataframe: a dataframe containing touchpoint data."[Admin] raw_touchpoint_report_detailed"
#' @return list
#' @export
#' @import dplyr
#' @examples
#' tp <- laycUtils::load_txt('./my_data_folder/touchpoints.txt')
#' tp <- laycUtils::format_data(tp)
#' tp <- tp[tp$tp_name == "jrt pre-/post test", ]
#'
#' jrt_gains(tp_data = tp)

jrt_gains <- function(tp_data,
                        var = c('subject_id', 'date', 'answer_id', 'question_id',
                                'question_short', 'answer_weight'),
                        weight_var = 'answer_weight',
                        weight_id = '^q',
                        group_var = c('subject_id', 'answer_id'),
                      eto_tp = c("jrt pre-/post test")) {

  # Subset data
  tp_data <- tp_data[tp_data$tp_name == eto_tp, ]
  # Compute total scores
  tp_data <- sum_weights(tp_data)

  # Identify pre and post test
  tp_data <- id_prepost(tp_data)

  # Compute change
  tp_data <- compute_change(tp_data)

  tp_data <- tp_data[!is.na(tp_data$change_ord), ]
  # Participants showing positive change
  pos <- length(unique(tp_data$subject_id[tp_data$change_ord == 'positive']))
  # Participants showing no change
  none <- length(unique(tp_data$subject_id[tp_data$change_ord == 'no change']))
  # Participants showing negative change
  neg <- length(unique(tp_data$subject_id[tp_data$change_ord == 'negative']))
  # Participants with matching pre post test
  total <- pos + none + neg

  # Return dataframe
  return(list(positive = pos, no_change = none, negative = neg, total = total, percent_increase = pos/total ))
}
