#' sum_weights()
#'
#' This is a helper function that sums question weights from surveys to compute a total score. The data comes from the following ETO Results report: "[Admin] raw_touchpoint_report_detailed".
#' @param tp_data dataframe: a dataframe containing touchpoint data."[Admin] raw_touchpoint_report_detailed"
#' @param var character vector: column names to keep for calculations
#' @param weight_id character: pattern that identifies the question to be included in the sum of weights. Can be  a regular expression.
#' @return dataframe
#' @export
#' @import dplyr
#' @examples
#' tp <- laycUtils::load_txt('./my_data_folder/touchpoints.txt')
#' tp <- laycUtils::format_data(tp)
#' tp <- tp[tp$tp_name == "jrt pre-/post test", ]
#'
#' sum_weights(tp_data = tp)

sum_weights <- function(tp_data,
                        var = c('subject_id', 'date', 'answer_id', 'question_id',
                                'question_short', 'answer_weight'),
                        weight_var = 'answer_weight',
                        weight_id = '^q',
                        group_var = c('subject_id', 'answer_id')) {

  # Keep only relevant columns
  tp_data <- tp_data[ , colnames(tp_data) %in% var]

  # Identify weigthed question to be summed & create a subset of tp_data
  keep <- stringr::str_detect(tp_data$question_short, '^q')
  to_sum <- tp_data[keep, ]
  to_sum[ , weight_var] <- as.numeric(to_sum[ , weight_var])

  # Compute score for each unique combination of subject_id & answer_id
  # Code from http://stackoverflow.com/questions/21208801/group-by-multiple-columns-in-dplyr-using-string-vector-input
  dots <- lapply(group_var, as.symbol)
  to_sum %>%
    group_by_(.dots=dots) %>%
    mutate_(score = ~sum(answer_weight, na.rm = TRUE)) %>%
    ungroup() %>%
    select_(.dots = list(quote(subject_id), quote(answer_id), quote(score))) %>%
    distinct ->
    to_sum

  # Merge scores with original dataset
  tp_data %>%
    left_join(to_sum, by = group_var) ->
    tp_data

  # Return dataframe
  return(tp_data)
}



#' compute_change()
#'
#' This is a helper function that computes change in score between pre and post test. The data must be pre-processed by the followng functions: sum_weights() and id_prepost()
#' @param df dataframe: a dataframe returned by id_prepost()
#' @param col character vector: name of columns to keep to compute change in scores. There should be 3 columns: A column containing participant ids, a column specifying whether this is a pre / post record, a column containing the total score.
#' @return dataframe
#' @export
#' @import dplyr
#' @examples
#' tp <- laycUtils::load_txt('./my_data_folder/touchpoints.txt')
#' tp <- laycUtils::format_data(tp)
#' tp <- tp[tp$tp_name == "jrt pre-/post test", ]
#' tp <- sum_weights(tp)
#' tp <- id_prepost(tp)
#'
#' compute_change(df = tp)

compute_change <- function(df, col = c('subject_id', 'prepost', 'score')) {

  ## Compute change: post-score minus pre-score
  # Select only relevant columns
  out <- df[ , col]
  # Remove duplicates
  out <- unique(out)
  # Remove NAs
  out <- out[!is.na(out$prepost), ]
  # Sort rows
  out <- dplyr::arrange_(out, ~subject_id, ~desc(prepost)) # Add unit test to check that the ordering is correct
  # Group by subject_id in order to compute change for each participant
  out <- dplyr::group_by_(out, ~subject_id)
  # Remove participants without matching pre / post
  out <-  dplyr::mutate_(out, n = ~length(subject_id))
  out <- dplyr::filter_(out, ~n > 1)
  # Compute change
  out <-  dplyr::mutate_(out, change = ~diff(score))
  # Keep only relevant information
  out <-  dplyr::ungroup(out)
  out <-  dplyr::select_(out, ~subject_id, ~change)
  out <-  dplyr::distinct_(out)

  # Add classification variable: positive, no change, negative
  out$change_ord[out$change > 0] <- 'positive'
  out$change_ord[out$change == 0] <- 'no change'
  out$change_ord[out$change < 0] <- 'negative'

  # Merge out with riginal dataset
  df <- left_join(df, out, by = 'subject_id')

  # Return dataframe
  return(df)
}




#' id_prepost()
#'
#' This is a helper function that identifies 'pre' and 'post' test based on the date the test was taken.
#' @param df dataframe: a dataframe containing longitudinal data.
#' @return dataframe
#' @export
#' @import dplyr
#' @examples
#' tp <- laycUtils::load_txt('./my_data_folder/touchpoints.txt')
#' tp <- laycUtils::format_data(tp)
#' tp <- tp[tp$tp_name == "jrt pre-/post test", ]
#'
#' id_prepost(df = tp)

id_prepost <- function(df){

  # Identify when the test was taken for the first & last time
  df %>%
    dplyr::group_by_(~subject_id) %>%
    dplyr::mutate_(first = ~min(date),
                   last = ~max(date)) %>%
    dplyr::ungroup() ->
    df

  # Assign pre / post values to first / last date taken

  df$prepost[df$date == df$first] <- 'pre'
  df$prepost[df$date == df$last] <- 'post'

  # Return dataframe
  return(as.data.frame(df))
}







