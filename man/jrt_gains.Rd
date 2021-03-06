% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/02_jrt_gains.R
\name{jrt_gains}
\alias{jrt_gains}
\title{jrt_gains()}
\usage{
jrt_gains(tp_data, var = c("subject_id", "date", "answer_id", "question_id",
  "question_short", "answer_weight"), weight_var = "answer_weight",
  weight_id = "^q", group_var = c("subject_id", "answer_id"),
  eto_tp = c("jrt pre-/post test"))
}
\arguments{
\item{tp_data}{dataframe: a dataframe containing touchpoint data."[Admin] raw_touchpoint_report_detailed"}
}
\value{
list
}
\description{
This function computes soft skills gains as measured by the JRT prepost test. It returns the number of participants who showed positive, negative or no change. The data comes from the following ETO Results report: "[Admin] raw_touchpoint_report_detailed".
}
\examples{
tp <- laycUtils::load_txt('./my_data_folder/touchpoints.txt')
tp <- laycUtils::format_data(tp)

jrt_gains(tp_data = tp)
}

