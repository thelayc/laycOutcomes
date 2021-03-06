% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/utils.R
\name{compute_change}
\alias{compute_change}
\title{compute_change()}
\usage{
compute_change(df, col = c("subject_id", "prepost", "score"))
}
\arguments{
\item{df}{dataframe: a dataframe returned by id_prepost()}

\item{col}{character vector: name of columns to keep to compute change in scores. There should be 3 columns: A column containing participant ids, a column specifying whether this is a pre / post record, a column containing the total score.}
}
\value{
dataframe
}
\description{
This is a helper function that computes change in score between pre and post test. The data must be pre-processed by the followng functions: sum_weights() and id_prepost()
}
\examples{
tp <- laycUtils::load_txt('./my_data_folder/touchpoints.txt')
tp <- laycUtils::format_data(tp)
tp <- tp[tp$tp_name == "jrt pre-/post test", ]
tp <- sum_weights(tp)
tp <- id_prepost(tp)

compute_change(df = tp)
}

