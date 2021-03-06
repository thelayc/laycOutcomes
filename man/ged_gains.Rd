% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/02_ged_gains.R
\name{ged_gains}
\alias{ged_gains}
\title{ged_gains()}
\usage{
ged_gains(pos_data, eto_programs = c("ss - ged", "pg - employment ged",
  "dc - wise ged"))
}
\arguments{
\item{pos_data}{dataframe: a dataframe containing pos data."[Admin] raw_pos_report"}

\item{eto_programs}{character vector:  a vector of character containing the name of ETO programs to keep for analysis.}
}
\value{
numeric
}
\description{
This function returns the number of participants who increased their academic skills as measured by practice GED scores. It takes one dataframe as input. The data comes from the following ETO Results report: "[Admin] raw_pos_report".
}
\examples{
pos <- laycUtils::load_txt('./my_data_folder/pos.txt')
pos <- laycUtils::format_data(pos)

ged_gains(pos_data = pos)
}

