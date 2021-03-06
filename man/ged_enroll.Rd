% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/01_ged_enroll.R
\name{ged_enroll}
\alias{ged_enroll}
\title{ged_enroll()}
\usage{
ged_enroll(enroll_data, eto_programs = c("ss - ged", "pg - employment ged",
  "dc - wise ged"))
}
\arguments{
\item{enroll_data}{dataframe: a dataframe containing enrollment data."[Admin] raw_enrollment_report"}

\item{eto_programs}{character vector:  a vector of character containing the name of ETO programs to keep for analysis.}
}
\value{
numeric
}
\description{
This function returns the number of participants who enrolled in GED. It takes one dataframe as input. The data comes from the following ETO Results report: "[Admin] raw_enrollment_report".
}
\examples{
enroll <- laycUtils::load_txt('./my_data_folder/enrollment.txt')
enroll <- laycUtils::format_data(enroll)

ged_enroll(enroll_data = enroll)
}

