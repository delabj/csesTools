% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calculate_functions.R
\name{calc_voted}
\alias{calc_voted}
\title{Calculate Voter Status}
\usage{
calc_voted(
  data,
  new_name = "voted",
  early_module_ballot_1,
  early_module_ballot_2,
  current_pres_1st,
  current_pres_2nd,
  current_lh
)
}
\arguments{
\item{data}{a data frame}

\item{new_name}{name of the new column}
}
\description{
If a voter voted in any of the elections (lower house, upper house, head of state)
the respondent is classified as `Voted`, otherwise `Did Not Vote`
}
