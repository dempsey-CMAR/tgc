% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tgc_functions.R
\name{TGC_calculate_degree_days}
\alias{TGC_calculate_degree_days}
\title{Use TGC model to calculate degree days required to grow from initial to final
weight}
\usage{
TGC_calculate_degree_days(initial_weight, final_weight, tgc)
}
\arguments{
\item{initial_weight}{A vector of initial weight(s) of the fish in kilograms.}

\item{final_weight}{A vector of final weight(s) of the fish in kilograms.}

\item{tgc}{A vector of thermal growth coefficient(s).}
}
\value{
Returns a table with columns \code{INITIAL_WEIGHT},
\code{FINAL_WEIGHT}, \code{TGC}, and \code{TGC_DEGREE_DAYS}.
}
\description{
Use TGC model to calculate degree days required to grow from initial to final
weight
}
