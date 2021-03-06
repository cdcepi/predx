% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/check_probs_sum_to_one.R
\name{check_probs_sum_to_one}
\alias{check_probs_sum_to_one}
\title{Check that probabilities sum to 1 (or approximately 1)}
\usage{
check_probs_sum_to_one(probs, msg = T, tolerance = .Machine$double.eps^0.5)
}
\arguments{
\item{probs}{vector of probabilities for a single forecast}

\item{tolerance}{tolerance relative to 1, default = .Machine$double.eps^0.5}
}
\value{
TRUE or error message
}
\description{
Check that probabilities sum to 1 (or approximately 1)
}
