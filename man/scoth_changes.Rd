% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/scoth_scoring_algorithm.R
\name{scoth_changes}
\alias{scoth_changes}
\title{Remove the `i`th change point and update the costs
Policy: Taking the maximum gain seen up to now, and removing also from the max vector}
\usage{
scoth_changes(
  x,
  cost_func = c_cusum_L2,
  s = 1,
  e = length(x) + 1,
  verbose = TRUE
)
}
\arguments{
\item{x}{univariate or multivariate vector series, on which change points need to be found}

\item{cost_func}{a cost function}

\item{s}{start of the interval of interest [s, e)}

\item{e}{end of the interval of interest [s, e)}

\item{verbose}{whether to print advance of the algorithm}
}
\value{
vector of length the number of possible change points, with the gain
w.r.t to the global cost of [s, e).
}
\description{
Remove the `i`th change point and update the costs
Policy: Taking the maximum gain seen up to now, and removing also from the max vector
}
