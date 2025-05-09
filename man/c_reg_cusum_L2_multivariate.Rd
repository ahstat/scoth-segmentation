% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cost_functions.R
\name{c_reg_cusum_L2_multivariate}
\alias{c_reg_cusum_L2_multivariate}
\title{Cost function [regression L2] for the multivariate case}
\usage{
c_reg_cusum_L2_multivariate(
  x = data.frame(x1 = rnorm(100), x2 = rnorm(100)),
  s = 1,
  e = nrow(x) + 1,
  normalization = FALSE
)
}
\arguments{
\item{x}{Multivariate data frame series, on which change points need to be found}

\item{s}{Start of the interval of interest [s, e)}

\item{e}{End of the interval of interest [s, e)}
}
\value{
L2 regression cost function
}
\description{
Cost function [regression L2] for the multivariate case
}
