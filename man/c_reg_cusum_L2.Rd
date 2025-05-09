% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cost_functions.R
\name{c_reg_cusum_L2}
\alias{c_reg_cusum_L2}
\title{Cost function [regression L2]}
\usage{
c_reg_cusum_L2(x, s = 1, e = length(x) + 1)
}
\arguments{
\item{x}{Univariate vector series, on which change points need to be found}

\item{s}{Start of the interval of interest [s, e)}

\item{e}{End of the interval of interest [s, e)}
}
\value{
L2 regression cost function
}
\description{
Cost function [regression L2]
}
