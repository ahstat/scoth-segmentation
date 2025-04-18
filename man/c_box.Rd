% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cost_functions.R
\name{c_box}
\alias{c_box}
\title{Dummy cost function based on the area of the rectangle encompassing all the data points}
\usage{
c_box(x, s = 1, e = length(x) + 1)
}
\arguments{
\item{x}{Univariate vector series, on which change points need to be found}

\item{s}{Start of the interval of interest [s, e)}

\item{e}{End of the interval of interest [s, e)}
}
\value{
Box cost function
}
\description{
Dummy cost function based on the area of the rectangle encompassing all the data points
}
