% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sota_algorithms.R
\name{ecp_func}
\alias{ecp_func}
\title{ECP change point algorithms}
\usage{
ecp_func(df, parameters = list(method = "e_agglo", params = "alpha1"))
}
\arguments{
\item{df}{data frame with one or multiple columns}

\item{parameters}{list of parameters for this algorithm}
}
\value{
data frame with the binarized prediction for each possible change point index
}
\description{
ECP change point algorithms
}
