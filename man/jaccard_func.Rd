% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/load_data.R
\name{jaccard_func}
\alias{jaccard_func}
\title{Compute the Jaccard index of two vectors}
\usage{
jaccard_func(x, y)
}
\arguments{
\item{x}{vector of index where a change is detected for a certain user}

\item{y}{vector of index where a change is detected for another user}
}
\value{
the Jaccard index computed as the size of the intersection over the
size of the union
}
\description{
Compute the Jaccard index of two vectors
}
