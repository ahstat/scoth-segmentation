% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sota_output_tables.R
\name{mapping_family}
\alias{mapping_family}
\title{Mapping each algorithm to its family}
\usage{
mapping_family(algorithm_fullname = "ocp&l=200,a=10,b=0.1,k=1")
}
\arguments{
\item{algorithm_fullname}{string representing an algorithm with the selected
parameters}
}
\value{
a string with the corresponding family among `Penalty`, `Solution Path`,
`Misc` and `Zero`
}
\description{
Mapping each algorithm to its family
}
