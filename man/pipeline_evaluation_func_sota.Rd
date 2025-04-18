% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sota_results.R
\name{pipeline_evaluation_func_sota}
\alias{pipeline_evaluation_func_sota}
\title{Compute evaluation metrics of each selection for state-of-the-art algorithms
and save them on disk}
\usage{
pipeline_evaluation_func_sota(metric, groundtruth, OUTPUT_DATA_FOLDER)
}
\arguments{
\item{metric}{string representing the metrics of interest, either "F1" or "TCPD" (for F1-biased and cover)}

\item{groundtruth}{data frame with ground truth indexes obtained with `load_groundtruths`}

\item{OUTPUT_DATA_FOLDER}{filepath to the output data folder}
}
\value{
save a RDS file for the evaluation results of each data set. Each file
is a data frame. For each parameter of each sota algorithm, get the
corresponding *evaluation* (list of metrics for each ground truth user),
along with additional information
}
\description{
Compute evaluation metrics of each selection for state-of-the-art algorithms
and save them on disk
}
