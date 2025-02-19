library(scoth)
library(dplyr)
library(tidyr)
library(tibble)
library(jsonlite)
library(logger)
# additional packages needed:
#  - in R: breakfast, changepoint, changepoint.np, ecp, genlasso, ocp, robseg
#  - in python: changeforest, river
library(reticulate)
library(scmamp)
options(warn=1)

## Parameters ----
PYTHON_FOLDER = "/home/x/.conda/envs/y/bin/python" # with `x` the username, and `y` the conda environment
PUBLIC_DATA_FOLDER = file.path(here::here(), "public-data")
OUTPUT_DATA_FOLDER = file.path(here::here(), "output-data")

## Loading benchmark data and ground truth ----
loaded_kpis = load_kpis(PUBLIC_DATA_FOLDER)
groundtruth = load_groundtruths(PUBLIC_DATA_FOLDER)

## Pipeline ----
results = list()
for(algorithm_name in all_sota_algorithms()) {
  results[[algorithm_name]] = pipeline_selection_func_sota(algorithm_name, loaded_kpis, OUTPUT_DATA_FOLDER)
}
results = bind_rows(results)

metric = "F1" # F1 metric
pipeline_evaluation_func_sota(metric, groundtruth, OUTPUT_DATA_FOLDER)
pipeline_summarization_func(metric, file.path(OUTPUT_DATA_FOLDER, "sota"))

metric = "TCPD" # F1-biased and cover metrics from the TCPD paper
pipeline_evaluation_func_sota(metric, groundtruth, OUTPUT_DATA_FOLDER)
pipeline_summarization_func(metric, file.path(OUTPUT_DATA_FOLDER, "sota"))

## Result tables ----
get_oracle_table_sota(OUTPUT_DATA_FOLDER) # table left
get_single_best_table_sota(OUTPUT_DATA_FOLDER) # table right

## Result CD plot ----
plot_cd_figures(OUTPUT_DATA_FOLDER)
