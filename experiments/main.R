library(scoth)
library(dplyr)
library(tidyr)
library(tibble)
library(jsonlite)
library(logger)
# libraries used for the figure
library(ggplot2)
library(gridExtra)
library(grid)

## Parameters ----
PUBLIC_DATA_FOLDER = file.path(here::here(), "public-data")
OUTPUT_DATA_FOLDER = file.path(here::here(), "output-data")
cost_methods = c("reg_cusum_L2", "cusum_L2")
thresholds = c(seq(from = 0.03, to = 0.2, by = 0.01), 0.3, 0.4, 0.5, 0.6, 0.7, 1)
nb_levels = 10

## Loading benchmark data and ground truth ----
loaded_kpis = load_kpis(PUBLIC_DATA_FOLDER)
groundtruth = load_groundtruths(PUBLIC_DATA_FOLDER)

## Pipeline ----
pipeline_scoring_func(loaded_kpis, cost_methods, OUTPUT_DATA_FOLDER)
pipeline_selection_func(thresholds, nb_levels, OUTPUT_DATA_FOLDER)

metric = "F1" # F1 metric
pipeline_evaluation_func(metric, groundtruth, OUTPUT_DATA_FOLDER)
pipeline_summarization_func(metric, OUTPUT_DATA_FOLDER)

metric = "TCPD" # F1-biased and cover metrics from the TCPD paper
pipeline_evaluation_func(metric, groundtruth, OUTPUT_DATA_FOLDER)
pipeline_summarization_func(metric, OUTPUT_DATA_FOLDER)

## Result tables ----
get_oracle_table(OUTPUT_DATA_FOLDER) # table left
get_single_best_table(OUTPUT_DATA_FOLDER) # table right

## Result figure ----
plot_figure_qualitative_results(OUTPUT_DATA_FOLDER, groundtruth) # last figure
