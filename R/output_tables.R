#' Function for reproducing the results using our methodology in the oracle setting
#'
#' @param OUTPUT_DATA_FOLDER filepath to the output data folder
#' @return the average scores and deviations for the three metrics
#' @export
get_oracle_table = function(OUTPUT_DATA_FOLDER) {
  logger::log_info(paste0("Result for our methodology in the oracle setting"))
  df1 = get_oracle(metric = "F1", OUTPUT_DATA_FOLDER)
  df2 = get_oracle(metric = "F1_biased", OUTPUT_DATA_FOLDER)
  df3 = get_oracle(metric = "cover", OUTPUT_DATA_FOLDER)
  output_filepath = file.path(OUTPUT_DATA_FOLDER, "table_left.txt")
  output = cbind(df1, df2, df3)
  sink(output_filepath)
  print(output)
  sink()
  logger::log_info(paste0("Result saved in ", output_filepath))
  return(output)
}

#' Function for reproducing the results using our methodology in the oracle setting for a single metric
#'
#' @param metric string representing the metrics of interest, within: "F1", "F1_biased" or "cover"
#' @param OUTPUT_DATA_FOLDER filepath to the output data folder
#' @return the average scores and deviations for a single metric
get_oracle = function(metric = "F1", OUTPUT_DATA_FOLDER, full_results = FALSE) {
  if(metric == "F1") {
    metric = "F1"
    eval = "F1"
    final_name = "F1"
  } else if(metric == "F1_biased") {
    metric = "TCPD"
    eval = "F1"
    final_name = "F1_biased"
  } else if(metric == "cover") {
    metric = "TCPD"
    eval = "covering"
    final_name = "cover"
  }

  ## Take the threshold giving the best F1 score for each experiment
  # 1. arrange by F1 to get the largest F1-score over all the thresholds
  # 2. arrange by threshold to get the largest possible threshold for ties
  results_best_F1 = load_summary_func_initial(OUTPUT_DATA_FOLDER, metric) %>%
    group_by(dataset_name, feature_name, cost_method, selection_level, groundtruth_name, metric) %>%
    arrange(F1, selection_threshold_included, .by_group = TRUE) %>%
    filter(row_number()==n()) %>%
    ungroup()

  results_best_F1 = results_best_F1 %>%
    rename(eval = !!eval) %>%
    filter(groundtruth_name == "groundtruth") %>% # only needed for the F1 metric
    select(algorithm, dataset_name, eval) %>%
    group_by(dataset_name) %>%
    arrange(-eval) %>%
    slice(1) %>%
    ungroup()

  if(full_results) {
    results_best_F1 = results_best_F1 %>%
      select(-algorithm) %>%
      mutate(algorithm_name = "Ours") %>%
      select(algorithm_name, everything())
    return(results_best_F1)
  }

  results_best_F1 = results_best_F1 %>%
    summarize(eval_mean = round(mean(eval),2), eval_sd = round(sd(eval),2), .groups="drop") %>%
    mutate(eval = paste0(eval_mean, "±", eval_sd)) %>%
    select(eval) %>%
    rename(!!final_name := eval)
  return(results_best_F1)
}

#' Function for reproducing the results using our methodology in the single-best setting
#'
#' @param OUTPUT_DATA_FOLDER filepath to the output data folder
#' @return the average score and deviation, along with the selected best setting
#' @export
get_single_best_table = function(OUTPUT_DATA_FOLDER, full_results = FALSE) {
  logger::log_info(paste0("Result for our methodology in the single-best setting"))
  metric = "F1"

  res = load_summary_func_initial(OUTPUT_DATA_FOLDER, metric) %>%
    filter(groundtruth_name == "groundtruth") %>%
    rename(threshold = selection_threshold_included) %>%
    group_by(dataset_name, cost_method, threshold) %>%
    arrange(-F1) %>%
    slice(1) %>%
    ungroup()

  output = res %>%
    group_by(cost_method, threshold) %>%
    summarize(eval_mean = round(mean(F1),2), eval_sd = round(sd(F1),2),
              time_taken_mean = round(mean(scoring_time_taken),1), time_taken_sd = round(sd(scoring_time_taken),1),
              count = n(), .groups="drop") %>%
    arrange(-eval_mean) %>%
    slice(1)

  if(output$count != 42) {
    # we should average over the 42 datasets
    stop("Issue in the computations")
  }

  if(full_results) {
    res_full = res %>%
      mutate(algorithm_name = "Ours") %>%
      filter(cost_method %in% output$cost_method, threshold %in% output$threshold) %>%
      select(dataset_name, algorithm_name, F1)
    return(res_full)
  }

  output_filepath = file.path(OUTPUT_DATA_FOLDER, "table_right.txt")
  output = output %>%
    mutate(eval = paste0(eval_mean, "±", eval_sd),
           `time_taken (s)` = paste0(time_taken_mean, "±", time_taken_sd)) %>%
    select(eval, `time_taken (s)`) %>%
    mutate(eval = paste0(eval, " (", output$cost_method, "; threshold=", output$threshold, ")")) %>%
    rename(!!metric := eval)
  sink(output_filepath)
  print(output)
  sink()
  logger::log_info(paste0("Result for our methodology saved in ", output_filepath))
  return(output)
}

#' Helper for loading the summarized results (obtained from the `pipeline_summarization_func` function)
#'
#' @param OUTPUT_DATA_FOLDER filepath to the output data folder
#' @param metric string representing the metrics of interest, either "F1" or "TCPD" (for F1-biased and cover)
#' @return the data frame giving the metrics (F1 scores and additional metrics) for
#' each dataset, cost method, threshold, level, and ground truth user
load_summary_func_initial = function(OUTPUT_DATA_FOLDER, metric = "F1") {
  results = readRDS(file.path(OUTPUT_DATA_FOLDER, paste0("summarizing_results_", metric), "summary.RDS"))
  return(results)
}
