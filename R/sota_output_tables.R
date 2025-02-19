#' Function for reproducing the results using the for state-of-the-art algorithms
#' in the single-best setting
#'
#' @param OUTPUT_DATA_FOLDER filepath to the output data folder
#' @param full_results whether to output the full results or the summarized table.
#' With FALSE: outputs the summarized table that averages the
#' F1 score over the datasets. With TRUE: output the data frame with the F1 score
#' for each dataset
#' @param mapping_sota function mapping the algorithm_fullname to its family.
#' With `NULL`: keep the original algorithm_name (corresponding to the package from
#' which the method has been computed). With `mapping_detailed`: separate the
#' different search methods, resulting in 15+ rows; With `mapping_family`: group
#' the algorithm based on their family (selection path, penalty, misc)
#' @return a data frame, with the average scores and deviations for each method,
#' using the single best setting for each algorithm
#' @export
get_single_best_table_sota = function(OUTPUT_DATA_FOLDER, full_results = FALSE, mapping_sota = mapping_family) {
  OUTPUT_DATA_SOTA_FOLDER = file.path(OUTPUT_DATA_FOLDER, "sota")
  logger::log_info(paste0("Result for sota methodologies in the single-best setting"))
  metric = "F1"

  res = load_summary_func_initial(OUTPUT_DATA_SOTA_FOLDER, metric) %>%
    filter(groundtruth_name == "groundtruth")

  if(!is.null(mapping_sota)) {
    res$algorithm_name = sapply(res$algorithm_fullname, mapping_sota)
  }

  # best single result per algorithm_name
  output = res %>%
    group_by(algorithm_name, algorithm_fullname) %>%
    summarize(eval_mean = round(mean(F1),2), eval_sd = round(sd(F1),2),
              time_taken_mean = round(mean(time_taken),4), time_taken_sd = round(sd(time_taken),4),
              count = n(), .groups="drop") %>%
    group_by(algorithm_name) %>%
    arrange(-eval_mean) %>%
    slice(1)

  if(!all(output$count == 42)) {
    # we should average over the 42 datasets
    stop("Issue in the computations")
  }

  if(full_results) {
    res_full = res %>%
      filter(algorithm_fullname %in% output$algorithm_fullname) %>%
      select(dataset_name, algorithm_name, F1) %>%
      filter(algorithm_name != "Zero")
    return(res_full)
  }

  output_filepath = file.path(OUTPUT_DATA_SOTA_FOLDER, "table_right.txt")
  output = output %>%
    mutate(eval = paste0(eval_mean, "±", eval_sd)) %>%
    select(algorithm_name, algorithm_fullname, eval) %>%
    rename(!!metric := eval)
  sink(output_filepath)
  print(output)
  sink()
  logger::log_info(paste0("Result for sota methodology saved in ", output_filepath))
  return(output)
}

#' Function for reproducing the results using for state-of-the-art algorithms
#' in the oracle setting
#'
#' @param OUTPUT_DATA_FOLDER filepath to the output data folder
#' @param mapping_sota function mapping the algorithm_fullname to its family.
#' With `NULL`: keep the original algorithm_name (corresponding to the package from
#' which the method has been computed). With `mapping_detailed`: separate the
#' different search methods, resulting in 15+ rows; With `mapping_family`: group
#' the algorithm based on their family (chain of subsets, selection path, penalty,
#' or misc)
#' @return a data frame, with the average scores and deviations for each method,
#' using the oracle setting (algorithm parameters reaching the best performance
#' for each dataset), and for the three evaluation metrics
#' @export
get_oracle_table_sota = function(OUTPUT_DATA_FOLDER, mapping_sota = mapping_family) {
  logger::log_info(paste0("Result for the sota methodologies in the oracle setting"))
  df1 = get_oracle_sota(metric = "F1", OUTPUT_DATA_FOLDER, FALSE, mapping_sota)
  df2 = get_oracle_sota(metric = "F1_biased", OUTPUT_DATA_FOLDER, FALSE, mapping_sota)
  df3 = get_oracle_sota(metric = "cover", OUTPUT_DATA_FOLDER, FALSE, mapping_sota)
  OUTPUT_DATA_SOTA_FOLDER = file.path(OUTPUT_DATA_FOLDER, "sota")
  output_filepath = file.path(OUTPUT_DATA_SOTA_FOLDER, "table_left.txt")
  output = cbind(df1, df2, df3)
  sink(output_filepath)
  print(output)
  sink()
  logger::log_info(paste0("Result saved in ", output_filepath))
  return(output)
}

#' Function for reproducing the results using for state-of-the-art algorithms
#' in the oracle setting for a single evaluation metric
#'
#' @param metric string representing the metrics of interest, within: "F1", "F1_biased" or "cover"
#' @param OUTPUT_DATA_FOLDER filepath to the output data folder
#' @param full_results whether to output the full results or the summarized table.
#' With FALSE: outputs the summarized table that averages the
#' F1 score over the datasets. With TRUE: output the data frame with the F1 score
#' for each dataset
#' @param mapping_sota function mapping the algorithm_fullname to its family.
#' With `NULL`: keep the original algorithm_name (corresponding to the package from
#' which the method has been computed). With `mapping_detailed`: separate the
#' different search methods, resulting in 15+ rows; With `mapping_family`: group
#' the algorithm based on their family (selection path, penalty, misc)
#'
#' @return a data frame, with the average scores and deviations for each method,
#' using the oracle setting (algorithm parameters reaching the best performance
#' for each dataset), and for a single evaluation metric
get_oracle_sota = function(metric = "F1", OUTPUT_DATA_FOLDER, full_results = FALSE, mapping_sota = mapping_family) {
  OUTPUT_DATA_SOTA_FOLDER = file.path(OUTPUT_DATA_FOLDER, "sota")
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

  ## Take the hyperparameters giving the best F1 score for each algorithm
  results_best_F1 = load_summary_func_initial(OUTPUT_DATA_SOTA_FOLDER, metric)

  if(!is.null(mapping_sota)) {
    results_best_F1$algorithm_name = sapply(results_best_F1$algorithm_fullname, mapping_sota)
  }

  results_best_F1 = results_best_F1 %>%
    group_by(dataset_name, feature_name, algorithm_name, groundtruth_name, metric) %>%
    arrange(F1, .by_group = TRUE) %>%
    filter(row_number()==n()) %>%
    ungroup()

  results_best_F1 = results_best_F1 %>%
    rename(eval = !!eval) %>%
    filter(groundtruth_name == "groundtruth") %>% # only needed for the F1 metric
    select(algorithm_name, dataset_name, eval) %>%
    group_by(algorithm_name, dataset_name) %>%
    arrange(-eval) %>%
    slice(1) %>%
    ungroup()

  if(full_results) {
    return(results_best_F1 %>% filter(algorithm_name != "Zero"))
  }

  results_best_F1 = results_best_F1 %>%
    group_by(algorithm_name) %>%
    summarize(eval_mean = round(mean(eval),2), eval_sd = round(sd(eval),2), .groups="drop") %>%
    mutate(eval = paste0(eval_mean, "±", eval_sd)) %>%
    select(algorithm_name, eval) %>%
    rename(!!final_name := eval)
  return(results_best_F1)
}

#' Mapping each algorithm to its family
#'
#' @param algorithm_fullname string representing an algorithm with the selected
#' parameters
#'
#' @return a string with the corresponding family among `Penalty`, `Solution Path`,
#' `Misc` and `Zero`
mapping_family = function(algorithm_fullname = "ocp&l=200,a=10,b=0.1,k=1") {
  algorithm_name = strsplit(algorithm_fullname, "&")[[1]][1]
  if(algorithm_name == "adwin") {
    return("Penalty")
  } else if(algorithm_name == "breakfast") {
    return("Solution Path")
  } else if(algorithm_name == "changeforest") {
    return("Solution Path")
  } else if(algorithm_name == "cpnp") {
    return("Penalty")
  } else if(algorithm_name == "ecp") {
    return("Misc")
  } else if(algorithm_name == "killick") {
    part2 = strsplit(algorithm_fullname, "&")[[1]][2]
    search = strsplit(part2, ",", fixed = TRUE)[[1]][2]
    search = strsplit(search, "=")[[1]][2]
    if(search == "BinSeg") {
      # BinSeg selects one change point at each iteration, without
      # solving an optimization problem, and belongs to the Solution Path
      # family
      return("Solution Path")
    } else {
      return("Penalty")
    }
  } else if(algorithm_name == "lasso") {
    return("Penalty")
  } else if(algorithm_name == "ocp") {
    return("Misc")
  } else if(algorithm_name == "rfpop") {
    return("Penalty")
  } else if(algorithm_name == "zero") {
    return("Zero")
  } else {
    stop("Unknown algorithm_name")
  }
}

#' Mapping each algorithm to its search method
#'
#' @param algorithm_fullname string representing an algorithm with the selected
#' parameters
#'
#' @return a string with the corresponding search method
mapping_detailed = function(algorithm_fullname = "ocp&l=200,a=10,b=0.1,k=1") {
  algorithm_name = strsplit(algorithm_fullname, "&")[[1]][1]
  if(algorithm_name == "adwin") {
    return("adwin")
  } else if(algorithm_name == "breakfast") {
    part2 = strsplit(algorithm_fullname, "&")[[1]][2]
    search = strsplit(part2, ",", fixed = TRUE)[[1]][1]
    search = strsplit(search, "=")[[1]][2]
    return(search)
  } else if(algorithm_name == "changeforest") {
    return("changeforest")
  } else if(algorithm_name == "cpnp") {
    return("cpnp")
  } else if(algorithm_name == "ecp") {
    part2 = strsplit(algorithm_fullname, "&")[[1]][2]
    search = strsplit(part2, ",", fixed = TRUE)[[1]][1]
    search = strsplit(search, "=")[[1]][2]
    if(search == "kcpa") {
      return("kcpa")
    } else {
      return("ecp")
    }
  } else if(algorithm_name == "killick") {
    part2 = strsplit(algorithm_fullname, "&")[[1]][2]
    search = strsplit(part2, ",", fixed = TRUE)[[1]][2]
    search = strsplit(search, "=")[[1]][2]
    return(search)
  } else if(algorithm_name == "lasso") {
    return("lasso")
  } else if(algorithm_name == "ocp") {
    return("ocp")
  } else if(algorithm_name == "rfpop") {
    return("rfpop")
  } else if(algorithm_name == "zero") {
    return("zero")
  } else {
    stop("Unknown algorithm_name")
  }
}
