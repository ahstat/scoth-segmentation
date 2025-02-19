#' Expand grid helper for `build_grid`
#'
#' @param ... Arguments, each of them being a numeric or string vector (no list),
#' e.g. `...` is `p=c(1,2),method=c("a","b")`
#' @returns Data frame output of the grid of possible combinations of the output,
#' names with the name of the original arguments, and with the increment begining
#' by the last columns. Please use `build_grid` for change point detection, this
#' is only an internal helper
expand_grid_df = function(...) {
  # rev added to increment the parameters in the order of reading
  expand.grid.df = function(...) Reduce(function(...) merge(..., by=NULL), rev(list(...)))
  df = expand.grid.df(...)
  if(class(df) != "data.frame") {
    df = data.frame(df)
  }
  colnames(df) = names(rev(list(...)))
  return(rev(df))
}

#' Convert a univariate algorithm to the multivariate setting
#'
#' This function is used for the algorithms that can originally only run on
#' univariate series. The final predictions are obtained by taking the union of
#' all the predictions made (as done by PELT in the changepoint package)
#'
#' @param f change point detection algorithm giving as input a univariate series
#' and a list of parameters, and outputting a data.frame with index and predictions
#' @return function that returns the aggregated data.frame of the predictions
#' obtained by applying f on each univariate series
multivariate_func = function(f) {
  # merge the different predictions by taking the union of predictions,
  # since some algorithms do not manage multivariate series
  function(x, parameters) {
    res = list()
    for(j in 1:length(x)) {
      res[[j]] = f(x[[j]], parameters)
    }
    selection = data.frame(index = res[[1]]$index, predictions = apply(sapply(res, function(x){x$predictions}), 1, max))
    return(selection)
  }
}

#' Compute binarized selections for a given algorithm for state-of-the-art algorithms
#' and save them on disk
#'
#' @param algorithm_name change point algorithm name
#' @param loaded_kpis data frame containing the `kpis` series (each univariate or multivariate)
#' and the corresponding `dataset_name`
#' @param OUTPUT_DATA_FOLDER filepath to the output data folder
#' @return save a RDS file for the selection results of each data set. For each
#' parameter of the algorithm, get the corresponding *selection* (data frame with
#' the 0/1 result obtained for each possible change point index)
#' @export
pipeline_selection_func_sota = function(algorithm_name, loaded_kpis, OUTPUT_DATA_FOLDER) {
  f = algorithm_name_to_function(algorithm_name)
  df_parameters = algorithm_name_to_parameters(algorithm_name)
  folderpath = file.path(OUTPUT_DATA_FOLDER, "sota", "algorithm_selection_results", algorithm_name)
  filepath = file.path(folderpath, "results.RDS")
  if(!file.exists(filepath)) {
    logger::log_info(paste0("Begin the computations for the ", algorithm_name, " algorithm"))
    df_selection = list()
    dir.create(file.path(folderpath, "parts"), showWarnings = FALSE, recursive = TRUE)
    for(k in 1:nrow(df_parameters)) { # loop over parameters
      p = df_parameters[k,]
      filepath_cur_hyper = file.path(folderpath, "parts", paste0(k, ".RDS"))
      algorithm_fullname = paste0(algorithm_name, "&", paste(paste0(paste(names(p)), "=", paste(p)), collapse=","))
      if(!file.exists(filepath_cur_hyper)) {
        logger::log_info(paste0(k,"/",nrow(df_parameters), " [", algorithm_fullname, "]"))
        df_selection_cur_hyper = list()
        for(i in 1:length(loaded_kpis$kpis)) { # loop over datasets
          dataset_name = loaded_kpis$dataset_name[i]
          logger::log_info(dataset_name)
          x = loaded_kpis$kpis[[i]]
          feature_name = ifelse(length(x) == 1, names(x)[1], "aggregate")
          parameters = as.list(p)
          start_time = Sys.time()
          selection = f(x, parameters)
          time_taken = as.numeric(difftime(Sys.time(), start_time, units = "secs")) # always in seconds
          df_selection_cur_hyper[[i]] = data.frame(dataset_name = dataset_name,
                                                   feature_name = feature_name,
                                                   algorithm_name = algorithm_name,
                                                   algorithm_fullname = algorithm_fullname,
                                                   time_taken = time_taken,
                                                   has_error = FALSE)
          df_selection_cur_hyper[[i]]$x = list(x)
          df_selection_cur_hyper[[i]]$selection = list(selection)
        }
        df_selection_cur_hyper = bind_rows(df_selection_cur_hyper)
        saveRDS(df_selection_cur_hyper, filepath_cur_hyper)
      }
      df_selection[[k]] = readRDS(filepath_cur_hyper)
    }
    df_selection = bind_rows(df_selection)
    saveRDS(df_selection, filepath)
    logger::log_info(paste0("Selections for ", algorithm_name, " algorithm saved in ", filepath))
  }
  df_selection = readRDS(filepath)
  return(df_selection)
}

#' Compute evaluation metrics of each selection for state-of-the-art algorithms
#' and save them on disk
#'
#' @param metric string representing the metrics of interest, either "F1" or "TCPD" (for F1-biased and cover)
#' @param groundtruth data frame with ground truth indexes obtained with `load_groundtruths`
#' @param OUTPUT_DATA_FOLDER filepath to the output data folder
#' @return save a RDS file for the evaluation results of each data set. Each file
#' is a data frame. For each parameter of each sota algorithm, get the
#' corresponding *evaluation* (list of metrics for each ground truth user),
#' along with additional information
#' @export
pipeline_evaluation_func_sota = function(metric, groundtruth, OUTPUT_DATA_FOLDER) {
  logger::log_info(paste0("Begin the evaluation step for the ", metric, " metric (using sota algorithms)"))
  results = list()
  for(algorithm_name in all_sota_algorithms()) {
    results[[algorithm_name]] = pipeline_selection_func_sota(algorithm_name, loaded_kpis, OUTPUT_DATA_FOLDER)
  }
  results = bind_rows(results)
  dataset_names = unique(results$dataset_name)

  subfolder_evaluation = paste0("evaluation_results_", metric)
  folder_out = file.path(OUTPUT_DATA_FOLDER, "sota", subfolder_evaluation)
  if(!dir.exists(folder_out)) {
    dir.create(folder_out, showWarnings = FALSE, recursive = TRUE)
  }

  for(i in 1:length(dataset_names)) {
    dataset_name = dataset_names[i]
    df = results %>% filter(dataset_name == !!dataset_name)
    file = paste0(paste(df$dataset_name[1], df$feature_name[1], sep = "@"), ".RDS")
    filepath_evaluation_output = file.path(folder_out, file)
    if(!file.exists(filepath_evaluation_output)) {
      logger::log_info(paste0("Evaluation for dataset ", file))
      groundtruth_current_idx = which(rownames(groundtruth) == dataset_name)
      if(length(groundtruth_current_idx) != 1) {
        stop("Cannot find the ground truth index for this dataset_name")
      }
      groundtruth_node = groundtruth[groundtruth_current_idx,,drop=FALSE]
      if(nrow(groundtruth_node) != 1) {
        stop("groundtruth_node has not the right shape")
      }
      # evaluate and add the column to the original df
      df_out = pipeline_evaluation_single_selection(df, groundtruth_node, metric)
      df_out = df_out %>% select(-selection)
      saveRDS(df_out, filepath_evaluation_output)
    }
  }
  logger::log_info(paste0("Evaluations for the ", metric, " metric (using sota algorithms) saved in ", folder_out))
}
