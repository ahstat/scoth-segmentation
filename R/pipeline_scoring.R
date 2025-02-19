#' Compute scorings of all the data sets and save them on disk
#'
#' @param loaded_kpis data frame containing the `kpis` series (each univariate or multivariate)
#' and the corresponding `dataset_name`
#' @param cost_methods vector of cost functions names among the available cost functions
#' @param OUTPUT_DATA_FOLDER filepath to the output data folder
#' @return save a RDS file for the scoring results of each data set, each file
#' being a data frame with `length(cost_methods)` rows, each row containing the
#' *scoring* (data frame with the score obtained for each possible change point index),
#' along additional information
#' @export
pipeline_scoring_func = function(loaded_kpis, cost_methods, OUTPUT_DATA_FOLDER) {
  logger::log_info("Begin the scoring step")
  my_folder = file.path(OUTPUT_DATA_FOLDER, "algorithm_scoring_results")
  dir.create(my_folder, showWarnings = FALSE, recursive = TRUE)
  row = 2
  for(row in 1:nrow(loaded_kpis)) {
    loaded_kpis_cur = loaded_kpis[row,]
    dataset_name = loaded_kpis_cur$dataset_name
    kpis = loaded_kpis_cur$kpis[[1]]
    nb_features = length(colnames(kpis)) # number of features
    if(nb_features == 1) {
      x = kpis[,1]
      feature_name = names(kpis)[1]
    } else {
      x = kpis
      feature_name = "aggregate"
    }
    out_file = paste0(paste(dataset_name, feature_name, sep = "@"), ".RDS")
    file_out = file.path(my_folder, out_file)
    if(!file.exists(file_out)) {
      explicit_name = paste0("[", paste(dataset_name, feature_name, sep = " x "), "]")
      logger::log_info(paste0("Scoring for dataset ", paste0(row, "/", nrow(loaded_kpis)), " ", explicit_name))
      df = pipeline_scoring_series(x, cost_methods) # main function
      df$dataset_name = dataset_name
      df$feature_name = feature_name
      df = df %>% select(dataset_name, feature_name, everything())
      df$x = list(x) # easier to keep the original series in memory for the next parts
      saveRDS(df, file_out)
    }
  }
  logger::log_info(paste0("Scores saved in ", file.path(OUTPUT_DATA_FOLDER, "algorithm_scoring_results")))
}

#' Compute scoring of a single data set (univariate or multivariate)
#'
#' @param x univariate (vector) or multivariate (data.frame) series
#' @param cost_methods vector of cost functions names among the available cost functions
#' @return a data frame with `length(cost_methods)` rows, each row containing the
#' *scoring* (data frame with the score obtained for each possible change point index),
#' along with the `cost_method`, the `cost_initial` (initial cost of the whole series),
#' the `scoring_time_taken` (in seconds) and whether the `scoring_has_error`
pipeline_scoring_series = function(x, cost_methods = c("reg_cusum_L2", "cusum_L2")) {
  output = list()
  i=1
  for(i in 1:length(cost_methods)) {
    start.time <- Sys.time()
    out = tryCatch({scoth_scoring_func(x, cost_methods[i])},
                   error=function(cond) {return(NULL)},
                   finally={})
    end.time <- Sys.time()
    time.taken <- end.time - start.time
    out$has_error = is.null(out)
    out$time_taken_stage1 = time.taken
    output[[i]] = out
  }

  # Post processing
  df = data.frame(cost_method = cost_methods)

  # Extract uniform scores: list of data frames, each with 2 columns index
  # (from 2 to length(x)), and score, with the score for each index
  length_x = ifelse(class(x) == "data.frame", nrow(x), length(x))
  df$scoring = lapply(output, function(cur) {
    data.frame(index = 2:length_x, score = cur$changepoint_gain)
  })

  df$cost_initial = sapply(output, function(i){i$cost_initial})
  df$scoring_time_taken = sapply(output, function(i){i$time_taken_stage1})
  df$scoring_has_error = sapply(output, function(i){i$has_error})

  return(df)
}
