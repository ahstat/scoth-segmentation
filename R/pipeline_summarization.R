#' Summarize all the evaluation results into a single file, for each metric
#'
#' @param metric string representing the metrics of interest, either "F1" or "TCPD" (for F1-biased and cover)
#' @param OUTPUT_DATA_FOLDER filepath to the output data folder
#' @return save a RDS file for the summarized results. Each file
#' is a data frame. For each dataset_name, cost_method, threshold, level, and
#' ground truth user, report the scores (F1 score, F1-biased score or cover)
#' @export
pipeline_summarization_func = function(metric, OUTPUT_DATA_FOLDER) {
  logger::log_info(paste0("Begin the summarization step for the ", metric, " metric"))
  subfolder_evaluation = paste0("evaluation_results_", metric)
  subfolder_summarization = paste0("summarizing_results_", metric)
  files = list.files(file.path(OUTPUT_DATA_FOLDER, subfolder_evaluation))
  folder_out = file.path(OUTPUT_DATA_FOLDER, subfolder_summarization)
  if(!dir.exists(folder_out)) {
    dir.create(folder_out, showWarnings = FALSE, recursive = TRUE)
  }
  fileout = file.path(folder_out, "summary.RDS")
  if(!file.exists(fileout)) {
    i=33
    df_list = list()
    for(i in 1:length(files)) {
      file = files[i]
      logger::log_info(paste0("Summarization for dataset ", file))
      filepath_evaluation_input = file.path(OUTPUT_DATA_FOLDER, subfolder_evaluation, file)
      if(!file.exists(filepath_evaluation_input)) {
        stop("missing file, stop")
      } else {
        df = readRDS(filepath_evaluation_input)
        df = summarize_current_experiment_evaluation(df)
        df_list[[i]] = df
      }
    }
    df = bind_rows(df_list)
    df$algorithm = paste0(df$cost_method, " x ", df$selection_level)
    df = df %>% filter(!is.na(nb_gt)) # remove users that did not label some experiments
    saveRDS(df, fileout)
  }
  logger::log_info(paste0("Summarization for the ", metric, " metric saved in ", folder_out))
}

#' Summarize a single dataset evaluation result
#'
#' @param df data frame with the *evaluation* obtained for each dataset, cost, threshold, level
#' @return the same evaluation with the ground truth user added as an additional column
summarize_current_experiment_evaluation = function(df) {
  if(!"evaluation" %in% colnames(df)) {
    stop("df should contain the evaluation column")
  }
  colnames_groundtruths = names(df$evaluation[[1]][[1]])
  metric = names(df$evaluation[[1]])[1]
  df_out = list()
  df_tmp = df %>% select(-"evaluation") # elements will be extracted from this list column

  if(metric == "F1") {
    for(colname_gt in colnames_groundtruths) {
      df_cur = df_tmp
      df_cur$groundtruth_name = colname_gt # current user
      df_cur$metric = metric
      df_cur$F1 = sapply(df$evaluation, function(x){x[[metric]][[colname_gt]]$F1})
      df_cur$nb_gt = sapply(df$evaluation, function(x){x[[metric]][[colname_gt]]$nb_gt})
      df_cur$nb_pred = sapply(df$evaluation, function(x){x[[metric]][[colname_gt]]$nb_pred})
      df_out[[length(df_out)+1]] = df_cur
    }
  } else if(metric == "TCPD") {
    colname_gt = "groundtruth" # the contribution of the different users have been merged already
    df_cur = df_tmp
    df_cur$groundtruth_name = colname_gt
    df_cur$metric = metric
    df_cur$F1 = sapply(df$evaluation, function(x){x[[metric]][[colname_gt]]$F1})
    df_cur$covering = sapply(df$evaluation, function(x){x[[metric]][[colname_gt]]$covering})
    df_cur$nb_gt = sapply(df$evaluation, function(x){x[[metric]][[colname_gt]]$nb_gt})
    df_cur$nb_pred = sapply(df$evaluation, function(x){x[[metric]][[colname_gt]]$nb_pred})
    df_out[[length(df_out)+1]] = df_cur
  } else {
    stop("Unknown metric")
  }

  df_out = bind_rows(df_out)
  return(df_out)
}
