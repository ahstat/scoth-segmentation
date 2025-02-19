#' Compute evaluation metrics of each selection and save them on disk
#'
#' @param metric string representing the metrics of interest, either "F1" or "TCPD" (for F1-biased and cover)
#' @param groundtruth data frame with ground truth indexes obtained with `load_groundtruths`
#' @param OUTPUT_DATA_FOLDER filepath to the output data folder
#' @return save a RDS file for the evaluation results of each data set. Each file
#' is a data frame. For each cost_method, threshold, and level, get the
#' corresponding *evaluation* (list of metrics for each ground truth user),
#' along with additional information
#' @export
pipeline_evaluation_func = function(metric, groundtruth, OUTPUT_DATA_FOLDER) {
  logger::log_info(paste0("Begin the evaluation step for the ", metric, " metric"))
  files = list.files(file.path(OUTPUT_DATA_FOLDER, "algorithm_selection_results"))
  subfolder_evaluation = paste0("evaluation_results_", metric)
  folder_out = file.path(OUTPUT_DATA_FOLDER, subfolder_evaluation)
  if(!dir.exists(folder_out)) {
    dir.create(folder_out, showWarnings = FALSE, recursive = TRUE)
  }

  i=33
  for(i in 1:length(files)) {
    file = files[i]
    dataset_name = strsplit(file, "@")[[1]][1]
    filepath_evaluation_output = file.path(OUTPUT_DATA_FOLDER, subfolder_evaluation, file)
    filepath_selection_input = file.path(OUTPUT_DATA_FOLDER, "algorithm_selection_results", file)
    if(!file.exists(filepath_evaluation_output)) {
      logger::log_info(paste0("Evaluation for dataset ", file))
      if(!file.exists(filepath_selection_input)) {
        print("missing file (pass)")
      } else {
        df = readRDS(filepath_selection_input) %>%
          select(dataset_name, feature_name, cost_method, scoring_time_taken, selection_threshold_included, selection_level, selection)
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
  }
  logger::log_info(paste0("Evaluations for the ", metric, " metric saved in ", folder_out))
}

#' Compute the evaluation of a single data set
#'
#' @param df data frame with the *selection* obtained for each dataset, cost, threshold and level
#' @param groundtruth_node ground truth row giving the selected ground truth
#' indexes for each user for this data set
#' @param metric string representing the metrics of interest, either "F1" or "TCPD" (for F1-biased and cover)
#' @return a data frame giving, for each selection and each ground truth user,
#' the *evaluation* (list of metrics for each ground truth user), along with
#' additional information
pipeline_evaluation_single_selection = function(df, groundtruth_node, metric = "F1") {
  colnames_groundtruths = colnames(groundtruth_node)
  df_out = df
  df_out$evaluation = vector(mode = 'list', length = nrow(df_out))

  k=1
  colname_gt = colnames_groundtruths[1]
  for(k in 1:nrow(df)) { # loop on (cost, scoring, selection) possibilities
    #if(k %% 100 == 0) {
    #  cat(paste0(k, "/", nrow(df), " "))
    #}
    evaluation_current = list()
    evaluation_current[[metric]] = list()

    if(metric == "F1") {
      # In this case, get the F1 metric for each user labels individually
      for(colname_gt in colnames_groundtruths) { # loop on each user
        index_to_replace = groundtruth_node[[colname_gt]][[1]]
        if(is.null(index_to_replace)) { # case of a NULL ground truth == the user did not look at this series (/neq empty labels)
          binarization = NULL
        } else {
          binarization = df$selection[[k]]
          binarization$groundtruth = 0
          binarization$groundtruth[which(binarization$index %in% index_to_replace)] = 1
        }
        evaluation_current[[metric]][[colname_gt]] = metrics_F1_func(binarization)
      }
    } else if(metric == "TCPD") {
      # In this case, get the metrics for all users together following the metrics in TCPD
      if(length(colnames_groundtruths) == 9) {
        if(all(colnames_groundtruths == c("groundtruth", paste0("user_", c(6:10, 12:14))))) {
          colnames_groundtruths = colnames_groundtruths[-1]
        }
      }
      binarization = df$selection[[k]]
      for(colname_gt in colnames_groundtruths) { # loop on each groundtruth before the computation
        index_to_replace = groundtruth_node[[colname_gt]][[1]]
        if(!is.null(index_to_replace)) {
          binarization[[colname_gt]] = 0
          binarization[[colname_gt]][which(binarization$index %in% index_to_replace)] = 1
        }
      }
      # Get the metrics given all the binarizations combined
      evaluation_current[[metric]][["groundtruth"]] = metrics_TCPD_func(binarization)
    } else {
      stop("Unknown evaluation metric")
    }

    df_out$evaluation[[k]] = evaluation_current
  }
  return(df_out)
}
