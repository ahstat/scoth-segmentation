#' Compute selections of each scoring and save them on disk
#'
#' After loading the scorings of each dataset, apply the selection thresholds and
#' iterate over the levels, before finally saving each RDS (one for each dataset)
#' inside the OUTPUT_DATA_FOLDER
#'
#' @param thresholds vector of selected thresholds
#' @param nb_levels number of recursive levels
#' @param OUTPUT_DATA_FOLDER filepath to the output data folder
#' @return save a RDS file for the selection results of each data set. Each file
#' is a data frame. For each cost_method, threshold, and level, get the
#' corresponding *selection* (data frame with the 0/1 result obtained for each
#' possible change point index), along with additional information
#' @export
pipeline_selection_func = function(thresholds = c(seq(from = 0.03, to = 0.2, by = 0.01), 0.3, 0.4, 0.5, 0.6, 0.7, 1), nb_levels = 10, OUTPUT_DATA_FOLDER) {
  logger::log_info("Begin the selection step")
  my_selection_folder = file.path(OUTPUT_DATA_FOLDER, "algorithm_selection_results")
  if(!dir.exists(my_selection_folder)) {
    dir.create(my_selection_folder, showWarnings = FALSE, recursive = TRUE)
  }

  my_scoring_folder = file.path(OUTPUT_DATA_FOLDER, "algorithm_scoring_results")
  my_scoring_files = list.files(my_scoring_folder)
  file_idx = 2

  for(file_idx in 1:length(my_scoring_files)) { # one file_idx = one series
    file_out = file.path(my_selection_folder, my_scoring_files[file_idx])
    if(!file.exists(file_out)) {
      my_file = file.path(my_scoring_folder, my_scoring_files[file_idx])
      df = readRDS(my_file) # load scoring
      rows = nrow(df)
      row = 1
      selection_df = list()
      for(row in 1:rows) { # one row = one scoring for this file
        names = strsplit(gsub(".RDS", "", basename(file_out)), "@")[[1]]
        explicit_name = paste0("[", paste(names[1], names[2], sep = " x "), "]")
        logger::log_info(paste0("Selection for dataset ", paste0(file_idx, "/", length(my_scoring_files)), ", ", "cost=", paste0(row, "/", rows), " ", explicit_name))

        # Elements regarding current scoring (common to each possible selection)
        single_row_scoring_df = df[row,]
        if(nrow(single_row_scoring_df) > 1) {
          stop("The input should be a single scoring")
        }

        cost_func_str = single_row_scoring_df$cost_method
        multivariate = ifelse(class(single_row_scoring_df$x[[1]]) == "data.frame", TRUE, FALSE)
        if(multivariate) {
          cost_func_str = paste0(cost_func_str, "_multivariate")
        }

        cost_func = all_cost_functions()[[which(names(all_cost_functions()) == cost_func_str)]]
        scoring = single_row_scoring_df$scoring[[1]]
        cost_initial = single_row_scoring_df$cost_initial
        x = single_row_scoring_df$x[[1]]

        # Perform the selection over the different thresholds
        single_selection_df = pipeline_selection_single_scoring(scoring, thresholds, nb_levels, cost_initial, cost_func, x)

        # Adding all the previous scoring columns: dataset_name, cost_method, ...
        col_scoring = colnames(single_row_scoring_df)
        for(col in 1:length(col_scoring)) {
          single_selection_df[[col_scoring[col]]] = single_row_scoring_df[[col_scoring[col]]]
        }

        # Reorder columns
        single_selection_df = single_selection_df %>% select(!!colnames(single_row_scoring_df), everything())
        selection_df[[row]] = single_selection_df
      }

      selection_df = bind_rows(selection_df)
      saveRDS(selection_df, file_out)
    }
  }
  logger::log_info(paste0("Selections saved in ", file.path(OUTPUT_DATA_FOLDER, "algorithm_selection_results")))
}

#' Compute the selection of a single data set and scoring over all thresholds and levels
#'
#' @param scoring data frame with columns index and score
#' @param thresholds vector of selected thresholds
#' @param nb_levels number of recursive levels
#' @param cost_initial initial cost of the whole data set series
#' @param cost_func current cost function
#' @param x the current univariate or multivariate data set series
#' @return a data frame giving, for each threshold and level, the corresponding
#' *selection* (data frame with the 0/1 result obtained for each possible change
#' point index), along with additional information
pipeline_selection_single_scoring = function(scoring, thresholds, nb_levels, cost_initial, cost_func, x) {

  if(is.null(scoring)) {
    stop("There is an error in the scoring")
  }

  if(any(is.na(scoring$score))) {
    stop("There is a NA score")
  }

  if(any(scoring$score < 0)) {
    stop("There is a negative score")
  }

  if(all(scoring$score == 0)) {
    stop("Degenerate scoring")
  }

  default_score = 0
  changepoint_gain = scoring$score
  output = list()
  threshold = 1
  k = length(thresholds)
  cat("threshold ")
  for(k in 1:length(thresholds)) {
    threshold = thresholds[k]
    cat(paste0(round(threshold,4), " "))
    if(threshold <= max(changepoint_gain)) {
      raw_output = scoth_recursive_partitions(threshold, nb_levels, changepoint_gain, cost_initial, x, cost_func)
      output_cur = convert_partitions_to_data_frame(threshold, raw_output, scoring)
      output[[length(output) + 1]] = output_cur
    } else {
      # create output_cur with only 0 predictions
      selection_threshold_included = rep(threshold, nb_levels)
      selection_level = 1:nb_levels
      index = 1 + (1:length(changepoint_gain))
      predictions = rep(0, length(index))
      selection = data.frame(index = index, predictions = predictions)
      output_cur = list()
      output_cur$selection_threshold_included = selection_threshold_included
      output_cur$selection_level = selection_level
      for(cur_level in 1:nb_levels) {
        output_cur[["selection"]][[cur_level]] = selection
        output_cur[["selection_additional"]][[cur_level]] = list(partitions = list(data.frame(s=1, e=length(changepoint_gain)+2, cost = cost_initial)))
      }
      output[[length(output) + 1]] = output_cur
    }
  }
  cat("\n")
  # End code

  output = pipeline_selection_single_post_process(output, scoring, nb_levels, cost_initial, default_score, x)

  if(!all(colnames(output) == c("selection_threshold_included", "selection_level", "selection", "selection_additional"))) {
    stop("Colnames for the output are incorrect")
  }
  return(output)
}

#' Convert the output obtained from scoth_recursive_partitions to a data.frame with
#' the selection/binarization at each level for the method
#'
#' @param threshold numeric selected threshold to be indicated in `selection_threshold_included` column
#' @param raw_output list with partitions and changepoint_gains at each level obtained
#' from the `scoth_recursive_partitions` function
#' @param scoring initial scoring related to the changepoint_gains, only used for checking
#' @return data.frame with columns `selection_threshold_included`, `selection_level`,
#' `selection`, `selection_additional`, for a single threshold and at all levels, giving the
#' binarization for this threshold
convert_partitions_to_data_frame = function(threshold, raw_output, scoring) {
  nb_levels = length(raw_output$partitions)
  # fill the thresholds (non missing ones)
  # create an output entry for each threshold (for adding them one by one possibly at the end)
  # and for levels from 1 to nb_levels
  output_cur = data.frame(selection_threshold_included = threshold, selection_level = 1:nb_levels) # single row here
  output_cur[["selection"]] = vector(mode = 'list', length = nrow(output_cur))
  output_cur[["selection_additional"]] = vector(mode = 'list', length = nrow(output_cur))

  for(cur_level in 1:nb_levels) {
    # partition data frame, each row being an interval [s, e) between two change points (or the border) with the associated cost
    partition = raw_output$partitions[[cur_level]]
    # compute the selection/binarization given a partition for the method (data.frame of index and predictions (either 0 normal point or 1 change point))
    selection = data.frame(index = 2:(tail(partition$e,1)-1), predictions = 0)
    pred_idx = (partition$s)[-1] # ordered and ok the minimum index is 2
    if(length(pred_idx) > 0) {
      selection$predictions[pred_idx-1] = 1 # shift of 1 because the first idx is for index=2
    }
    # checkings
    if(nrow(scoring) != nrow(selection)) {
      stop("scoring and selection indexes should have the same size")
    }
    if(!all(scoring$index == selection$index)) {
      stop("scoring and selection indexes should be the same")
    }
    output_cur[["selection"]][[cur_level]] = selection
    output_cur[["selection_additional"]][[cur_level]] = lapply(raw_output, function(x) {x[1:cur_level]})
  }
  return(output_cur)
}

#' Helper for post-processing the `output_list`
#'
#' @param output_list list with the possible thresholds at each level up to a certain level
#' @param scoring data frame with columns index and score
#' @param nb_levels number of levels considered
#' @param cost_initial initial cost of the whole series
#' @param default_score minimum possible scoring for the scoring score
#' @param x the current univariate or multivariate data set series
#' @return post-processed `output_list` with the added thresholds `0` (default scores) and `+Inf`,
#' added to `output_list`
pipeline_selection_single_post_process = function(output_list, scoring, nb_levels, cost_initial, default_score, x) {
  # adding threshold = inf and 0 (default_score)
  output_first = multithreshold_default_inf(scoring, nb_levels, cost_initial)
  output_last = multithreshold_default_min(scoring, nb_levels, default_score)
  if(output_list[[length(output_list)]]$selection_threshold_included[1] != default_score) {
    output = bind_rows(c(list(output_first), output_list, list(output_last)))
  } else {
    # already threshold=0 in the output_list
    output = bind_rows(c(list(output_first), output_list))
  }

  # Adding level 0
  output_level_0 = output %>% filter(is.infinite(selection_threshold_included), selection_level == 1)
  if(nrow(output_level_0) > 1) {
    stop("output_level_0")
  }
  output_level_0$selection_level = 0
  output_level_0_list = list()
  for(threshold in unique(output$selection_threshold_included)) { # here should be more than those thresholds, since should be all the thresholds
    output_level_0$selection_threshold_included = threshold
    output_level_0_list[[length(output_level_0_list)+ 1]] = output_level_0
  }
  output_level_0_list = bind_rows(output_level_0_list)

  # Merging level 0 with other levels
  output = rbind(output_level_0_list, output) %>% arrange(selection_threshold_included, selection_level)

  return(output)
}

#' Helper for extreme thresholds functions (`multithreshold_default_inf` and `multithreshold_default_min`)
#'
#' @param single_threshold threshold either default_score or +Inf, corresponding to an extreme threshold
#' @param single_prediction prediction that all the index elements should have (0 for +Inf, or 1 for default_score threshold)
#' @param index index of the scoring of the form 2:n with n the number of row of the related scoring
#' @param levels vector of levels of the form 1:nb_levels
#' @return a pre-filled template of the selection output row for this threshold, with the selection
#' filled and the selection_additional template
default_single_row_output = function(single_threshold, single_prediction, index, levels = NA) {
  nb_levels = length(levels)
  output = data.frame(selection_threshold_included = rep(single_threshold, nb_levels),
                      selection_level = levels)
  output[["selection"]] = vector(mode = 'list', length = nrow(output))
  output[["selection_additional"]] = vector(mode = 'list', length = nrow(output))
  for(i in 1:nb_levels) {
    output[["selection"]][[i]] = data.frame(index = index, predictions = single_prediction)
  }
  return(output)
}

#' Selection output data.frame row for the +Inf threshold in the case
#' of multithreshold selection method
#'
#' @param scoring data frame with columns index and score
#' @param nb_levels number of recursive levels
#' @param cost_initial initial cost of the whole data set series
#' @return selection output rows for the +Inf threshold as a data.frame
multithreshold_default_inf = function(scoring, nb_levels, cost_initial) {
  output = default_single_row_output(+Inf, 0, scoring$index, 1:nb_levels)
  for(i in 1:nb_levels) {
    partition = data.frame(s = 1, e = max(scoring$index)+1, cost = cost_initial)
    output[["selection_additional"]][[i]] = list(partitions = rep(list(partition), i),
                                                 changepoint_gains = rep(list(NA), i),
                                                 threshold_level = 1)
  }
  return(output)
}

#' Selection output data.frame row for the default_score threshold in the case
#' of multithreshold selection method
#'
#' @param scoring data frame with columns index and score
#' @param nb_levels number of recursive levels
#' @param default_score minimum possible scoring for the scoring score
#' @return selection output rows for the default_score threshold as a data.frame
multithreshold_default_min = function(scoring, nb_levels, default_score) {
  output = default_single_row_output(default_score, 1, scoring$index, 1:nb_levels)
  for(i in 1:nb_levels) {
    partition = data.frame(s = 1:max(scoring$index), e = 2:(max(scoring$index)+1), cost = 0)
    output[["selection_additional"]][[i]] = list(partitions = rep(list(partition), i),
                                                 changepoint_gains = rep(list(NA), i),
                                                 threshold_level = 1)
  }
  return(output)
}
