#' Main selection function
#'
#' Iterate the partitioning of the series at different level of granularity, based on
#' a changepoint gain (computed w.r.t. a certain cost).
#'
#' @param threshold within [0,1]
#' @param nb_levels number of levels of granularity to be computed iteratively
#' @param changepoint_gain vector of length the number of possible change points, with the gain
#' w.r.t to the global cost of [s, e). The larger gain the more probable it is a change
#' @param cost_initial cost of the whole series, used for normalizing the gain to
#' a new partition
#' @param x univariate (vector) or multivariate (data.frame) series, on which change points need to be found
#' @param cost_func a cost function
#' @return list with two elements. Each element has the length of the number of
#' levels. The two elements are `partitions` and `changepoint_gains`
#'
#' For partitions, each element is a data frame, each row being an interval [s, e)
#' between two change points (or the border) with the associated cost
#'
#' For change point gains, each element is a vector of length the number of possible change points,
#' with the gain w.r.t to the local cost of each partition [s, e), and where the remaining elements
#' corresponding to previous change points assigned at larger granularity are set to NA
#' @export
scoth_recursive_partitions = function(threshold = 0.1, nb_levels = 4, changepoint_gain, cost_initial, x, cost_func) {
  s = 1
  e = length(changepoint_gain)+2 # == length(x)+1

  # partition for the first level
  partition = get_partition_from_changepoint_gain(changepoint_gain, x, cost_func, s, e, threshold)

  # next levels
  changepoint_gains = list()
  partitions = list()
  changepoint_gains[[1]] = changepoint_gain
  partitions[[1]] = partition

  if(nb_levels > 1) {
    for(k in 1:(nb_levels-1)) {
      changepoint_gains[[k+1]] = iterate_changepoint_gain_from_partition(partitions[[k]], changepoint_gain, cost_initial)
      partitions[[k+1]] = get_partition_from_changepoint_gain(changepoint_gains[[k+1]], x, cost_func, s, e, threshold)
    }
  }

  return(list(partitions = partitions, changepoint_gains = changepoint_gains))
}

#' Update to the lower granularity changepoint_gain without the need to recompute all the elements
#'
#' @param partition data frame, each row being an interval [s, e) between two change points and the
#' related cost. Those are the iterated ones
#' @param changepoint_gain vector of length the number of possible change points, with the gain
#' w.r.t to the *global* cost of the whole series. The larger gain the more probable it is a change
#' Here it is always `changepoint_gain` of the *global* cost, not the next ones
#' @param cost_initial cost of the whole series, used for normalizing the gain to
#' a new partition
#' @return vector of length the number of possible change points, with the gain
#' w.r.t to the local cost of each partition [s, e), and where the remaining elements
#' corresponding to previous change points assigned at larger granularity are set to NA
iterate_changepoint_gain_from_partition = function(partition, changepoint_gain, cost_initial) {
  # this iteration is done without the need to recompute all the df and df2
  e = tail(partition$e, 1)
  changepoint_gain2 = rep(NA, e-2)

  for(i in 1:nrow(partition)) {
    if(partition[i,]$e - partition[i,]$s < 1.5) {
      # e - s < 1.5 <==> e-s=1 <==> [s, s+1) interval i.e. no possible change point on it
      # And interval_i = [s,s-1) is not a proper interval
    } else {
      # Example 1:
      #   s e cost
      #   1 6    0   and  [1,6) = [1,2) [2,3) [3,4) [4,5) [5,6)
      #                         has five possible change points
      #
      # For [1, 3), interval_i is of length 1
      # For [1, 2) it's not possible (no change point) and already managed in the previous condition
      interval_i = partition[i,]$s:(partition[i,]$e-2)
      cost_interval = partition[i,]$cost
      V_segmentation = sum(partition$cost) # total cost of the current segmentation, for the weight zoom
      changepoint_gain_interval = changepoint_gain[interval_i]
      if(cost_interval == 0) { # the considered interval is a constant
        changepoint_gain2[interval_i] = 0
      } else {
        len_local_interval = length(changepoint_gain_interval)+1 # e.g. for x=c(1,2,3,4), there are 3 gains, so the interval has size length(gain)+1
        len_global_interval = e-1
        # with the following parameters (and keep_global_ty==""), we have the formula:
        # changepoint_gain_interval*(cost_initial/V_segmentation) ; which is a zoom, but weaker than the full "" zoom
        changepoint_gain2[interval_i] = changepoint_gain_interval*(cost_initial/V_segmentation)
      }
    }
  }
  return(changepoint_gain2)
}

#' Build partition from selected change points based on a criteria
#'
#' @param changepoint_gain vector of length the number of possible change points, with the gain
#' w.r.t to the global cost of [s, e). The larger gain the more probable it is a change
#' @param x univariate (vector) or multivariate (data.frame) series, on which change points need to be found
#' @param cost_func a cost function
#' @param s start of the interval of interest [s, e)
#' @param e end of the interval of interest [s, e) (w.r.t. absolute index 1)
#' @param threshold within [0,1]
#' @return data frame, each row being an interval [s, e) between two change points (or the border)
#' with the associated cost
get_partition_from_changepoint_gain = function(changepoint_gain, x, cost_func, s, e, threshold) {
  # s and e below are always the ones used for the `scoth_changes` function
  na_changepoint = which(is.na(changepoint_gain))
  if(length(na_changepoint) > 0) {
    changepoint_gain[na_changepoint] = +Inf # keep them in the partition
  }
  idx = find_indexes(changepoint_gain, s, threshold) # b as in WBS2 [s,length(x))
  partition = get_partition_from_idx(idx, x, cost_func, s, e)
  return(partition)
}

#' Select change points
#'
#' @param changepoint_gain vector of length the number of possible change points, with the gain
#' w.r.t to the global cost of [s, e). The larger gain the more probable it is a change
#' @param s start of the interval of interest [s, e)
#' @param threshold within [0,1]
#' @return vector of indexes (w.r.t. absolute index 1, not to s) considered as change points
find_indexes = function(changepoint_gain, s=1, threshold = 0.03) {
  # the possible NA of `changepoint_gain` (happening after the first iteration) are ignored
  idx = which(changepoint_gain >= threshold)
  # Conversion of the idx local to [s,e) to the idx global to [1,length(x)+1]
  idx = idx+s-1 # if s=1, the index is unchanged
  return(idx)
}

#' Convert the selected index change points to a series of ranges indicating each interval
#'
#' @param idx vector of indexes considered as change points (w.r.t. absolute index 1)
#' @param x univariate (vector) or multivariate (data.frame) series, on which change points need to be found
#' @param cost_func a cost function
#' @param s start of the interval of interest [s, e) (w.r.t. absolute index 1)
#' @param e end of the interval of interest [s, e) (w.r.t. absolute index 1)
#' @return data frame, each row being an interval [s, e) between two change points (or the border)
get_partition_from_idx = function(idx=c(), x, cost_func, s=1, e=length(x)+1) {
  # get_partition_from_idx have idx such that idx[1]>=s and idx[last] <= e-2, each element is change when idx=s:(e-2)
  t = c(s, idx+1, e) # idx in [s,e-1) so idx+1 in [s+1,e) so always disjoint from {s,e}
  df = build_df_df2(x, t, cost_func)$df # including useful cost of each interval
  df
}
