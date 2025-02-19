#' Compute the F1 score metric
#'
#' @param binarization data frame with three columns:
#' - index: the index of the possible change points from 2 to T
#' - predictions: binary vector, with 1 when the index is detected as a change point
#' - groundtruth: binary vector, with 1 when the index is set by the ground truth as a change point
#' @return a list of the F1 score, precision, recall, TP (true positives), along with
#' the number of ground truth change points and of predicted change points
metrics_F1_func = function(binarization = data.frame(index = 2:101, predictions = rep(0, 100), groundtruth = rep(0, 100))) {
  if(is.null(binarization)) {
    return(list(F1= NA, precision = NA, recall = NA, TP = NA, nb_gt = NA, nb_pred = NA))
  }
  pred_idx = binarization$index[which(binarization$predictions > 0)]
  gt_idx = binarization$index[which(binarization$groundtruth > 0)]
  TP = metrics_F1_TP_func(pred_idx, gt_idx, W = 5) # option is always set to 2
  nb_gt = length(gt_idx)
  nb_pred = length(pred_idx)
  precision = TP/nb_pred # true precision without repetition
  recall = TP/nb_gt # true precision without repetition
  f1 = f1_func(precision, recall)
  return(list(F1 = f1,
              precision = precision,
              recall = recall,
              TP = TP,
              nb_gt = nb_gt,
              nb_pred = nb_pred))
}

#' Extract the number of true positives (TP) given the predicted and ground truth
#' indexes, for a certain window. We follow here the recommendations from the
#' following paper `https://ceur-ws.org/Vol-1226/paper31.pdf`
#'
#' @param pred_idx vector of predicted change point indexes
#' @param gt_idx vector of ground truth change point indexes
#' @param W window range for considering the detected change point as valid, w.r.t. the ground truth indexes
#' @return the number of true positives
metrics_F1_TP_func = function(pred_idx= c(1000, 1001, 1015), gt_idx= c(1002, 1018, 1030), W = 5) {
  if(length(gt_idx) == 0) {
    return(0)
  }

  TP = rep(NA, length(gt_idx))
  for(k in 1:length(gt_idx)) {
    x = gt_idx[k]
    TP[k] = any(pred_idx %in% (x-W):(x+W))
    if(TP[k] == TRUE) {
      pred_idx = pred_idx[-which(pred_idx %in% (x-W):(x+W))[1]]
    }
  }
  TP = sum(TP)
  return(TP)
}

#' Compute the F1-score given precision and recall, handling all the corner cases
#'
#' @param precision numeric value
#' @param recall numeric value
#' @param W window range for considering the detected change point as valid, w.r.t. the ground truth indexes
#' @return the F1-score
f1_func = function(precision, recall) {
  out = 2*(precision*recall)/(precision + recall)

  idx_null_f1 = which(precision == 0 & recall == 0) # bad both precision and recall
  # perf_all2[idx_null_f1,]
  # out[idx_null_f1]
  if(length(idx_null_f1) > 0) {
    out[idx_null_f1] = 0
  }

  idx_nan_recall_f1 = which(precision == 0 & is.nan(recall)) # no ground truth but one prediction at least
  # perf_all2[idx_nan_recall_f1,]
  # out[idx_nan_recall_f1]
  if(length(idx_nan_recall_f1) > 0) {
    out[idx_nan_recall_f1] = 0
  }

  idx_nan_precision_f1 = which(is.nan(precision) & recall == 0) # at least one ground truth / but no prediction
  # perf_all2[idx_nan_precision_f1,]
  # out[idx_nan_precision_f1]
  if(length(idx_nan_precision_f1) > 0) {
    out[idx_nan_precision_f1] = 0
  }

  idx_nan = which(is.nan(precision) & is.nan(recall)) # both 0 gt and pred
  # perf_all2[idx_nan,]
  # out[idx_nan]
  if(length(idx_nan) > 0) {
    out[idx_nan] = 1
  }

  if(length(which(is.na(out)))>0) {
    stop("no NA should be remaining")
  }
  return(out)
}
