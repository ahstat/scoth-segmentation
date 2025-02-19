#' Compute the F1-biased metric and the cover metric following the TCPD benchmark,
#' reproducing the code available at the following address:
#' `https://github.com/alan-turing-institute/TCPDBench/blob/master/analysis/scripts/metrics.py`
#' The corresponding tests are available in the test folder.
#'
#' @param binarization data frame with three columns:
#' - index: the index of the possible change points from 2 to T
#' - predictions: binary vector, with 1 when the index is detected as a change point
#' - groundtruth: binary vector, with 1 when the index is set by the ground truth as a change point
#' @return a list of the F1 biased score, precision, recall, covering score,
#' additional recall parts (for each user), covering parts (for each user), along with
#' the number of ground truth change points and of predicted change points
metrics_TCPD_func = function(binarization) {
  if(is.null(binarization)) {
    return(list(F1= NA, precision = NA, recall = NA, covering = NA))
  }

  predictions = binarization$index[which(binarization$predictions > 0)]
  if(ncol(binarization) < 3) {
    return(list(F1= NA, precision = NA, recall = NA, covering = NA))
    # happen e.g. when no ground truth
    # print(colnames(binarization))
    # stop("binarization colnames should be at least 3")
  }
  annotations = list()
  for(col_idx in 3:ncol(binarization)) {
    col_name = colnames(binarization)[col_idx]
    annotations[[col_name]] = binarization$index[which(binarization[,col_idx] > 0)]
  }

  n_obs = nrow(binarization)+2 # need because of 2 shifts (otherwise covering not ok when prediction is always 1)
  f_measure = metrics_TCPD_f_measure(annotations, predictions, return_PR = TRUE) # using the information for all users
  covering = metrics_TCPD_covering(annotations, predictions, n_obs)

  precision = f_measure$P
  recall = f_measure$R
  f1 = f_measure$F1

  pred_idx = binarization$index[which(binarization$predictions > 0)]
  gt_idx = binarization$index[which(binarization$groundtruth > 0)]
  nb_gt = length(gt_idx)
  nb_pred = length(pred_idx)

  return(list(F1= f1,
              precision = precision,
              recall = recall,
              covering = covering$covering,
              recall_parts = list(f_measure$Ri),
              covering_parts = list(covering$Ci),
              nb_gt = nb_gt,
              nb_pred = nb_pred))
}

# The following functions directly reproduce this code:
# `https://github.com/alan-turing-institute/TCPDBench/blob/master/analysis/scripts/metrics.py`

metrics_TCPD_covering = function(annotations, predictions, n_obs) {
  Ak = sapply(annotations, metrics_TCPD_partition_from_cps, n_obs, simplify = FALSE)
  pX = metrics_TCPD_partition_from_cps(predictions, n_obs)
  Cs = sapply(Ak, function(Ak_k) {metrics_TCPD_cover_single(Ak_k, pX)})
  return(list(covering = sum(Cs)/length(Cs), Ci = Cs))
}

metrics_TCPD_f_measure = function(annotations, predictions, margin = 5, alpha = 0.5, return_PR = FALSE) {
  Tks = sapply(annotations, function(x){sort(unique(c(0, x)))}) # ensure 0 is in all the sets
  X = sort(unique(c(0, predictions)))
  Tstar = c()
  if(length(Tks) == 0) {
    stop("no annotation")
  }
  Tstar = sort(unique(as.numeric(unlist(Tks))))
  K = length(Tks)
  P = length(metrics_TCPD_true_positives(Tstar, X, margin=margin)) / length(X)

  TPk = sapply(Tks, function(x){metrics_TCPD_true_positives(x, X, margin=margin)}, simplify = FALSE)
  Ri = sapply(TPk, length)/sapply(Tks, length)
  R = sum(1 / K * Ri)
  F1 = P * R / (alpha * R + (1 - alpha) * P)
  if(return_PR) {
    return(list(F1=F1,P=P,R=R,Ri=Ri))
  } else {
    return(F1)
  }
}

metrics_TCPD_true_positives = function(gt_idx_with_1 = c(1,10,20,23), pred_idx_with_1 = c(3,8,20), margin = 5) {
  X = pred_idx_with_1
  TP = c()
  tau = gt_idx_with_1[1]
  for(tau in gt_idx_with_1) {
    idx = which(abs(tau - X) <= margin)
    close = data.frame(dists = abs(tau - X)[idx], X = X[idx])
    if(nrow(close) > 0) {
      #dist = close$dists[1]
      xstar = close$X[1]
      TP = c(TP, tau)
      X = X[-which(X == xstar)]
    }
  }
  return(TP)
}

metrics_TCPD_partition_from_cps = function(locations, n_obs) {
  if(0 %in% locations) {
    locations = locations[-which(locations == 0)]
  }
  d = data.frame(start = c(0, locations), end = c(locations, n_obs))
  sapply(1:nrow(d), function(x){d$start[x]:(d$end[x]-1)}, simplify = FALSE)
}

metrics_TCPD_cover_single = function(S, Sprime) {
  if(class(S) != "list") {
    stop("should be a list")
  }
  total = sum(sapply(Sprime, length))
  total2 = sum(sapply(S, length))
  if(total != total2) {
    stop("unequal partitions")
  }
  C = sum(sapply(S, function(R) {length(R) * max(sapply(Sprime, function(Rprime) {metrics_TCPD_overlap(R, Rprime)}))}))

  C = C/total
  return(C)
}

metrics_TCPD_overlap = function(A, B) {
  length(intersect(A,B)) / length(union(A,B))
}
