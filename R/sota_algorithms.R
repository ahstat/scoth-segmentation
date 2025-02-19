#' ADWIN algorithm
#'
#' @param x univariate series
#' @param parameters list of parameters for this algorithm
#' @return data frame with the binarized prediction for each possible change point index
adwin_func = function(x, parameters = list(delta=0.002, clock=32, max_buckets=5, min_window_length=5, grace_period=10)) {
  # this function needs python with river 0.21.1
  # river 0.10 is not sufficient since some parameters are missing in ADWIN
  # river 0.21.1 needs a recent python 3.9+
  # reticulate package needs to be recent for working with python 3.9+
  # PYTHON_FOLDER should be a defined global variable in the form "/dir/.conda/envs/envname/bin/python"
  reticulate::use_python(PYTHON_FOLDER)
  river = reticulate::import("river")
  adwin = river$drift$ADWIN(parameters$delta, parameters$clock, parameters$max_buckets, parameters$min_window_length, parameters$grace_period)
  res = rep(0, length(x)-1) # from i=2 to i=length(x)
  for(i in 1:length(x)) {
    adwin$update(x[i])
    # i=1 cannot be a change, since it's the first element
    # then, i=2 corresponds to the first possible change point index, hence the i-1
    if(adwin$drift_detected) {
      res[i-1] = 1
    }
  }
  return(data.frame(index=2:length(x), predictions = res))
}

#' breakfast algorithms
#'
#' @param x univariate series
#' @param parameters list of parameters for this algorithm
#' @return data frame with the binarized prediction for each possible change point index
breakfast_func = function(x, parameters = list(scoring = "wbs", M = 1000, selection = "sdll")) {
  x = as.matrix(data.frame(x = as.numeric(x)))

  # scoring ----
  scoring = parameters$scoring
  M = parameters$M # only used for: wbs, not, wbs2
  if(scoring == "wbs") {
    sol = breakfast::sol.wbs(x, M, systematic.intervals = TRUE, seed = 1)
  } else if(scoring == "not") {
    sol = breakfast::sol.not(x, M, systematic.intervals = TRUE, seed = 1)
  } else if(scoring == "idetect_seq") {
    sol = breakfast::sol.idetect_seq(x, points = 3)
  } else if(scoring == "tguh") {
    sol = breakfast::sol.tguh(x, p = 0.01)
  } else if(scoring == "wbs2") {
    sol = breakfast::sol.wbs2(x, M, systematic.intervals = TRUE)
  } else {
    stop("Unknown scoring method")
  }

  # selection ----
  selection = parameters$selection

  if(selection == "thresh") {
    pred_x = breakfast::model.thresh(sol, th_const = 1.15) # default th_const selected
  } else if(selection == "ic") {
    pred_x = breakfast::model.ic(sol, q.max = 30) # q.max is the max number of change points
  } else if(selection == "sdll") {
    pred_x = breakfast::model.sdll(sol, lambda = 0.9)
  } else if(selection == "lp") {
    min.d = pmin(5, floor((length(x)-3)/2)) # default to 5, but need to put to 3 for extrashort breakfast case with length(x)=10
    pred_x = breakfast::model.lp(sol, min.d=min.d)
  } else {
    stop("Unknown selection method")
  }

  # Extract the data frame
  res = rep(0, dim(x)[1]-1)
  res[pred_x$cpts] = 1
  return(data.frame(index = 2:length(x), predictions = res))
}

#' changeforest algorithm
#'
#' @param df data frame with one or multiple columns
#' @param parameters list of parameters for this algorithm
#' @return data frame with the binarized prediction for each possible change point index
changeforest_func = function(df, parameters = list(method = "random_forest", segmentation_type = "bs")) {
  # Source of the python package: https://github.com/mlondschien/changeforest
  # Installed via `pip install changeforest`
  # PYTHON_FOLDER should be a defined global variable in the form "/dir/.conda/envs/envname/bin/python"
  reticulate::use_python(PYTHON_FOLDER)
  np = reticulate::import("numpy")
  changeforest = reticulate::import("changeforest")

  method = parameters$method
  segmentation_type = parameters$segmentation_type

  for(i in 1:ncol(df)) {
    df[,i] = as.numeric(df[,i])
  }

  X = as.matrix(df) # work for dimension >= 1
  # default control parameters are used (number of trees, permutation cuts for some methods)
  if(!(method %in% c("knn", "change_in_mean", "random_forest"))) {
    stop("Unknown method")
  }

  if(!(segmentation_type %in% c("bs", "sbs", "wbs"))) {
    stop("Unknown segmentation type")
  }

  result = changeforest$changeforest(X, method, segmentation_type)

  cpt_output = result$split_points()

  res = rep(0, nrow(df)-1)
  if(length(cpt_output) > 0) {
    res[cpt_output] = 1
  }

  return(data.frame(index = 2:nrow(df), predictions = res))
}

#' Non parametric change point algorithm
#'
#' @param x univariate series
#' @param parameters list of parameters for this algorithm
#' @return data frame with the binarized prediction for each possible change point index
cpnp_func = function(x, parameters = list(pen = "SIC", Q = 10)) {
  pen = parameters$pen
  Q = parameters$Q

  # Cost function ----
  f = changepoint.np::cpt.np # cost = "ecdf" in this case

  # Search function ----
  search = "PELT" # `search` must be "PELT"

  # Number of quantiles ----
  nquantiles = Q

  # Penalty ----
  penalty = ifelse(grepl("pen", pen), "Manual", pen)
  pen.value = get_pen.value(pen)

  # Shape the input vector ----
  # PELT is taking the union of set-index over each univariate vector, which
  # is the same strategy as in the `multivariate_func` function
  x = as.matrix(data.frame(x = as.numeric(x)))

  # Get the results ----
  changes = f(t(x), penalty = penalty, pen.value = pen.value, method = search, nquantiles = nquantiles)

  # Extract the data frame
  pred_x = lapply(changes, function(x){x@cpts})
  pred_x = sort(unique(unlist(pred_x)))
  pred_x = pred_x[-length(pred_x)]
  res = rep(0, dim(x)[1]-1)
  res[pred_x] = 1
  return(data.frame(index = 2:length(x), predictions = res))
}

#' ECP change point algorithms
#'
#' @param df data frame with one or multiple columns
#' @param parameters list of parameters for this algorithm
#' @return data frame with the binarized prediction for each possible change point index
ecp_func = function(df, parameters = list(method = "e_agglo", params = "alpha1")) {
  method = parameters$method
  params = parameters$params

  params_vector = strsplit(params, "_", fixed = TRUE)[[1]]
  X = as.matrix(df) # multivariate input

  # computations ----
  if(method == "eagglo") {
    alpha = params_vector[1]
    alpha = as.numeric(gsub("alpha", "", alpha, fixed = TRUE))
    pred_x = ecp::e.agglo(X, alpha=alpha)$estimates
  } else if(method == "edivisive") {
    alpha = params_vector[1]
    alpha = as.numeric(gsub("alpha", "", alpha, fixed = TRUE))
    signif = params_vector[2]
    signif = as.numeric(gsub("signif", "", signif, fixed = TRUE))
    permutruns = 199 # default
    minsize = 2 # smallest
    pred_x = ecp::e.divisive(X, sig.lvl = signif, R = permutruns, min.size = minsize, alpha=alpha)$estimates
  } else if(method == "kcpa") {
    pen = params_vector[1]
    pen = as.numeric(gsub("pen", "", pen, fixed = TRUE))
    max_nb = 1000 # Q as in PELT
    pred_x =  ecp::kcpa(X, L=max_nb, C=pen)
  } else {
    stop("unknown method for ecp_func")
  }

  # post process ----
  # first and last point should be removed (if present)
  if(1 %in% pred_x) {
    pred_x = pred_x[-which(pred_x == 1)]
  }
  if((nrow(df)+1) %in% pred_x) {
    pred_x = pred_x[-which(pred_x == (nrow(df)+1))]
  }
  pred_x = sort(unique(unlist(pred_x)))
  res = rep(0, nrow(df)-1)
  res[pred_x-1] = 1
  return(data.frame(index = 2:nrow(df), predictions = res))
}

#' PELT and related change point algorithms
#'
#' @param x univariate series
#' @param parameters list of parameters for this algorithm
#' @return data frame with the binarized prediction for each possible change point index
killick_func = function(x, parameters = list(cost = "meanvar", search = "AMOC", pen = "SIC")) {
  cost = parameters$cost
  search = parameters$search
  pen = parameters$pen

  # Cost function ----
  if(cost == "meanvar") {
    f = changepoint::cpt.meanvar
  } else if(cost == "var") {
    f = changepoint::cpt.var
  } else if(cost == "mean") {
    f = changepoint::cpt.mean
  } else {
    stop("'cost' function must be either meanvar, var, or mean")
  }

  # Search policy ----
  if(!(search %in% c("AMOC", "PELT", "SegNeigh", "BinSeg"))) {
    stop("'search' must be 'AMOC', 'PELT', 'SegNeigh', or 'BinSeg'")
  }

  # Maximum number of changes detected ----
  Q = ifelse(search == "SegNeigh", 30, 1000) # change to 30 for SegNeigh, since it is slower
  Q = min(Q, floor(length(x)/2))

  # Penalty ----
  penalty = ifelse(grepl("pen", pen), "Manual", pen)
  pen.value = get_pen.value(pen)

  # Shape the input vector ----
  # PELT is taking the union of set-index over each univariate vector, which
  # is the same strategy as in the `multivariate_func` function
  x = as.matrix(data.frame(x = as.numeric(x)))

  # Get the results ----
  changes = f(t(x), penalty = penalty, pen.value = pen.value, method = search, Q = Q)

  # Extract the data frame
  pred_x = lapply(changes, function(x){x@cpts})
  pred_x = sort(unique(unlist(pred_x)))
  pred_x = pred_x[-length(pred_x)]
  res = rep(0, dim(x)[1]-1)
  res[pred_x] = 1
  return(data.frame(index = 2:length(x), predictions = res))
}

#' Lasso-based change point algorithms
#'
#' @param x univariate series
#' @param parameters list of parameters for this algorithm
#' @return data frame with the binarized prediction for each possible change point index
lasso_func = function(x, parameters = list(lambda = 1, gamma = 0, eps = 1e-3, trend = TRUE)) {
  lambda = parameters$lambda
  gamma = parameters$gamma
  eps = parameters$eps
  trend = parameters$trend
  if(!trend) {
    out = genlasso::fusedlasso1d(x)
    beta = genlasso::softthresh(out, lambda=lambda, gamma=gamma)
  } else {
    # cannot use gamma != 0 in this case
    out = genlasso::trendfilter(x, ord=1)
    beta = genlasso::coef.genlasso(out, lambda=max(lambda, min(out$lambda)))$beta
  }
  if(ncol(beta) > 1) {
    stop("ncol beta should be 1")
  }
  output = as.numeric(beta)
  if(!trend) {
    detected_changepoints = which(abs(diff(output)) > eps)
  } else {
    detected_changepoints = which(abs(diff(diff(output))) > eps)
  }
  res = data.frame(index = 2:length(x), predictions = 0)
  res$predictions[detected_changepoints] = 1
  return(res)
}

#' OCP change point algorithm
#'
#' @param df data frame with one or multiple columns
#' @param parameters list of parameters for this algorithm
#' @return data frame with the binarized prediction for each possible change point index
ocp_func = function(df, parameters = list(l=10, a=0.01, b=0.01, k=0.01)) {
  X = as.matrix(df) # multivariate input
  X = scale(X) # scaling the input

  lambda = parameters$l
  hazard_func = function(x, l) {
    ocp::const_hazard(x, l=lambda)
  }

  init_params = list(list(m = 0, # since the data is scaled
                          k = parameters$k,
                          a = parameters$a,
                          b = parameters$b))

  fit = ocp::onlineCPD(X,
                       oCPD=NULL,
                       missPts = "none", # instead of "prev"
                       hazard_func=hazard_func,
                       probModel=list("gaussian"), # either gaussian or poisson
                       init_params=init_params,
                       multivariate=ifelse(ncol(X) > 1, TRUE, FALSE),
                       cpthreshold = 0.5, # unused by us
                       truncRlim = 10^(-4), # in the paper 1e-4
                       minRlength = 1,
                       maxRlength = 10^4, # bigger than any of our datasets
                       minsep = 1,
                       maxsep = 10^4) # bigger than any of our datasets

  pred_x = as.vector(fit$changepoint_lists$maxCPs[[1]])

  # it seems that 1 is always considered as a change point, so remove it if existing
  # (first element should not be considered as a change point in our case)

  # a convention in CPD field is to set first point as a change point, but
  # we do not set this in this work

  if(1 %in% pred_x) {
    pred_x = pred_x[-which(pred_x == 1)]
  }

  pred_x = sort(unique(unlist(pred_x)))
  res = rep(0, nrow(df)-1)
  res[pred_x-1] = 1
  return(data.frame(index = 2:nrow(df), predictions = res))
}

#' RFPOP change point algorithm
#'
#' @param x univariate series
#' @param parameters list of parameters for this algorithm
#' @return data frame with the binarized prediction for each possible change point index
rfpop_func = function(x, parameters = list(cost = "Outlier", pen = "pen0.3")) {
  loss = parameters$cost
  pen = parameters$pen

  if(grepl("pen", pen)) {
    pen = as.numeric(gsub("pen", "", pen, fixed = TRUE))
  } else {
    stop("Penalty should be on the form penNUMBER")
  }

  normalized = TRUE
  x.data = x
  if(normalized) {
    est.sd <- mad(diff(x.data)/sqrt(2))
    if(is.na(est.sd)) {
      est.sd = 1
    }
    if(est.sd == 0) {
      est.sd = 1
    }
    x.data = x.data/est.sd
  }

  if(loss == "Outlier") {
    res <- robseg::Rob_seg.std(x = x.data,
                               loss = "Outlier",
                               lambda = pen*log(length(x.data)),
                               lthreshold=3) # fixed cf doc
  } else if(loss == "Huber") {
    res <- robseg::Rob_seg.std(x = x.data,
                               loss = "Huber",
                               lambda = pen*log(length(x.data)),
                               lthreshold = 1.345) # fixed cf doc
  } else if(loss == "L1") {
    res <- robseg::Rob_seg.std(x = x.data,
                               loss = "L1",
                               lambda = pen*log(length(x.data)))
  } else if(loss == "L2") {
    res <- robseg::Rob_seg.std(x = x.data,
                               loss = "L2",
                               lambda = pen*log(length(x.data)))
  } else {
    stop("Unknown loss")
  }

  cpt <- res$t.est[-length(res$t.est)]
  pred_x = sort(unique(unlist(cpt)))
  out = rep(0, length(x)-1)
  out[pred_x] = 1
  res = data.frame(index = 2:length(x), predictions = out)
  return(res)
}

#' Zero change point algorithm (predict 0 change points)
#'
#' @param x univariate series
#' @param parameters list of parameters for this algorithm
#' @return data frame with the binarized prediction for each possible change point index
zero_func = function(x, parameters = list()) {
  res = data.frame(index = 2:length(x))
  res$predictions = 0
  return(res)
}

#' Helper function for `killick_func` and `cpnp_func` functions
#' Retrieve the penalty value given the input
#'
#' @param pen string with the form of a penalty value (e.g., "pen0.1"), or of
#' a penalty method (e.g., "BIC")
#' @return extract the penalty scheme (e.g., "0.1*log(n)") if the input is a
#' penalty value, or put 0 if the input is a penalty method
get_pen.value = function(pen) {
  if(grepl("pen", pen)) { # manual penalty
    pen_numeric = as.numeric(gsub("pen", "", pen, fixed = TRUE))
    pen.value = paste0(pen_numeric, " * log(n)")
  } else { # automatic penalty
    pen.value = 0 # in this case, the parameter is not used, since an automatic penalty scheme has been selected
  }
  return(pen.value)
}
