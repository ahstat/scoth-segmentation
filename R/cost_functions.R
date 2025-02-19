#' All cost functions used in the pipeline
#'
#' @return Vector of all exported functions
#' @export
all_cost_functions = function() {
  cost_funcs = c(c_reg_cusum_L2, c_cusum_L2, c_reg_cusum_L2_multivariate, c_cusum_L2_multivariate)
  names(cost_funcs) = c("reg_cusum_L2", "cusum_L2", "reg_cusum_L2_multivariate", "cusum_L2_multivariate")
  return(cost_funcs)
}

#' Cost function [L2]
#'
#' @param x Univariate vector series, on which change points need to be found
#' @param s Start of the interval of interest [s, e)
#' @param e End of the interval of interest [s, e)
#' @return L2 cost function
c_cusum_L2 = function(x = rnorm(100), s = 1, e = length(x)+1) {
  x_cur = x[s:(e-1)]
  cusum = sum((x_cur - mean(x_cur))^2)
  return(cusum)
}

#' Cost function [regression L2]
#'
#' @param x Univariate vector series, on which change points need to be found
#' @param s Start of the interval of interest [s, e)
#' @param e End of the interval of interest [s, e)
#' @return L2 regression cost function
c_reg_cusum_L2 = function(x, s = 1, e = length(x)+1) {
  idx = s:(e-1)
  x_cur=x[idx]
  reg = lm(x_cur~idx)
  sum((predict(reg) - x_cur)^2)
}

#' Dummy cost function based on the area of the rectangle encompassing all the data points
#'
#' @param x Univariate vector series, on which change points need to be found
#' @param s Start of the interval of interest [s, e)
#' @param e End of the interval of interest [s, e)
#' @return Box cost function
c_box = function(x, s = 1, e = length(x)+1) {
  diff_time = e-s
  x_cur = x[s:(e-1)]
  # (range in values) * (range in time)
  diff(range(x_cur)) * diff_time
}

#' Cost function [L2] for the multivariate case
#'
#' @param x Multivariate data frame series, on which change points need to be found
#' @param s Start of the interval of interest [s, e)
#' @param e End of the interval of interest [s, e)
#' @return L2 cost function
c_cusum_L2_multivariate = function(x = data.frame(x1 = rnorm(100), x2 = rnorm(100)), s = 1, e = nrow(x)+1, normalization = FALSE) {
  x_cur = x[s:(e-1),]
  if(nrow(x_cur) == 1) {
    return(0)
  }
  if(normalization) {
    x_cur = normalize_multivariate_series(x_cur)
  }
  x_delta_per_feat = sapply(1:ncol(x_cur), function(feat){x_cur[,feat] - mean(x_cur[,feat])})
  cusum = sum(x_delta_per_feat^2)
  cusum
}

#' Cost function [regression L2] for the multivariate case
#'
#' @param x Multivariate data frame series, on which change points need to be found
#' @param s Start of the interval of interest [s, e)
#' @param e End of the interval of interest [s, e)
#' @return L2 regression cost function
c_reg_cusum_L2_multivariate = function(x = data.frame(x1 = rnorm(100), x2 = rnorm(100)), s = 1, e = nrow(x)+1, normalization = FALSE) {
  idx = s:(e-1)
  x_cur = x[idx,]
  if(nrow(x_cur) == 1) {
    return(0)
  }
  if(normalization) {
    x_cur = normalize_multivariate_series(x_cur)
  }
  reg = lm(as.matrix(x_cur)~idx)
  sum((predict(reg) - x_cur)^2)
}

#' Normalization of the multivariate series
#'
#' @param x_cur Multivariate data frame series, that needs to be normalized among features
#' @return the normalized data frame
normalize_multivariate_series = function(x_cur) {
  if(nrow(x_cur) == 1) {
    stop("The series should have values for at least two time stamps")
  }
  sd_per_feat = sapply(1:ncol(x_cur), function(feat){sd(x_cur[,feat])})
  idx_0_sd = which(sd_per_feat < 1e-16)
  if(length(idx_0_sd) > 0) {
    # the shift in mean already gives a vector of 0 so that sum component will be 0
    sd_per_feat[idx_0_sd] = 1
  }
  x_cur = sapply(1:ncol(x_cur), function(feat){(x_cur[,feat] - mean(x_cur[,feat]))/sd_per_feat[feat]})
  return(x_cur)
}
