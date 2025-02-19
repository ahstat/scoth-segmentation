#' List of selected state-of-the-art change point algorithms
#'
#' @return a string vector listing the different selected sota algorithms
#' @export
all_sota_algorithms = function() {
  return(c("adwin", "breakfast", "changeforest", "cpnp", "ecp",
           "killick", "lasso", "ocp", "rfpop", "zero"))
}

#' Internal function for mapping a change point algorithm to its related function
#'
#' @param algorithm_name string within `all_sota_algorithms()`
#' @return the related function to apply the get the binarized change point results
algorithm_name_to_function = function(algorithm_name) {
  if(algorithm_name == "adwin") {
    return(multivariate_func(adwin_func))
  } else if(algorithm_name == "breakfast") {
    return(multivariate_func(breakfast_func))
  } else if(algorithm_name == "changeforest") {
    return(changeforest_func)
  } else if(algorithm_name == "cpnp") {
    return(multivariate_func(cpnp_func))
  } else if(algorithm_name == "ecp") {
    return(ecp_func)
  } else if(algorithm_name == "killick") {
    return(multivariate_func(killick_func))
  } else if(algorithm_name == "lasso") {
    return(multivariate_func(lasso_func))
  } else if(algorithm_name == "ocp") {
    return(ocp_func)
  } else if(algorithm_name == "rfpop") {
    return(multivariate_func(rfpop_func))
  } else if(algorithm_name == "zero") {
    return(multivariate_func(zero_func))
  } else {
    stop("unknown algorithm_name in `algorithm_name_to_function`")
  }
}

#' Internal function for mapping a change point algorithm to its grid of parameters
#'
#' @param algorithm_name string within `all_sota_algorithms()`
#' @return the selected grid for this algorithm
algorithm_name_to_parameters = function(algorithm_name) {
  default_grid = c(0.0001, 0.0003, 0.001, 0.003, 0.01, 0.03, 0.1, 0.3, 1, 3, 10, 30, 100, 300, 1000)
  if(algorithm_name == "adwin") {
    delta_grid = default_grid[default_grid <= 1] # the parameter delta should be <= 1
    clock_grid = c(1,32)
    df_parameters = expand_grid_df(delta=delta_grid, clock=clock_grid, max_buckets=5, min_window_length=5, grace_period=10)
  } else if(algorithm_name == "breakfast") {
    scoring_grid_with_M = c("wbs", "not", "wbs2")
    scoring_grid_without_M = c("idetect_seq", "tguh")
    M_grid = c(10, 100, 1000)
    selection_grid = c("thresh", "ic", "sdll", "lp")
    df_parameters = rbind(
      expand_grid_df(scoring=scoring_grid_with_M, M=M_grid, selection=selection_grid),
      expand_grid_df(scoring=scoring_grid_without_M, M=NA, selection=selection_grid)
    )
  } else if(algorithm_name == "changeforest") {
    method_grid = c("knn", "change_in_mean", "random_forest")
    segmentation_type_grid = c("bs", "sbs", "wbs")
    df_parameters = expand_grid_df(method=method_grid, segmentation_type=segmentation_type_grid)
  } else if(algorithm_name == "cpnp") {
    pen_grid_auto = c("SIC", "BIC", "MBIC", "AIC", "Hannan-Quinn")
    pen_grid_numerical = paste0("pen", default_grid)
    pen_grid = c(pen_grid_auto, pen_grid_numerical)
    nquantiles_grid = c(10, 20, 30, 40) # parameters as in van2020evaluation
    df_parameters = expand_grid_df(pen=pen_grid, Q=nquantiles_grid)
  } else if(algorithm_name == "ecp") {
    # parameters took from van2020evaluation
    params_grid_eagglo = paste0("alpha", c(0.5, 1, 1.5))
    params_grid_edivisive = c(paste0("alpha", c(0.5, 1, 1.5), "_signif0.01"),
                              paste0("alpha", c(0.5, 1, 1.5), "_signif0.05"))
    params_grid_kcpa = paste0("pen", default_grid)
    df_parameters = rbind(
      expand_grid_df(method = "eagglo", params = params_grid_eagglo),
      expand_grid_df(method = "edivisive", params = params_grid_edivisive),
      expand_grid_df(method = "kcpa", params = params_grid_kcpa)
    )
  } else if(algorithm_name == "killick") {
    cost_grid = c("meanvar", "var", "mean")
    search_grid = c("AMOC", "PELT", "SegNeigh", "BinSeg")
    pen_grid_auto = c("SIC", "BIC", "MBIC", "AIC", "Hannan-Quinn")
    pen_grid_numerical = paste0("pen", default_grid)
    pen_grid = c(pen_grid_auto, pen_grid_numerical)
    df_parameters = expand_grid_df(cost=cost_grid, search=search_grid, pen=pen_grid)
    # MBIC penalty not implemented for SegNeigh method
    df_parameters = df_parameters %>% filter(search != "SegNeigh" | pen != "MBIC")
  } else if(algorithm_name == "lasso") {
    lambda_grid = default_grid
    gamma_grid = c(0, 0.001, 0.01, 0.1, 1)
    df_parameters = rbind(
      expand_grid_df(lambda=lambda_grid, gamma=gamma_grid, eps=1e-3, trend=FALSE),
      expand_grid_df(lambda=lambda_grid, gamma=0, eps=1e-3, trend=TRUE)
    )
  } else if(algorithm_name == "ocp") {
    # full grid has been taken since it is the best according to van2020evaluation
    l_grid = c(10, 50, 100, 200)
    a_grid = c(0.01, 0.1, 1, 10, 100)
    b_grid = c(0.01, 0.1, 1, 10, 100)
    k_grid = c(0.01, 0.1, 1, 10, 100)
    df_parameters = expand_grid_df(l=l_grid, a=a_grid, b=b_grid, k=k_grid)
    # Some OCP parameters raise error with pred probs, those are removed
    df_parameters = df_parameters %>% filter(!(a == 100 & b == 0.01)) # error for all values of l and k
  } else if(algorithm_name == "rfpop") {
    cost_grid = c("Outlier", "Huber", "L1", "L2")
    pen_grid = paste0("pen", default_grid)
    df_parameters = expand_grid_df(cost=cost_grid, pen=pen_grid)
  } else if(algorithm_name == "zero") {
    df_parameters = expand_grid_df(param="default")
  } else {
    stop("unknown algorithm_name in `algorithm_name_to_parameters`")
  }
  return(df_parameters)
}
