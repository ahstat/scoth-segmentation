#' Main scoring function: cost for each possible change point along with the initial cost of the series
#'
#' @param x univariate (vector) or multivariate (data.frame) series, on which change points need to be found
#' @param cost_func a cost function name
#' @param verbose whether to print advance of the algorithm
#' @return a list with the initial cost (a numeric) and the "score" for each possible
#' change point (first element = first possible change point i.e. {1} alone and {2...} after)
#' @export
scoth_scoring_func = function(x, cost_func, verbose = TRUE) {
  multivariate = ifelse(class(x) == "data.frame", TRUE, FALSE)

  if(class(cost_func) == "character") {
    if(multivariate) {
      cost_func = paste0(cost_func, "_multivariate")
    }
    if(cost_func %in% names(all_cost_functions())) {
      cost_func = all_cost_functions()[[which(names(all_cost_functions()) == cost_func)]]
    } else {
      stop("cost_func is a string but not known by all_cost_functions()")
    }
  }

  if(multivariate) {
    e = nrow(x)+1
  } else {
    e = length(x)+1
  }

  cost_initial = cost_func(x, s=1, e=e) # partition at level 0
  changepoint_gain = scoth_changes(x, cost_func, s=1, e=e, verbose = verbose) # level 1
  return(list(cost_initial = cost_initial, changepoint_gain = changepoint_gain))
}

#' Compute the cost of each interval (df) and of consecutive pairs of interval (df2)
#'
#' @param x univariate or multivariate vector series, on which change points need to be found
#' @param t a subset of 1:(length(x)+1) in the increasing order, typically
#' an interval s:e or 1:(length(x)+1), but should be numeric
#' @param cost_func Selected cost function
#' @return a list of two elements, each being a data frame.
#' The first two columns of each data frame is an interval [s, e) (s included and e excluded)
#' The third column is the cost of this interval
build_df_df2 = function(x, t, cost_func) {
  t = as.numeric(t)
  N = length(t)
  df = data.frame(s = t[-N], e = t[-1])
  df2 = data.frame(s = t[-c(N-1,N)], e = t[-c(1,2)])
  df$cost = apply(df, 1, function(r) {cost_func(x, r[[1]], r[[2]])})
  df2$cost = apply(df2, 1, function(r) {cost_func(x, r[[1]], r[[2]])})
  return(list(df=df, df2=df2))
}

#' Compute the difference of cost after removing one of the remaining change points,
#' for each possible change point that can be removed. All the costs are global
#' to the whole interval of interest c(l$df$s[1], l$df$e[nrow(l$df)])
#'
#' @param l list of (df,df2) data frame
#' @return difference of cost after removing each change point. This is the
#' difference of global costs.
#'
#' For a specific index, if the difference is large and positive,
#' the cost is increasing by removing this specific change point,
#' so this change point is important and should not be removed.
#'
#' For a specific index, if the difference is close to zero or negative
#' the difference of cost is small by removing this specific change point,
#' so this change point is not important and can be removed.
compute_diff_costs = function(l) {
  k = nrow(l$df)-1 # number of existing change points
  l$df2$cost - l$df$cost[1:k] - l$df$cost[2:(k+1)]
}

#' Remove the `i`th change point and update the costs
#'
#' @param l list of (df,df2) data frame
#' @param i change point `i`th the be removed, i.e. the end time of the row
#' `i` of df, which is also the start time of the row `i+1` of df. `i` should
#' be in 1:k with k=nrow(l$df)-1 the number of change points in `l`
#' @param x univariate or multivariate vector series, on which change points need to be found
#' @param cost_func a cost function
#' @return updated `l` list of (df,df2) data frame
update_costs = function(l, i, x, cost_func) {
  k = nrow(l$df2) # number of potential change points
  # nrow(l$df) == k+1
  if(i < 1) {
    stop("i should be in {1, ..., k} with k the number of change points of l")
  }
  if(i > k) {
    stop("i should be in {1, ..., k} with k the number of change points of l")
  }

  t = c(l$df$s, tail(l$df$e,1))
  # t[1] = start
  # t[2] = potential change point 1
  # t[i+1] = potential change point i
  # Removed change is t[i+1]

  # Algorithm to update df ----
  #   1. Remove row i+1 of df (always possible cf i < n);
  #   2. Replace row i of df by row i of df2 (always possible because i <= n-1)
  l$df = l$df[-(i+1),]
  l$df[i, ] = l$df2[i, ]

  # Algorithm to update df2 ----
  #   1. Remove row i of df2;
  #   2. Replace row i-1 with (i-1,i,i+1)                                         {if possible}
  #   3. Replace row i with (i,i+1,i+2) [which was at position i+1 before step 1] {if possible}
  l$df2 = l$df2[-i, ] # (*)
  if(i > 1) { # if not the first potential change point
    l$df2[i-1,] = c(t[i-1], t[i+2], cost_func(x, t[i-1], t[i+2]))
  }
  if(i < k) { # if not the last potential change point
    l$df2[i,] = c(t[i], t[i+3], cost_func(x, t[i], t[i+3])) # was position i+1 before (*)
  }

  return(l)
}

#' Remove the `i`th change point and update the costs
#' Policy: Taking the maximum gain seen up to now, and removing also from the max vector
#'
#' @param x univariate or multivariate vector series, on which change points need to be found
#' @param cost_func a cost function
#' @param s start of the interval of interest [s, e)
#' @param e end of the interval of interest [s, e)
#' @param verbose whether to print advance of the algorithm
#' @return vector of length the number of possible change points, with the gain
#' w.r.t to the global cost of [s, e).
scoth_changes = function(x, cost_func = c_cusum_L2, s=1, e=length(x)+1, verbose = TRUE) {
  # get the cost of the whole series
  cost_initial = cost_func(x, s, e)
  # if the series has a cost of zero, it's not possible to improve it,
  # so the changepoint_gain of each individual point is 0
  if(cost_initial == 0) {
    changepoint_gain = rep(0, e-s-1)
    return(changepoint_gain)
  }

  ## Get the potential change points on the s:e range ----
  # k is the number of possible change points on the interval
  k = e-s-1
  # Example: With the input c(1,1,10,1), we have s=1 and e=4+1=5
  # And the intervals are      [1,2) [2,3), [3,4), [4,5)
  # with corresponding values:   1     1     10      1
  # There are k=e-s-1=3 possible change points:
  #         {1}{1,10,1} {1,1}{10,1} {1,1,10}{1}
  # The change point of index 1 is done in t=2
  # The change point of index 2 is done in t=3
  # The change point of index 3 is done in t=4

  ## Build initial df and df2 ----
  t = s:e # go from s to e included adding the two borders,
  # of length k+2 with k the number of possible change points
  # For example with x=(1,1,10,1), we have s=1, e=5, k=3 and length(s:e)=5=k+2
  l = build_df_df2(x, t, cost_func)

  ## Loop to trim the changes ----
  # At the beginning, all change points
  # Each step of the loop is removing one potential change point
  changepoint_gain = rep(0, k)

  for(loop in 1:k) {
    if(verbose) {
      if(k > 1000) {
        if(loop %% 1000 == 0) {
          print(paste0(loop, "/", k))
        }
      }
    }

    ## Compute the costs in removing one of the existing change points ----
    diff_costs = compute_diff_costs(l)

    ## Compute the difference of gains in removing one of the existing change points ----
    # With `cost_initial` the cost of the whole series s:e,
    # and `cost` the global cost of a certain set of cuts,
    # the `gain` is defined by:
    #     gain := 1 - cost/cost_initial.
    #
    # The gain is in (-inf, 1]:
    # A gain of 0 or negative is a bad cut,
    # A gain of 1 is cutting perfectly into boxes of size 0
    #
    # Given two cuts `cost_before` and `cost_after`, the difference of gains is given by:
    #     diff_gain = gain_before - gain_after
    #               = (1 - cost_before/cost_initial) - (1 - cost_after/cost_initial)
    #               = (cost_after-cost_before)/cost_initial
    #               = diff_cost/cost_initial
    diff_gains = diff_costs/cost_initial

    # Indexes of the change points that are still existing at that step
    replacement = l$df$e-1+(-s+1) # correction so that s=1 gives -s+1=0
    replacement = replacement[-length(replacement)]

    # Replace the elements of the changepoint_gain by the best seen difference of
    # gain before being removed.
    # For example, if a certain index has a gain of 0.09, there exists a step
    # at which removing this change point would increase the cost by an area of
    # 9% of the total area.

    # taking the max over all seen up to now works better
    changepoint_gain[replacement] = pmax(diff_gains, changepoint_gain[replacement])

    # The worst gain taking the current worst on the max function
    i = which.min(changepoint_gain[replacement])

    l = update_costs(l, i, x, cost_func)
  }

  return(changepoint_gain)
}
