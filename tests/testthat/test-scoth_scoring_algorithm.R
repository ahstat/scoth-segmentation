test_that("build_df_df2 gives coherent results for one example with t={1, 5, 14, 29, e}", {
  # Example:
  # With t={1, 5, 14, 29, e} with e=length(x)+1, the df is:
  #  s   e   cost
  #  1   5   cost([ 1,  5))
  #  5  14   cost([ 5, 14))
  # 14  29   cost([14, 29))
  # 29   e   cost([29,  e)) = cost([29,length(x)])
  #
  # and df2 is:
  #  s   e   cost
  #  1  14   cost([ 1, 14))
  #  5  29   cost([ 5, 29))
  # 14   e   cost([14,  e)) = cost([14,length(x)])
  cost_func = c_box # c_box is a dummy cost function based on the area
  set.seed(1234)
  x = rnorm(100)
  e = length(x)+1
  t = c(1, 5, 14, 29, e)
  l = build_df_df2(x, t, cost_func)
  expect_equal(l$df$s, c(1,5,14,29))
  expect_equal(l$df$e, c(5,14,29,101))
  expect_equal(l$df2$s, c(1,5,14))
  expect_equal(l$df2$e, c(14,29,101))
})

test_that("build_df_df2 gives correct results for one example with t={1, 5, 14, 29, e}", {
  cost_func = c_box
  set.seed(1234)
  x = rnorm(100)
  e = length(x)+1
  t = c(1, 5, 14, 29, e)
  l = build_df_df2(x, t, cost_func)

  k = nrow(l$df)-1 # 3              v      v       v
  # Currently 3 change points: [1,5) [5,14) [14,29) [29,e)
  # The cost is sum(l$df$cost) for now (before removing one of the changes)

  # We can either remove change point 1, 2, or 3:
  # By removing change point 1: [1,14) [14,29) [29,e)
  # By removing change point 2: [1,5) [5,29) [29,e)
  # By removing change point 3: [1,5) [5,14) [14,e)
  #
  # We see that removing change point i corresponds to:
  # 1. take the previous global cost,
  # 2. then subtracting the costs of the intervals at row i and i+1,
  # 3. finally adding the cost of the interval corresponding to the removed pair of interval
  #
  # For i=2:
  #       after                         before                         cost of single intervals  cost of the pair
  # cost([1,5) [5,29) [29,e)) = cost([1,5) [5,14) [14,29) [29,e)) - cost([5,14)) - cost([14,29)) + cost([5,29))

  # Overall, the changepoint_gain is a the vector c(0,18,18)
  expect_equal(compute_diff_costs(l),
               c(l$df2$cost[1] + l$df$cost[3] + l$df$cost[4] - sum(l$df$cost),
                 l$df$cost[1] + l$df2$cost[2] + l$df$cost[4] - sum(l$df$cost),
                 l$df$cost[1] + l$df$cost[2] + l$df2$cost[3] - sum(l$df$cost)))
})

test_that("compute_diff_costs gives the correct output for x=(1,1,10,1)", {
  cost_func = c_box
  x = c(1,1,10,1)
  s = 1
  e = length(x)+1
  t = s:e
  l = build_df_df2(x, t, cost_func)

  # number of existing change points
  k = nrow(l$df)-1 # 3            v   v    v
  # Currently 3 change points: {1} {1} {10} {1}
  # The cost is 0 for now (before removing one of the changes)

  # We can either remove change point 1, 2, or 3:
  # By removing change point 1: {1,1}{10}{1}, the cost after is still 0, so after-before=0-0=0
  # By removing change point 2: {1}{1,10}{1}, the cost after is 18, so after-before=18-0=18
  # By removing change point 3: {1}{1}{10,1}, the cost after is 18, so after-before=18-0=18

  # Overall, the changepoint_gain is a the vector c(0,18,18)
  expect_equal(compute_diff_costs(l), c(0,18,18))

  # The cost before (=global box without change points) is 36 = 9*4 = range(values)*|[1,5)|
  # The cost original (with all the change points not trimmed) is 0 (sum of df$l costs)
  #  _________       ___________
  # |     .   |     |  |  |. |  |
  # |         |     |  |  |  |  |
  # |         | --> |  |  |  |  | df
  # | . .   . |     | .| .|  |. |
  # |_________|     |__|__|__|__|
  #  1 2 3 4         0  0  0  0 = E.g. cost of [1,2) = 1*0 = 0
  #                [1,2) [3,4)
  #                  [2,3)  [4,5)
  #  _________       ___________
  # |     .   |     |  |  . |  |
  # |         |     |  |    |  |
  # |         | --> |          | df2
  # | . .   . |     | . .|   . |
  # |_________|     |____|_____|
  #                   0 18 18   = E.g. cost of [3,5) = 2*9 = 18 (9 = 10 - 1 range of values)
  #  1 2 3 4        [1,3) [3,5)
  #                    [2,4)
  #
  # From those 3 positioned change point, we look at the gain of removing any of those change points:
  # There are 3 possibilities:
  #
  #  _______ (removing change point 1)
  # |   |.| |
  # |   | | |
  # |   | | | of cost 0+0+0=0
  # |. .| |.|
  # |___|_|_|
  #  1 2 3 4
  #
  #  _______ (removing change point 2)
  # | |  .| |
  # | |   | |
  # | |   | | of cost 0+18+0=18
  # |.|.  |.|
  # |_|___|_|
  #  1 2 3 4
  #
  #  _______ (removing change point 3)
  # | | |.  |
  # | | |   |
  # | | |   | of cost 0+0+18=18
  # |.|.|  .|
  # |_|_|___|
  #  1 2 3 4
  #
  # In terms of gains:
  # The gain before is going from a cost of 36 to a cost of 0, which is a gain of 1 = 100%
  # The gain after is going from a cost of 36 to a cost of either (0, 18, 18), which is a gain of (100%, 50%, 50%)
  # The difference of gain after removing the change i=(1,2,3) is diff_gains=(0%,50%,50%)
  # The smallest diff_gains is the one which is the less affecting the quality while removing a point
  # In this case it's i=1
})

test_that("update_costs gives a coherent output shape for an initial state with 8 change points", {
  cost_func = c_box
  set.seed(1234)
  x = rnorm(100)
  e = length(x)+1
  t = c(1, 5, 14, 29, 50, 51, 52, 78, 79, e)
  l = build_df_df2(x, t, cost_func)

  # number of existing change points
  k = nrow(l$df)-1 # 8
  # We have
  # nrow(l$df) == k+1
  # nrow(l$df2) == k

  # input with k = 8
  # - df:  [ 1  | 2  | 3  | 4  | 5  | 6  | 7  | 8  | 9 ]
  # - df2: [ 12 | 23 | 34 | 45 | 56 | 67 | 78 | 89 ]
  # The possible choices for cut goes from 1 to k

  # A -- selected cut idx_min: i=3
  #
  # output:     v    v    X    v    v    v    v    v
  # - df:  [ 1  | 2  | 3  | 4  | 5  | 6  | 7  | 8  | 9 ]  before
  #        [ 1  | 2  |   34    | 5  | 6  | 7  | 8  | 9 ]  after
  # Algorithm: 1. Remove row i+1 of df (always possible cf i < k+1);
  #            2. Replace row i of df by row i of df2 (always possible because i <= k)
  #
  # - df2:        i-1    i    i+1
  #        [ 12 | 23  | 34  | 45 | 56 | 67 | 78 | 89 ]    before
  #        [ 12 | 234 | 345 | 56 | 67 | 78 | 89 ]         after
  # Algorithm: 1. Remove row i of df2;
  #            2. Replace row i-1 with (i-1,i,i+1)
  #            3. Replace row i with (i,i+1,i+2) [which was at position i+1 before step 1]
  i = 3
  l_updated3 = update_costs(l, i, x, cost_func)
  expect_equal(length(l_updated3), 2)
  # third change point is 29 (in position 3+1=4, since the borders are 1 and e)
  expect_equal(l_updated3$df$s, c(1, 5, 14, 50, 51, 52, 78, 79))
  expect_equal(l_updated3$df$e, c(5, 14, 50, 51, 52, 78, 79, e))
  expect_equal(l_updated3$df$cost[1:2], l$df$cost[1:2]) # first two cells above
  expect_true(l_updated3$df$cost[3] >= sum(l$df$cost[3:4])) # merged cell, there is an increase of cost because less change points detected
  expect_equal(l_updated3$df$cost[4:nrow(l_updated3$df)], l$df$cost[5:nrow(l$df)]) # remaining are equals
  # for df2 (took from l$df2 and remove element 29)
  expect_equal(l_updated3$df2$s, c(1, 5, 14, 50, 51, 52, 78))
  expect_equal(l_updated3$df2$e, c(14, 50, 51, 52, 78, 79, e))

  # B -- selected cut idx_min: i=1 (corner case i=1)
  #
  # output:     X    v    v    v    v    v    v    v
  # - df:  [ 1  | 2  | 3  | 4  | 5  | 6  | 7  | 8  | 9 ]  before
  #        [   12    | 3  | 4  | 5  | 6  | 7  | 8  | 9 ]  after
  # Algorithm: Same as in the case A.
  #
  # - df2:    i   i+1
  #        [ 12 | 23  | 34 | 45 | 56 | 67 | 78 | 89 ]    before
  #        [ 123      | 34 | 56 | 67 | 78 | 89 ]         after
  # Algorithm: 1. Remove row i of df2;
  #            X. Don't replace row i-1 with (i-1,i,i+1) because i-1=0 not existing
  #            3. Replace row i with (i,i+1,i+2) [which was at position i+1 before step 1]
  i = 1
  l_updated1 = update_costs(l, i, x, cost_func)
  expect_equal(length(l_updated1), 2)
  # first change point is 5 (in position 1+1=2, since the borders are 1 and e)
  expect_equal(l_updated1$df$s, c(1, 14, 29, 50, 51, 52, 78, 79))
  expect_equal(l_updated1$df$e, c(14, 29, 50, 51, 52, 78, 79, e))
  expect_true(l_updated1$df$cost[1] >= sum(l$df$cost[1:2])) # merged cell, there is an increase of cost because less change points detected
  expect_equal(l_updated1$df$cost[2:nrow(l_updated3$df)], l$df$cost[3:nrow(l$df)]) # remaining are equals

  # C -- selected cut idx_min: i=k (corner case i=k)
  #
  # output:     v    v    v    v    v    v    v    X
  # - df:  [ 1  | 2  | 3  | 4  | 5  | 6  | 7  | 8  | 9 ]  before
  #        [ 1  | 2  | 3  | 4  | 5  | 6  | 7  |   89   ]  after
  # Algorithm: Same as in the case A.
  #
  # - df2:                                 i-1   i
  #        [ 12 | 23  | 34 | 45 | 56 | 67 | 78 | 89 ]    before
  #        [ 12 | 23  | 34 | 45 | 56 | 67 | 789 ]        after
  # Algorithm: 1. Remove row i of df2;
  #            2. Replace row i-1 with (i-1,i,i+1)
  #            X. Don't replace row i with (i,i+1,i+2) [which was at position i+1=k+1 i.e. this position does not exist]
  i = k
  l_updatedk = update_costs(l, i, x, cost_func)
  expect_equal(length(l_updatedk), 2)
  # last change point is 79
  expect_equal(l_updatedk$df$s, c(1, 5, 14, 29, 50, 51, 52, 78))
  expect_equal(l_updatedk$df$e, c(5, 14, 29, 50, 51, 52, 78, e))
  expect_equal(l_updatedk$df$cost[1:7], l$df$cost[1:7]) # firsts are equals
  expect_true(l_updatedk$df$cost[8] >= sum(l$df$cost[8:9])) # merged cell, there is an increase of cost because less change points detected
})

test_that("update_costs gives the correct shape for an example with a cost that is possible to compute by hand", {
  cost_func = c_box
  set.seed(1234)
  x = c(1,1,10,1)
  s = 1
  e = length(x)+1
  t = s:e
  l = build_df_df2(x, t, cost_func)
  k = nrow(l$df)-1 # 3, nb change points

  # We want to remove the index i=1
  i = 1
  # the index i=1 is the first change point, but since there is a border, it's
  # the element 2. Then we eliminate all `2` in df and df2, and the cost is as expected
  # (The borders should not change)
  l1 = update_costs(l, i, x, cost_func)
  # On df: we didn't lose anything, so all the costs are still 0:
  # _______ (removing change point 1)
  #|   |.| |
  #|   | | |
  #|   | | | of cost 0+0+0=0
  #|. .| |.|
  #|___|_|_|
  #[1,3) [4,5)
  #   [3,4)
  expect_equal(as.numeric(l1$df[1,]), c(1, 3, 0)) # [1,3) with cost of 0
  expect_equal(as.numeric(l1$df[2,]), c(3, 4, 0)) # [3,4) with cost of 0
  expect_equal(as.numeric(l1$df[3,]), c(4, 5, 0)) # [4,5) with cost of 0
  #
  # On df2: we remove the first change point, so we got:
  # _______ (removing change point 1)
  #|   |.  |
  #|   |   |
  #|       |
  #|. .  |.|
  #|___ _|_|
  #[1,4) of cost 27 = 3*9
  #    [3,5) of cost 18 = 2*9
  expect_equal(as.numeric(l1$df2[1,]), c(1, 4, 27)) # [1,4) with cost of 27
  expect_equal(as.numeric(l1$df2[2,]), c(3, 5, 18)) # [3,5) with cost of 18

  # Conclusion: it's normal that when i = which.min(diff_gains), the removed element
  # in df and df2 is i+1 (since i=1 corresponds to the 2, since the first border cannot
  # be selected)
})

test_that("scoth_changes gives the correct output for an example with a cost that is possible to compute by hand", {
  cost_func = c_box
  x = c(1,1,10,1)
  s = 1
  e = length(x)+1
  changepoint_gain = scoth_changes(x, cost_func, s, e)
  # at step 1, the gain is 0 after removing change point 1, so it can be removed
  l = build_df_df2(x, s:e, cost_func)
  diff_costs = compute_diff_costs(l)
  i = 1 # which.min(diff_costs)
  l1 = update_costs(l, i, x, cost_func)
  diff_costs = compute_diff_costs(l1)
  # The minimum is the second element, corresponding to the third change point: diff_costs/36
  (diff_costs/36)[2] == 0.5
  (diff_costs/36)[1] == 0.75 # so with the pmax method, replaced with 0.75 at this step
  i = 2 # which.min(diff_costs)
  l2 = update_costs(l1, i, x, cost_func)
  diff_costs = compute_diff_costs(l2)
  (diff_costs/36)[1] == 0.5 # lower than 0.75 put before, so we keep 0.75
  expect_equal(changepoint_gain, c(0, 0.75, 0.5))

  # In this case, if we select, contrary to the algorithm 1 of our methodology,
  # the last gain instead of the maximum gain, we would observe c(0, 0.5, 0.5) instead
})

test_that("scoth_changes gives the correct output in a corner case", {
  cost_func = c_box
  x = c(1,1,1,1,1)
  s = 1
  e = length(x)+1
  changepoint_gain = scoth_changes(x, cost_func, s, e)
  #           v     v     v     v
  # x is [1,2) [2,3) [3,4) [4,5) [5,6)
  # so there are four possible change points, here each with a cost of 0:
  expect_equal(changepoint_gain, c(0,0,0,0))
})

test_that("scoth_scoring_func gives the correct output in some cases", {
  cost_func = c_box
  x = c(1,2,3,4,5)
  l = scoth_scoring_func(x, cost_func, verbose = FALSE)
  # the initial cost is the cost of the whole interval
  expect_equal(l$cost_initial, cost_func(x, s=1, e=length(x)+1))
  # the gain is the gain for the whole interval
  expect_equal(l$changepoint_gain, scoth_changes(x, cost_func, s=1, e=length(x)+1))

  # corner case
  cost_func = c_box
  x = c(1,1,1,1,1)
  l = scoth_scoring_func(x, cost_func, verbose = FALSE)
  # the initial cost is the cost of the whole interval
  expect_equal(l$cost_initial, 0)
  # the gain is the gain for the whole interval
  expect_equal(l$changepoint_gain, rep(0, length(x)-1))
})
