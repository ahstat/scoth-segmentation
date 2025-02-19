test_that("find_indexes gives the correct output", {
  x = c(1, 1, 10, 1)
  changepoint_gain = c(0, 0.75, 0.5)

  # indexes 2 and 3 and greater than 0.03
  expect_equal(find_indexes(changepoint_gain, s=1, threshold = 0.03),
               c(2,3))

  # indexes 1,2,3 and greater than 0.00 (threshold including the equality)
  expect_equal(find_indexes(changepoint_gain, s=1, threshold = 0),
               c(1,2,3))

  # shifting based on s:
  expect_equal(find_indexes(changepoint_gain, s=2, threshold = 0),
               c(2,3,4))
  # Begins with 2 which is the second possible change point, the first being 1 outside of the s:e interval
})

test_that("find_indexes gives the correct size of output when changepoint_gain has NA values", {
  # NA elements appear at level > 1, and correspond to the indexes that
  # have already been considered to be change points at the previous levels.
  #
  # We don't want to output those values again while finding indexes
  changepoint_gain2 = c(0, NA, 0.1, 0.2, 0.6, NA, NA, 3, NA)

  # output should be of length 5, not 9
  expect_equal(length(find_indexes(changepoint_gain2, s=1, threshold = 0)), 5)
})

test_that("get_partition_from_idx gives the correct output", {
  # Four elements:
  # [1,2) [2,3) [3,4) [4,5)
  # and change points are in position 2 and 3, so the first one is removed:
  # [1,3) [3,4) [4,5)
  x = c(1, 1, 10, 1)
  changepoint_gain = c(0, 0.75, 0.5)
  idx = c(2, 3)
  s = 1
  e = length(x)+1
  cost_func = c_box
  partition = get_partition_from_idx(idx, x, cost_func, s, e)
  expect_equal(partition[,c(1,2)], data.frame(s = c(1,3,4),
                                              e = c(3,4,5)))

  # Four elements:
  # [1,2) [2,3) [3,4) [4,5)
  # and all change points are kept
  x = c(1, 1, 10, 1)
  idx = c(1, 2, 3)
  s = 1
  e = length(x)+1
  partition = get_partition_from_idx(idx, x, cost_func, s, e)
  expect_equal(partition[,c(1,2)], data.frame(s = c(1,2,3,4),
                                              e = c(2,3,4,5)))

  # Four elements:
  #      v     v     v      (and 3 possible change points v)
  # [1,2) [2,3) [3,4) [4,5)
  # but begin at 2:
  #       [2,3) [3,4) [4,5)
  # and only the third change point (w.r.t to the initial elements) is kept:
  #       [2,      4) [4,5)
  x = c(1, 1, 10, 1)
  idx = c(3)
  s = 2
  e = length(x)+1
  partition = get_partition_from_idx(idx, x, cost_func, s, e)
  expect_equal(partition[,c(1,2)], data.frame(s = c(2,4),
                                              e = c(4,5)))

  # corner case
  x = c(1, 1, 10, 1)
  idx = c()
  s = 1
  e = length(x)+1
  partition = get_partition_from_idx(idx, x, cost_func, s, e)
  expect_equal(partition[,c(1,2)], data.frame(s = s,
                                              e = e))
})

test_that("get_partition_from_changepoint_gain is coherent", {
  # `get_partition_from_changepoint_gain` is just combining `find_indexes` and `get_partition_from_idx`
  x = c(1, 1, 10, 1)
  changepoint_gain = c(0, 0.75, 0.5)
  s = 1
  e = length(x)+1
  cost_func = c_box

  threshold = 0.6 # test with threshold = 0.6
  partition_to_test = get_partition_from_changepoint_gain(changepoint_gain, x, cost_func, s, e, threshold)
  # we know it should give idx = c(2)
  idx = c(2)
  partition = get_partition_from_idx(idx, x, cost_func, s, e)
  expect_equal(partition_to_test, partition)
})

test_that("iterate_changepoint_gain_from_partition is corner cases", {
  cost_func = c_box

  ## Example with no change points at the first step ----
  x = c(1,2,3,4,5)
  s=1
  e=length(x)+1
  cost_initial = cost_func(x,s,e) # global cost
  changepoint_gain = scoth_changes(x, cost_func, s, e, verbose = FALSE)
  partition = get_partition_from_changepoint_gain(changepoint_gain, x, cost_func, s, e, threshold=10)
  expect_equal(partition[,c(1,2)], data.frame(s = s, e = e)) # test already done before
  # the partition has not changed from level 0, so the update should not change the changepoint_gain:
  changepoint_gain2 = iterate_changepoint_gain_from_partition(partition, changepoint_gain, cost_initial)
  expect_equal(changepoint_gain, changepoint_gain2)

  ## Example with only 0 ----
  x = c(0, 0, 0, 0, 0)
  s=1
  e=length(x)+1
  cost_initial = cost_func(x,s,e) # global cost
  changepoint_gain = scoth_changes(x, cost_func, s, e, verbose = FALSE)
  expect_equal(changepoint_gain, c(0,0,0,0)) # test already done before
  # 1. partition with threshold > 0 == cost
  partition = get_partition_from_changepoint_gain(changepoint_gain, x, cost_func, s, e, threshold=10)
  expect_equal(partition, get_partition_from_idx(c(), x, cost_func, s, e)) # same as doing nothing, no change point
  changepoint_gain2 = iterate_changepoint_gain_from_partition(partition, changepoint_gain, cost_initial)
  expect_equal(changepoint_gain, changepoint_gain2) # as before, still 0 cost, and same length
  # 2. partition with threshold == 0 == cost
  partition = get_partition_from_changepoint_gain(changepoint_gain, x, cost_func, s, e, threshold=0)
  expect_equal(partition, get_partition_from_idx(s:(e-2), x, cost_func, s, e)) # same as doing nothing, no change point
  changepoint_gain2 = iterate_changepoint_gain_from_partition(partition, changepoint_gain, cost_initial)
  expect_equal(changepoint_gain2, rep(NA, length(changepoint_gain))) # same length as changepoint_gain, but all NA since already filed with change points
})

test_that("iterate_changepoint_gain_from_partition with the different possible adaptations", {
  cost_func = c_box
  x = c(1,2,5,6,20,21,25,26)
  s=1
  e=length(x)+1
  cost_initial = cost_func(x,s,e) # global cost
  changepoint_gain = scoth_changes(x, cost_func, s, e, verbose = FALSE)
  expect_equal(changepoint_gain[1:3], c(0.01,0.08,0.01))
  # cutting when the value 20 appeared
  partition = get_partition_from_changepoint_gain(changepoint_gain, x, cost_func, s, e, threshold=0.5)
  # we have changepoint_gain=[0.01 0.08 0.01 0.78 0.01 0.10 0.01] and threshold=0.5, so the cut, the first interval becomes: [0.01 0.08 0.01]
  # the initial global cost is 200 = 8*(26-1)  t*y
  # for the first interval, the cost is 20 = 4*(6-1) t*y
  # We adjust the zoom:
  changepoint_gain_level2_global_w = iterate_changepoint_gain_from_partition(partition, changepoint_gain, cost_initial)
  V_segmentation = sum(partition$cost) # the new total cost is 20+24=44, compared to the initial global cost: V(/)=200
  # compared to the fully local zoom, we multiply by 4.5 instead of by 10
  expect_equal(changepoint_gain_level2_global_w[1:3], (200/44)*c(0.01,0.08,0.01))
})
