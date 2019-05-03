context("Test checkers")
test_that("checkers work as expected", {
  expect_true(check_prob(1))
  expect_length(check_prob(0), 1)
  expect_error(check_prob(-1))

  expect_true(check_trials(4))
  expect_error(check_trials(0.5))
  expect_error(check_trials(-1))

  expect_true(check_success(2,4))
  expect_error(check_success(4,2))
  expect_error(check_success(0.5, 3))
})

context("Summary Measures")
test_that("summary measures work as expected", {
  expect_equal(aux_mean(10, 0.3), 3)
  expect_equal(aux_mean(10, 0.5), 5)
  expect_equal(aux_mean(10, 0.7), 7)

  expect_equal(aux_variance(10, 0.3), 2.1)
  expect_equal(aux_variance(0, 0.9), 0)
  expect_equal(aux_variance(10, 0.5), 2.5)

  expect_equal(aux_mode(10, 0.3), 3)
  expect_equal(aux_mode(10, 0.5), 5)
  expect_equal(aux_mode(5,0.5), c(3,2))

  expect_length(aux_skewness(10, 0.3), 1)
  expect_type(aux_skewness(10, 0.3), "double")
  expect_length(aux_skewness(c(10,5), 0.1), 2)

  expect_length(aux_kurtosis(10, 0.3), 1)
  expect_type(aux_skewness(10, 0.3), "double")
  expect_length(aux_kurtosis(c(5,10), 0.3), 2)
})

context("Binomial")
test_that("binomial functions work as expected", {
  expect_equal(bin_choose(5,2), 10)
  expect_equal(bin_choose(5,2) %% 1, 0)
  expect_type(bin_choose(5,2), "double")

  expect_equal(bin_probability(2,5,0.5), 0.3125)
  expect_length(bin_probability(0:2, 5, 0.5),3)
  expect_type(bin_probability(0:2, 5, 0.5), "double")

  expect_type(bin_distribution(5,0.5), "list")
  expect_is(bin_distribution(5, 0.5), c("bindis", "data.frame"))
  expect_length(bin_distribution(5, 0.5), 2)

  expect_type(bin_cumulative(5,0.5), "list")
  expect_is(bin_cumulative(5, 0.5), c("bincum", "data.frame"))
  expect_length(bin_cumulative(5, 0.5), 3)
})

