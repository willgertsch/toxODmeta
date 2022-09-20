test_that("Example 1", {
  M = matrix(c(1,0,1,0), nrow=2)
  t = checkMinv(M)
  expect_equal(t, 0)
})

test_that("Example 2", {
  M = matrix(c(1,0,0,1), nrow=2)
  t = checkMinv(M)
  expect_equal(t, 1)
})

test_that("Example 3", {
  M = matrix(c(1,0,0,0,1,0,0,0,1), nrow=3)
  t = checkMinv(M)
  expect_equal(t, 1)
})

test_that("Example 4", {
  M = matrix(c(1,0,0,0,1,0,1,0,0), nrow=3)
  t = checkMinv(M)
  expect_equal(t, 0)
})
