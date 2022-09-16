# test main function

# update this to reference another source
# right now value comes from running the code that is being tested
test_that("logistic 0,1 on [0, 100]", {
  problem = list(
    model = "logistic",
    theta = c(0, 1),
    obj = "D",
    bound = 10,
    pts = 2
  )
  alg_options = list(
    algorithm = "PSO",
    iter = 500,
    swarm = 50
  )
  out = toxODmeta(problem, alg_options, 155)

  # should match to 3 decimal places
  expect_equal(round(out$optimumValue[1], 3), -3.595)
})
