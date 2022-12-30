# function for finding maximin designs
# theta grid is matrix with rows being parameter vectors
maximin = function(grad_fun, obj_fun, bound, pts, numPop, iter,
                   theta_grid, alg) {

  # forces

  # objective function
  f = function(vars, ...) {

    # compute design objective for each set of parameter values
    n = nrow(theta_grid)
    obj_vals = numeric(n)
    for (i in n) {
      theta = theta_grid[i, ]
      obj_fun_M = obj_fun_factory(grad_fun, obj_fun, theta, param)
      obj_vals[i] = obj_fun_M(vars)
    }

    # return minimum value
    return(min(obj_vals))
    #return(obj_vals[1])
  }

  # optimize
  rangeVar = matrix(c(rep(c(0, bound), pts), rep(c(0,1), pts)), nrow = 2)
  control = list(numPopulation = numPop, maxIter = iter)
  result = metaheuristicOpt::metaOpt(
    f,
    optimType = "MAX",
    algorithm = alg,
    numVar = 2 * pts,
    rangeVar,
    control
  )

  return(result)

}
