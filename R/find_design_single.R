# function for finding single objective designs
find_design_single = function(
    grad_fun,
    obj,
    theta,
    bound,
    pts,
    algorithm,
    swarm,
    iter,
    seed
  ) {

  # design objective
  if (obj == "D")
    obj_fun = obj.D
  else if (obj == "A")
    obj_fun = obj.A
  else
    stop("Objective not supported")

  # objective function
  param = c()
  obj_fun_M = obj_fun_factory(grad_fun, obj_fun, theta, param)

  # set up variable bounds
  rangeVar = matrix(c(rep(c(0, bound), pts), rep(c(0,1), pts)), nrow = 2)

  # algorithm options
  control = list(numPopulation = swarm, maxIter = iter)

  # find design
  result = metaheuristicOpt::metaOpt(
    obj_fun_M,
    optimType = "MAX",
    algorithm = algorithm,
    numVar = 2 * pts,
    rangeVar,
    control,
    seed = seed
  )

  # check optimality
  vars = result$result
  x = vars[1:pts]
  w = vars[(pts+1):(2*pts)]
  M = M.nonlinear(x, w, theta, grad_fun)
  problem = list(bound = bound, obj = obj, theta = theta)
  p = plot_sens(x, w, problem, M, grad_fun)

  return(list(result = result, plot = p))
}
