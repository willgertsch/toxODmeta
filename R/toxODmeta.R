# main function that the user/Shiny app calls
# inputs:
# problem: list used to define optimal design problem
# problem$model: model: logisticFP, exp, loglogistic, weibull
# problem$theta: local parameter values
# problem$obj: name of objective function
# problem$bound: upper dose limit
# problem$lam: dual objective parameter
# problem$d1: test dose
# problem$d2: reference dose
# problem$pts: number of design points
# alg_options: list used to define algorithm options
# alg_options$algorithm: name of algorithm to use
# alg_options$iter: number of iterations
# alg_options$swarm: swarm size
toxODmeta = function(problem, alg_options, seed) {

  # process input
  # select model
  model = problem$model
  if (model == "logistic") {
    M_fun = M.logistic
  }
  else if (model == "exp") {

  }
  else if (model == "loglogistic") {

  }
  else if (model == "weibull") {

  }
  else {
    stop("Model not supported")
  }

  # select objective function
  obj = problem$obj
  if (obj == "D") {
    obj_fun = obj.D
  }

  # make objective function
  theta = problem$theta
  obj_fun = obj_fun_factory(M_fun, obj_fun, theta)

  # set up bounds
  pts = problem$pts
  bound = problem$bound
  rangeVar = matrix(c(rep(c(0, bound), pts), rep(c(0,1), pts)), nrow = 2)

  # set up algorithm options
  swarm = alg_options$swarm
  iter = alg_options$iter
  control = list(numPopulation = swarm, maxIter = iter)

  # find optimal design using metaOpt
  algorithm = alg_options$algorithm
  result = metaheuristicOpt::metaOpt(
    obj_fun,
    optimType = "MAX",
    algorithm = algorithm,
    numVar = 2 * pts,
    rangeVar,
    control,
    seed = seed
  )

  # process output
  out = result
  return(out)
}
