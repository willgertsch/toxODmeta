# main function that the user/Shiny app calls
# inputs:
# problem: list used to define optimal design problem
# problem$model: model: logistic, exp, loglogistic, weibull
# problem$theta: local parameter values
# problem$obj: name of objective function
# problem$bound: upper dose limit
# problem$lam: dual objective parameter
# problem$d1: test dose
# problem$d0: reference dose
# problem$pts: number of design points
# alg_options: list used to define algorithm options
# alg_options$algorithm: name of algorithm to use
# alg_options$iter: number of iterations
# alg_options$swarm: swarm size
toxODmeta = function(problem, alg_options, seed) {

  # process input
  model = problem$model
  obj = problem$obj
  theta = problem$theta
  pts = problem$pts
  bound = problem$bound
  d1 = problem$d1
  d0 = problem$d0
  swarm = alg_options$swarm
  iter = alg_options$iter
  algorithm = alg_options$algorithm

  # select model
  if (model == "logistic") {
    grad_fun = grad.logistic
  }
  else if (model == "logistic-quadratic") {
    grad_fun = grad.logistic.quad
  }
  else if (model == "logistic-cubic") {
    grad_fun = grad.logistic.cubic
  }
  else if (model == "exponential") {
    grad_fun = grad.exponential
  }
  else if (model == "loglogistic") {
    grad_fun = grad.loglogistic
  }
  else if (model == "weibull") {
    grad_fun = grad.weibull
  }
  else {
    stop("Model not supported")
  }

  # select objective function
  param = c() # options for objective function
  if (obj == "D") {
    obj_fun = obj.D
  }
  else if (obj == "A") {
    obj_fun = obj.A
  }
  # else if (obj == "addrisk") {
  #   obj_fun = obj.addrisk
  #
  #   # compute gradients
  #   grad = grad_fun_factory(model)
  #   param = c(grad(d1), grad(d0))
  # }
  else
    stop("Objective not supported")

  # make objective function
  obj_fun_M = obj_fun_factory(grad_fun, obj_fun, theta, param)

  # set up bounds
  rangeVar = matrix(c(rep(c(0, bound), pts), rep(c(0,1), pts)), nrow = 2)

  # set up algorithm options
  control = list(numPopulation = swarm, maxIter = iter)

  # find optimal design using metaOpt
  result = metaheuristicOpt::metaOpt(
    obj_fun_M,
    optimType = "MAX",
    algorithm = algorithm,
    numVar = 2 * pts,
    rangeVar,
    control,
    seed = seed
  )

  # extract design points and weights
  vars = result$result
  x = vars[1:pts]
  w = vars[(pts+1):(2*pts)]

  # compute information matrix for optimal design
  M = M.nonlinear(x, w, theta, grad_fun)

  # plot sensitivity function
  result$sens_plot = plot_sens(x, w, problem, M, grad_fun)


  # process output
  out = result
  return(out)
}
