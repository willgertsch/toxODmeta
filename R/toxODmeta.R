# main function that the user/Shiny app calls
# inputs:
# problem: list used to define optimal design problem
# problem$model: model: logisticFP, exp, loglogistic, weibull
# problem$beta: local parameter values
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
toxODmeta = function(problem, alg_options) {

  # process input
  model = problem$model
  if (model == "logisticFP") {

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

  # find optimal design

  # process output
  out = list()
  return(out)
}
