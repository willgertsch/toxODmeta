# all logistic regression examples for paper

# linear


# quadratic
# set up
bound = 10
pts = 3
theta = c(2, 0, -0.1) # 3 points
theta = c(2, 0, -4)
theta = c(-2, 0, -0.1)
theta = c(-2, 0, -4)
grad_fun = grad.logistic.quad
obj_fun = obj.A
param = c()
obj_fun_M = obj_fun_factory(grad_fun, obj_fun, theta, param)


# run algorithm and get results
rangeVar = matrix(c(rep(c(0, bound), pts), rep(c(0,1), pts)), nrow = 2)
control = list(numPopulation = 100, maxIter = 1000)
result = metaheuristicOpt::metaOpt(
  obj_fun_M,
  optimType = "MAX",
  algorithm = "PSO",
  numVar = 2 * pts,
  rangeVar,
  control,
  seed = 1234
)
result

# check using equivalence theorem
vars = result$result
x = vars[1:pts]
w = vars[(pts+1):(2*pts)]
M = M.nonlinear(x, w, theta, grad_fun)
problem = list(bound = bound, obj = "A", theta = theta)
plot_sens(x, w, problem, M, grad_fun)

# cubic
# find good parameter values first
# values are from page 52 of Noursalehi 2000 Thesis
bound = 20
pts = 4
theta = c(-8.4013, 1.7995, -0.1267, 0.0030)
grad_fun = grad.logistic.cubic
obj_fun = obj.A
param = c()
obj_fun_M = obj_fun_factory(grad_fun, obj_fun, theta, param)


# run algorithm and get results
rangeVar = matrix(c(rep(c(0, bound), pts), rep(c(0,1), pts)), nrow = 2)
control = list(numPopulation = 100, maxIter = 1000)
result = metaheuristicOpt::metaOpt(
  obj_fun_M,
  optimType = "MAX",
  algorithm = "PSO",
  numVar = 2 * pts,
  rangeVar,
  control,
  seed = 1234
)
result

# check using equivalence theorem
vars = result$result
x = vars[1:pts]
w = vars[(pts+1):(2*pts)]
M = M.nonlinear(x, w, theta, grad_fun)
problem = list(bound = bound, obj = "A", theta = theta)
plot_sens(x, w, problem, M, grad_fun)

# fractional polynomial
# using DYME data example
bound = 500
pts = 3
theta = c(-5.14, -0.86, 0.73, 0, 0.5)
grad_fun = grad.logistic.fp
obj_fun = obj.A
param = c()
obj_fun_M = obj_fun_factory(grad_fun, obj_fun, theta, param)


# run algorithm and get results
rangeVar = matrix(c(rep(c(0, bound), pts), rep(c(0,1), pts)), nrow = 2)
control = list(numPopulation = 100, maxIter = 1000)
result = metaheuristicOpt::metaOpt(
  obj_fun_M,
  optimType = "MAX",
  algorithm = "DE",
  numVar = 2 * pts,
  rangeVar,
  control,
  seed = 1234
)
result

# check using equivalence theorem
vars = result$result
x = vars[1:pts]
w = vars[(pts+1):(2*pts)]
M = M.nonlinear(x, w, theta, grad_fun)
problem = list(bound = bound, obj = "A", theta = theta)
plot_sens(x, w, problem, M, grad_fun)

# log-logistic


# weibull
