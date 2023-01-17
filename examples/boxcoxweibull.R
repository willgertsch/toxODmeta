# example based off Backhaus et al (2000)
# Box-Cox Weibull model
bound = 5
pts = 3
theta = c(-3.942, 2.153, 0.495) # Pipemidic acid from table 4
grad_fun = grad.boxcoxweibull
obj_fun = obj.D # start by finding D optimal design
param = c()
obj_fun_M = obj_fun_factory(grad_fun, obj_fun, theta, param)
rangeVar = matrix(c(rep(c(0, bound), pts), rep(c(0,1), pts)), nrow = 2)
control = list(numPopulation = 100, maxIter = 500)
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
problem = list(bound = bound, obj = "D", theta = theta)
plot_sens(x, w, problem, M, grad_fun)


# find A optimal design
obj_fun = obj.A
obj_fun_M = obj_fun_factory(grad_fun, obj_fun, theta, param)
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
vars = result$result
x = vars[1:pts]
w = vars[(pts+1):(2*pts)]
M = M.nonlinear(x, w, theta, grad_fun)
problem = list(bound = bound, obj = "A", theta = theta)
plot_sens(x, w, problem, M, grad_fun)
