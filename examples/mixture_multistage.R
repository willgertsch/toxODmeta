# work based off Razzaghi (2002)
# a mixture model of two degree 2 multistage models
# algorithm parameters chosen by simulation study
bound = 150
pts = 6
# parameter values from table 2
# gamma0, gamma11, gamma12, gamma21, gamma22, theta
theta = c(.00833, -.10050, .00067, -0.00004, .00006, .00497)
grad_fun = grad.mix2
obj_fun = obj.D # start by finding D optimal design
param = c()
obj_fun_M = obj_fun_factory(grad_fun, obj_fun, theta, param)
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
problem = list(bound = bound, obj = "D", theta = theta)
plot_sens(x, w, problem, M, grad_fun)
# seem to have trouble with matrix being singular
# only happens if the design space is too small

# equally weighted design
# 0, 20, 49, 78, 109, 150
# original design
# 0, 30, 35, 45, 60, 75, 100, 150
# 8 points vs 6 points
x
w

# figure out design efficiencies
D_opt = obj_fun_M(vars)
D = obj_fun_M(c(0, 30, 35, 45, 60, 75, 100, 150, rep(1/8, 8)))

(D/D_opt)^(1/6) # very high efficiency
# 0.9925327

# find A optimal design
pts = 6
obj_fun = obj.A
obj_fun_M = obj_fun_factory(grad_fun, obj_fun, theta, param)
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
vars = result$result
x = vars[1:pts]
w = vars[(pts+1):(2*pts)]
M = M.nonlinear(x, w, theta, grad_fun)
problem = list(bound = bound, obj = "A", theta = theta)
plot_sens(x, w, problem, M, grad_fun)

A_opt = obj_fun_M(vars)
A = obj_fun_M(c(0, 30, 35, 45, 60, 75, 100, 150, rep(1/8, 8)))

A_opt/A # 0.3205199


# testing variation of local parameter values
# checking equivalence theorem
check_equiv = function(result, grad_fun, crit, theta, bound, pts) {
  vars = result$result
  x = vars[1:pts]
  w = vars[(pts+1):(2*pts)]
  M = M.nonlinear(x, w, theta, grad_fun)
  problem = list(bound = bound, obj = crit, theta = theta)
  plot_sens(x, w, problem, M, grad_fun)
}

# test
check_equiv(result, grad_fun, "D", theta, 150, 6)

# theta_variations is matrix
locality_sensitivity = function(theta, theta_variations, crit) {

  # find optimal design assuming theta
  bound = 150
  pts = 6
  grad_fun = grad.mix2
  if (crit == "D")
    obj_fun = obj.D
  else if (crit == "A")
    obj_fun = obj.A
  else
    stop()

  param = c()
  obj_fun_M = obj_fun_factory(grad_fun, obj_fun, theta, param)
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

  check_equiv(result, grad_fun, crit, theta, bound, pts)

  # get objective value
  obj_val = obj_fun_M(result$result)
  print(obj_val)

  # loop through variations on theta
  for (i in 1:nrow(theta_variations)) {
    theta_var = theta_variations[i, ]

    # find optimal design
    obj_fun_M = obj_fun_factory(grad_fun, obj_fun, theta_var, param)
    result_i = metaheuristicOpt::metaOpt(
      obj_fun_M,
      optimType = "MAX",
      algorithm = "PSO",
      numVar = 2 * pts,
      rangeVar,
      control,
      seed = 1234
    )

    # plot sensitivity function
    check_equiv(result_i, grad_fun, crit, theta_var, bound, pts)

    # compute efficiency of design assuming theta for each variation
    obj_val_i = obj_fun_M(result_i$result)
    print(obj_val_i)

    if (crit == "D")
      eff = (obj_val/obj_val_i)^(1/6)
    else if (crit == "A")
      eff = obj_val_i/obj_val

    print(theta_var)
    print(eff)
  }
}

theta1 = c(.00833, -.10050, .00067, -0.00004, .00006, 0.0001)
theta2 = c(.00833, -.10050, .00067, -0.00004, .00006, 0.2)
theta3 = c(.00833, -.10050, .00067, -0.00004, .00006, 0.5)
theta4 = c(.00833, -.10050, .00067, -0.00004, .00006, 0.8)
theta5 = c(.00833, -.10050, .00067, -0.00004, .00006, 0.9999)
theta_variations = rbind(theta1, theta2, theta3, theta4, theta5)
locality_sensitivity(c(.00833, -.10050, .00067, -0.00004, .00006, .00497),
                     theta_variations, "D")
locality_sensitivity(c(.00833, -.10050, .00067, -0.00004, .00006, .00497),
                     theta_variations, "A")


# minimax stuff ################################################################

# simulation study to confirm optimality
# loop through  algorithms for a few different choices of design points
pts = c(2, 3, 4, 5, 6, 8, 10)
algorithms = c(
  "PSO",
  "GWO",
  "ALO",
  "DE",
  "HS"
)
replicates = 5
control = list(numPopulation = 100, maxIter = 500)
obj_vals = c()
times = c()
design_points = c()
set.seed(1234)
for (i in 1:length(algorithms)) {
  algorithm = algorithms[i]
  for (j in 1:length(pts)) {
    num_pts = pts[j]
    rangeVar = matrix(c(rep(c(0, bound), num_pts), rep(c(0,1), num_pts)), nrow = 2)
    for (k in 1:replicates) {
      cat(algorithm, " with ", num_pts, " design points: ", k, "/", replicates, "\n")
      result = metaheuristicOpt::metaOpt(
        obj_fun_M,
        optimType = "MAX",
        algorithm = algorithm,
        numVar = 2 * num_pts,
        rangeVar,
        control
      )
      obj_vals = c(obj_vals, result$optimumValue)
      times = c(times, result$timeElapsed[1])
      design_points = c(design_points, num_pts)
    }
  }
}

df = data.frame(
  obj = obj_vals,
  time = times,
  algorithm = rep(algorithms, each = replicates*length(pts)),
  design_points = design_points
)

library(dplyr)
library(ggplot2)
df %>%
  ggplot(aes(x = design_points, y = obj, color = algorithm)) +
  geom_point()
# appears that 6 points is the best

df %>%
  filter(design_points == 6) %>%
  ggplot(aes(x = algorithm, y = obj, color = algorithm)) +
  geom_point() +
  theme_bw()
# HS seems best, followed by DE

# maximin for different mixing parameters
theta_grid = matrix(
  c(
    .00833, -.10050, .00067, -0.00004, .00006, .00497,
    .01, -.10050, .00067, -0.00004, .00006, .00497,
    .05, -.10050, .00067, -0.00004, .00006, .00497,
    .1, -.10050, .00067, -0.00004, .00006, .00497,
    .5, -.10050, .00067, -0.00004, .00006, .00497
  ),
  5, 6, byrow = T
)

result_maximin = maximin(
  grad_fun = grad_fun,
  obj_fun = obj_fun,
  bound = bound,
  pts = 10,
  numPop = 100,
  iter = 1000,
  theta_grid,
  alg = "PSO"
)

result_maximin
# 0
