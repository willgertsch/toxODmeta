# example of optimal designs for mixture modelss
# model with logit link and equal intercepts
# baseline probability of response will be .01
# odds are 1.5 per unit in one group and 2.5 in other
bound = 10
pts = 4
theta = c(0.7, log(.01/.99), log(1.5), log(2.5))
grad_fun = grad.mix1
obj_fun = obj.D
param = c()
obj_fun_M = obj_fun_factory(grad_fun, obj_fun, theta, param)
rangeVar = matrix(c(rep(c(0, bound), pts), rep(c(0,1), pts)), nrow = 2)
control = list(numPopulation = 100, maxIter = 500)
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

# check
vars = result$result
x = vars[1:pts]
w = vars[(pts+1):(2*pts)]
M = M.nonlinear(x, w, theta, grad_fun)
problem = list(bound = bound, obj = "D", theta = theta)
plot_sens(x, w, problem, M, grad_fun)

# test maximin
theta_grid = matrix(
  c(0.8, log(.01/.99), log(1.5), log(2.5),
    0.7, log(.01/.99), log(1.5), log(2.5),
    0.6, log(.01/.99), log(1.5), log(2.5),
    0.5, log(.01/.99), log(1.5), log(2.5)
    ),
  4, 4, byrow = T
)
result_maximin = maximin(
  grad_fun = grad_fun,
  obj_fun = obj_fun,
  bound = bound,
  pts = pts,
  numPop = 100,
  iter = 500,
  theta_grid,
  alg = "BHO"
  )
# -15.57
result_maximin$optimumValue

# CSO, ABC, GBS don't work
algorithms_list = c(
  "PSO",
  "ALO",
  "GWO",
  "DA",
  "FFA",
  "GA",
  "GOA",
  "HS",
  "MFO",
  "SCA",
  "WOA",
  "CLONALG",
  "DE",
  "SFL",
  "KH",
  "BA"
)

# plot comparing performance of different algorithms
set.seed(2023)
obj_vals = c()
times = c()
replicates = 50
for (i in 1:length(algorithms_list)) {
  algorithm = algorithms_list[i]
  for (j in 1:replicates) {
    cat(algorithm, ": ", j, "/", replicates, "\n")
    result = maximin(
      grad_fun = grad_fun,
      obj_fun = obj_fun,
      bound = bound,
      pts = pts,
      numPop = 50,
      iter = 500,
      theta_grid,
      alg = algorithm
    )
    obj_vals = c(obj_vals, result$optimumValue)
    times = c(times, result$timeElapsed[1])

  }
}

df = data.frame(
  obj = obj_vals,
  time = times,
  algorithm = rep(algorithms_list, each = replicates)
)

library(ggplot2)
library(dplyr)
df %>%
  ggplot(aes(x = algorithm, y = obj)) +
  geom_point()

df %>%
  filter(algorithm %in% c("ALO", "CLONALALG", "DE", "GOA", "GWO", "HS", "PSO")) %>%
  ggplot(aes(x = algorithm, y = obj)) +
  geom_point()

df %>%
  filter(algorithm %in% c("ALO", "PSO", "DE")) %>%
  ggplot(aes(x = algorithm, y = obj)) +
  geom_point()
# PSO is best
