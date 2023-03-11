# function for computing design efficiencies
compute_eff = function(
    model,
    theta,
    objective,
    d1,
    d2,
    w1,
    w2
) {

  # select gradient function
  if (model == "Logistic")
    grad_fun = grad.logistic
  else if (model == "Logistic quadratic")
    grad_fun = grad.logistic.quad
  else if (model == "Logistic cubic")
    grad_fun = grad.logistic.cubic
  else if (model == "Logistic fractional polynomial")
    grad_fun = grad.logistic.fp
  else if (model == "Mixture multistage")
    grad_fun = grad.mix2
  else if (model == "Box-Cox Weibull")
    grad_fun = grad.boxcoxweibull

  if (objective == "D")
    obj_fun = obj.D
  else if (objective == "A")
    obj_fun = obj.A

  # define objective function
  param = c()
  obj_fun_M = obj_fun_factory(grad_fun, obj_fun, theta, param)

  # compute and return efficiencies
  if (objective == "D")
    (exp(obj_fun_M(c(d1, w1)))/exp(obj_fun_M(c(d2, w2))))^(1/length(theta))
  else if (objective == "A")
    obj_fun_M(c(d2, w2))/obj_fun_M(c(d1, w1))
}
