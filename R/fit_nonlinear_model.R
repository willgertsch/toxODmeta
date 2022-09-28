# function for fitting nonlinear models
# y: number of successes out of 100
# x: independent variable
# model: model type
fit_nonlinear_model = function(y, x, model) {

  y = y/100

  # data
  d = data.frame(
    y = y,
    x = x
  )

  # select model formula
  if (model == "logistic") {
    f = y ~ 1/(1 + exp(theta0 + theta1 * x))
  }
  else if (model == "logistic-quadratic") {
    f = y ~ 1/(1 + exp(-(theta0 + theta1 * x + theta2 * x)))
  }
  else if (model == "loglogistic") {
    f = y ~ theta2 + (1 - theta2)/(1 + exp(-(theta0 + theta1 * log(x))))
  }
  else if (model == "exponential") {
    f = y ~ 1 - exp(-(theta0 + theta1*x))
  }
  else if (model == "weibull") {
    f = y ~ 1 - exp(-(theta0 + theta1*x^theta2))
  }
  else
    stop("Unknown model")

  mod = nls(
    f,
    data = d
  )

  # collect output
  out = list()
  out$yhat = predict(mod)
  out$theta = coef(mod)
  return(out)
}
