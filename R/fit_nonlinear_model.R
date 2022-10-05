# function for fitting nonlinear models
# y: number of successes out of 100
# x: independent variable
# model: model type
# primarily uses drm() from drc package
fit_nonlinear_model = function(successes, x, model) {

  y = successes/100

  # data
  d = data.frame(
    y = y,
    x = x,
    successes = successes,
    n = rep(100, length(x))
  )

  # select model formula
  # should the error term for the glms be Gaussian?
  if (model == "logistic") {
    mod = glm(
      cbind(successes, n) ~ x,
      data = d,
      family = binomial(link = "logit")
    )
    yhat = predict(mod, type = "response")
    theta = coef(mod)
  }
  else if (model == "logistic-quadratic") {
    mod = glm(
      cbind(successes, n) ~ x + I(x^2),
      data = d,
      family = binomial(link = "logit")
    )
    yhat = predict(mod, type = "response")
    theta = coef(mod)
  }
  else if (model == "logistic-cubic") {
    mod = glm(
      cbind(successes, n) ~ x + I(x^2) + I(x^3),
      data = d,
      family = binomial(link = "logit")
    )
    yhat = predict(mod, type = "response")
    theta = coef(mod)
  }
  else if (model == "loglogistic") {
    mod = drc::drm(
      formula = y ~ x,
      data = d,
      fct = drc::LL2.3u()
    )
    yhat = predict(mod)
    param = coef(mod) # different parameterization
    theta0 = param[1] * param[3]
    theta1 = -param[1]
    theta2 = param[2]
    theta = c(theta0, theta1, theta2)
  }
  else if (model == "exponential") {
    # special case of multistage model
    mod = drc::drm(
      formula = y ~ x,
      data = d,
      fct = drc::multi2(fixed = c(NA, NA, 0, 1, 0))
    )
    yhat = predict(mod)
    theta = coef(mod)
  }
  else if (model == "weibull") {

    # trick for this one is that it is expressed in terms of baseline dose


  }
  else
    stop("Unknown model")


  # collect output
  out = list()
  out$yhat = yhat
  out$theta = theta
  return(out)
}
