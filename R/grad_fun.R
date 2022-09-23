# gradient functions to be used in information matrices and elsewhere
# x: independent variable, scalar
# theta: parameter vector
# gradient is with respect to theta
grad.logistic = function(x, theta) {

  eta = theta[1] + theta[2] * x
  sigma = exp(eta)/(1 + exp(eta))^2
  grad = sigma * c(1, x)
  return(grad)
}

grad.logistic.quad = function(x, theta) {

  eta = theta[1] + theta[2] * x + theta[3] * x^2
  sigma = exp(eta)/(1 + exp(eta))^2
  grad = sigma * c(1, x, x^2)
  return(grad)
}

grad.loglogistic = function(x, theta) {

  eta = theta[1] + theta[2] * log(x)
  sigma = exp(eta)/(1 + exp(eta))^2
  g1 = (1 - theta[3]) * sigma
  g2 = (1 - theta[3]) * sigma * log(x)
  g3 = 1 - 1/(1 + exp(-eta))
  grad = c(g1, g2, g3)
  return(grad)
}

grad.exponential = function(x, theta) {

  eta = theta[1] + theta[2] * x
  sigma = exp(-eta)
  grad = sigma * c(1, x)
  return(grad)
}

grad.weibull = function(x, theta) {

  eta = theta[1] + theta[2] * x ^ theta[3]
  sigma = exp(-eta)
  grad = sigma * c(1, x^theta[3], x^theta[3] * log(x))
  return(grad)
}