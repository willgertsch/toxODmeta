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

grad.logistic.cubic = function(x, theta) {
  eta = theta[1] + theta[2] * x + theta[3] * x^2 + theta[4] * x^3
  sigma = exp(eta)/(1 + exp(eta))^2
  grad = sigma * c(1, x, x^2, x^3)
  return(grad)
}

# 2nd degree fractional polynomial predictor
grad.logistic.fp = function(x, theta) {

  # theta4 and theta5 are power paramters in this model
  powers = c(0, theta[4], theta[5])

  # x1 is the 2nd term in the polynomial
  x1 = H(2, x, powers)
  x2 = H(3, x, powers)
  eta = theta[1] + theta[2] * x1 + theta[3] * x2
  sigma = exp(eta)/(1 + exp(eta))^2
  grad = sigma * c(1, x1, x2)
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
