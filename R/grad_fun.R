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

# 4 parameter log-logistic function
# drc package version
grad.loglogistic4 = function(x, theta) {

  b = theta[1]
  c = theta[2]
  d = theta[3]
  y = theta[4] # e in the drc package notation

  eta = exp(b*(log(x) - log(y)))
  db = (d - c) * (log(x) - log(y)) * eta/(eta + 1)^2
  dc = (x^b) / (x^b + y^b)
  dd = (y^b) / (x^b + y^b)
  dy = b * x^b * y^(b - 1) * (d-c) / (x^b + y^b)^2
  grad = c(db, dc, dd, dy)
  return(grad)
}

# 5 parameter log-logistic function from drc package
grad.loglogistic5 = function(x, theta) {
  eta = theta[1] * (log(x) - log(theta[4]))

  # individual gradient components
  d1 = (theta[2] - theta[3]) * theta[5] * (1 + exp(theta))^(-theta[5] - 1) *
    exp(eta) * (log(x) - log(theta[4]))

  d2 = 1 - (1 + exp(eta))^(-theta[5])

  d3 = (1 + exp(theta))^(-theta[5])

  d4 = (theta[3] - theta[2]) * theta[5] * (1 + exp(eta))^(-theta[5] - 1) *
    exp(eta) * (theta[1]/theta[4])

  d5 = (theta[2] - theta[3]) * (1 + exp(eta))^(-theta[5]) * log(1 + exp(eta))

  grad = c(d1, d2, d3, d4, d5)
  return(grad)

}
