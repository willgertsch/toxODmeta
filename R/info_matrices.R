# information matrix functions

# information matrix
# x: array of design points
# w: array of weights
# theta: array of parameter values

# logistic regression model with linear predictor
# P(d) = [1 + exp(-b0 - b1*d)]^-1
M.logistic = function(x, w, theta) {

  # predictor
  eta = theta[1] + theta[2] * x

  # weight function
  sigma = exp(eta)/(1+exp(eta))^2

  # information matrix
  IM = 0
  for (i in 1:length(x)) {

    m12 = x[i]

    IM_i = w[i] * sigma[i] * matrix(c(
      1, m12,
      m12, x[i]^2
    ), ncol=2)

    IM = IM + IM_i

  }

  # return
  IM
}

# logistic regression model with quadratic predictor
# P(d) = [1 + exp(-b0 - b1*d - b2*d^2)]^-1
M.logistic.quad = function(x, w, theta) {

  # x transformations
  x1 = x
  x2 = x^2

  # predictor
  eta = theta[1] + theta[2] * x1 + theta[3] * x2

  # weight function
  sigma = exp(eta)/(1+exp(eta))^2

  # information matrix
  IM = 0
  for (i in 1:length(x)) {

    m12 = x1[i]
    m13 = x2[i]
    m23 = x1[i]*x2[i]

    IM_i = w[i] * sigma[i] * matrix(c(
      1, m12, m13,
      m12, x1[i]^2, m23,
      m13, m23, x2[i]^2
    ), ncol=3)

    IM = IM + IM_i

  }

  # return
  IM
}


# exponential regression with linear predictor
# P(d) = 1 - exp[-(b0 + b1 * d)]
M.exponential = function(x, w, theta) {

  # predictor
  eta = theta[1] + theta[2] * x

  # nonlinear weight function
  sigma = exp(-eta)

  # information matrix
  IM = 0
  for (i in 1:length(x)) {

    m12 = x[i]

    IM_i = w[i] * sigma[i] * matrix(c(
      1, m12,
      m12, x[i]^2
    ), ncol=2)

    IM = IM + IM_i

  }

  # return
  IM
}




