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


