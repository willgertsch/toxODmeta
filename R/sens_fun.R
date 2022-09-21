# sensitivity functions
# z: independent variable
# Minv: pre-computed inverse information matrix for design
# theta: parameter vector
sens.logistic.D = function(z, Minv, theta) {

  b = c(1, z)
  p = nrow(Minv)
  etaz = theta[1] + theta[2] * z
  sigmaz = exp(etaz)/(1+exp(etaz))^2
  y = sigmaz * t(b) %*% Minv %*% b - p

  return(y)

}

sens.logistic.A = function(z, Minv, theta) {

  Minv2 = Minv %*% Minv
  b = c(1, z)
  etaz = theta[1] + theta[2] * z
  sigmaz = exp(etaz)/(1+exp(etaz))^2
  y = sigmaz * t(b) %*% Minv2 %*% b - sum(diag(Minv))
  return(y)
}

# sensitivity function for quadratic predictor
sens.logistic.quad.D = function(z, Minv, theta) {

  b = c(1, z, z^2)
  p = nrow(Minv)
  etaz = theta[1] + theta[2] * z + theta[3] * z^2
  sigmaz = exp(etaz)/(1+exp(etaz))^2
  y = sigmaz * t(b) %*% Minv %*% b - p

  return(y)

}

sens.logistic.quad.A = function(z, Minv, theta) {
  Minv2 = Minv %*% Minv
  b = c(1, z, z^2)
  etaz = theta[1] + theta[2] * z + theta[3] * z^2
  sigmaz = exp(etaz)/(1+exp(etaz))^2
  y = sigmaz * t(b) %*% Minv2 %*% b - sum(diag(Minv))
  return(y)
}

# sensitivity function for exponential
sens.exponential.D = function(z, Minv, theta) {

  b = c(1, z)
  p = nrow(Minv)
  etaz = theta[1] + theta[2] * z
  sigmaz = exp(-etaz)
  y = sigmaz * t(b) %*% Minv %*% b - p

  return(y)
}

sens.exponential.A = function(z, Minv, theta) {
  Minv2 = Minv %*% Minv
  b = c(1, z)
  etaz = theta[1] + theta[2] * z
  sigmaz = exp(-etaz)
  y = sigmaz * t(b) %*% Minv2 %*% b - sum(diag(Minv))
  return(y)
}

# sensitivity function for Weibull
sens.weibull.D = function(z, Minv, theta) {

  logz = suppressWarnings(log(z))
  z1 = z^theta[3]
  z2 = z1 * logz
  b = c(1, z1, z2)
  etaz = theta[1] + theta[2] * z ^ theta[3]
  sigmaz = exp(-etaz)^2
  p = nrow(Minv)
  y = sigmaz * t(b) %*% Minv %*% b - p
  return(y)
}

sens.weibull.A = function(z, Minv, theta) {
  Minv2 = Minv %*% Minv
  logz = suppressWarnings(log(z))
  z1 = z^theta[3]
  z2 = z1 * logz
  b = c(1, z1, z2)
  etaz = theta[1] + theta[2] * z ^ theta[3]
  sigmaz = exp(-etaz)^2
  y = sigmaz * t(b) %*% Minv2 %*% b - sum(diag(Minv))
  return(y)

}

# sensitivity function for loglogistic
sens.loglogistic.D = function(z, Minv, theta) {

  logz = suppressWarnings(log(z))
  etaz = theta[1] + theta[2] * logz
  sigmaz = exp(etaz)/(1 + exp(etaz))^2
  loglogitz = 1/(1+exp(-etaz))

  z1 = (theta[3] - 1) * sigmaz
  z2 = (theta[3] - 1) * sigmaz * logz
  z3 = 1 - loglogitz
  b = c(z1, z2, z3)

  p = nrow(Minv)
  y = t(b) %*% Minv %*% b - p

  return(y)
}

sens.loglogistic.A = function(z, Minv, theta) {
  Minv2 = Minv %*% Minv
  logz = suppressWarnings(log(z))
  etaz = theta[1] + theta[2] * logz
  sigmaz = exp(etaz)/(1 + exp(etaz))^2
  loglogitz = 1/(1+exp(-etaz))

  z1 = (theta[3] - 1) * sigmaz
  z2 = (theta[3] - 1) * sigmaz * logz
  z3 = 1 - loglogitz
  b = c(z1, z2, z3)
  y = sigmaz * t(b) %*% Minv2 %*% b - sum(diag(Minv))
  return(y)
}
