# sensitivity functions
# z: independent variable
# M: pre-computed information matrix for design
# theta: parameter vector
sens.logistic.D = function(z, M, theta) {

  # check if M is invertible
  if (class(try(solve(M),silent=T))[1]!="matrix") {
    y = 1
  }
  else {
    Minv = solve(M)
    b = c(1, z)
    p = nrow(M)
    etaz = theta[1] + theta[2] * z
    sigmaz = exp(etaz)/(1+exp(etaz))^2
    y = sigmaz * t(b) %*% Minv %*% b - p
  }

  return(y)

}

# sensitivity function for quadratic predictor
sens.logistic.quad.D = function(z, M, theta) {

  # check if M is invertible
  if (class(try(solve(M),silent=T))[1]!="matrix") {
    y = 1
  }
  else {
    Minv = solve(M)
    b = c(1, z, z^2)
    p = nrow(M)
    etaz = theta[1] + theta[2] * z + theta[3] * z^2
    sigmaz = exp(etaz)/(1+exp(etaz))^2
    y = sigmaz * t(b) %*% Minv %*% b - p
  }

  return(y)

}
