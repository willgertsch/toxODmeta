# objective functions for information matrices

# M: an information matrix created from functions in info_matrices.R

# D optimality
# maximize logdetM
obj.D = function(M, param) {
  suppressWarnings(log(det(M)))
}

# A optimality
# minimize trM^-1
obj.A = function(M, param) {

  # check if matrix is invertible
  if (!checkMinv(M))
    return(-Inf)
  else
    return(-sum(diag(solve(M))))
}

# c optimality
# c is a vector of constants
# many objectives reduce to c-optimality
obj.c = function(M, param) {

  c = param

  # ensure matrix is invertible
  if (!checkMinv(M))
    return(-Inf)
  else
    return(-t(c) %*% solve(M, c))

}

# objective for estimating additional risk
# Var(P(d_1) - P(d_0))
# special case of c-optimality
# param contains the gradient values evaluated at the two doses
# assuming param is a vector of length 2p, where p is the number of paramters
# obj.addrisk = function(M, param) {
#
#   c = param
#   return(obj.c(M, c))
# }
