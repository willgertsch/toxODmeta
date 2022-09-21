# objective functions for information matrices

# M: an information matrix created from functions in info_matrices.R

# D optimality
# maximize logdetM
obj.D = function(M) {
  suppressWarnings(log(det(M)))
}

# A optimality
# minimize trM^-1
obj.A = function(M) {

  # check if matrix is invertible
  if (!checkMinv(M))
    return(-Inf)
  else
    return(-sum(diag(solve(M))))
}
