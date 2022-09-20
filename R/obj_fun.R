# objective functions for information matrices

# M: an information matrix created from functions in info_matrices.R

# D optimality
obj.D = function(M) {
  suppressWarnings(log(det(M)))
}
