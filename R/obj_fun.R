# objective functions for information matrices

# M: an information matrix created from functions in info_matrices.R

obj.D = function(M) {
  suppressWarnings(log(det(M)))
}
