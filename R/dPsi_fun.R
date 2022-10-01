# derivatives of objective functions with respect to information matrix
# matrix singularity is already checked here
# M: information matrix
dPsi.D = function(M, param) {
  Minv = solve(M)
  return(Minv)
}

dPsi.A = function(M, param) {
  Minv = solve(M)
  Minv2 = Minv %*% Minv
  return(Minv2)
}

dPsi.c = function(M, c) {

  Minv = solve(M)
  return(Minv %*% c %*% t(c) %*% Minv)
}
