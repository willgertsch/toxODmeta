# functions that don't belong anywhere else

# checks if information matrix is invertible
# returns 1 if invertible and 0 if not
# can optimize easily for 2 dim
checkMinv = function(M) {

  if (class(try(solve(M),silent=T))[1]!="matrix")
    return(0)
  else
    return(1)
}
