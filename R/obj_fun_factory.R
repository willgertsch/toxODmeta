# function for constucting objective function for use with algorithms
# M_fun: information function
# obj_fun: objective function of information matrix
# theta: parameter values to pass through to M_fun
construct = function(M_fun, obj_fun, theta) {

  # these are used in interface function
  force(M_fun)
  force(theta)

  # interface called by optimization software
  # return this function
  function(vars, ...) {
    # distinguish between points and weights
    pts = length(vars)/2
    x = vars[1:pts]
    w = vars[(pts+1):(2*pts)]

    # check weight constraint
    s = sum(w, na.rm = T) # na.rm needed to fix if statement error
    if (s > 1) # constraint implementation
      return(-Inf)

    #obj_value = suppressWarnings(log(det(M_fun(x, w, theta))))
    obj_value = obj_fun(M_fun(x, w, theta))

    # deal with missing
    if (is.na(obj_value))
      return(-Inf)
    else
      return(obj_value)
  }

}
