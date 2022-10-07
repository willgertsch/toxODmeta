# plot sensitivity function for a given design
# problem is same list from toxODmeta
# x, w are design point and weight vectors
# M: pre-computed information matrix
# grad_fun: gradient function
plot_sens = function(x, w, problem, M, grad_fun) {

  # x values
  step = problem$bound/1000
  xvals = seq(0, problem$bound, step)

  # select derivative function for sensitivity function
  if (problem$obj == "D") {
    dPsi = dPsi.D
    param = NULL
  }
  else if (problem$obj == "A") {
    dPsi = dPsi.A
    param = NULL
  }
  else if (problem$obj == "addrisk") {

    dPsi = dPsi.c
    # compute c vector
    d1 = problem$d1
    d0 = problem$d0
    theta = problem$theta
    param = grad_fun(d1, theta) - grad_fun(d0, theta)
  }
  else {
    # expand this to handle solving design problems with no verification
    #stop("No derivative specified for this objective.")
    # use y=2 to denote missing derivative function
    yvals = rep(2, length(xvals))
  }

  # compute sensitivity function
  # check first if matrix is invertible and then invert
  if (!checkMinv(M)) {
    # using y=1 to denote matrix singularity
    yvals = rep(1, length(xvals))
  }
  else {
    Minv = solve(M)
    yvals = sapply(xvals, sens, grad_fun, dPsi, M, problem$theta, param)
  }



  # plot
  # display message if missing matrix deriv or singular matrix
  if (sum(yvals - 1, na.rm = T) == 0) {
    p = ggplot2::ggplot(mapping = ggplot2::aes(y = yvals, x = xvals)) +
      ggplot2::geom_line(color = "blue") +
      ggplot2::geom_hline(yintercept = 0) +
      ggplot2::theme_bw() +
      ggplot2::labs(title = "Equivalence Theorem Check") +
      ggplot2::xlab("x") +
      ggplot2::ylab("ch(x)") +
      ggplot2::annotate("text", x = mean(xvals), y = 0.5,
                       label = "Singular information matrix", size = 5)
  }
  else if (sum(yvals - 2, na.rm = T) == 0) {
    p = ggplot2::ggplot(mapping = ggplot2::aes(y = yvals, x = xvals)) +
      ggplot2::geom_line(color = "blue") +
      ggplot2::geom_hline(yintercept = 0) +
      ggplot2::theme_bw() +
      ggplot2::labs(title = "Equivalence Theorem Check") +
      ggplot2::xlab("x") +
      ggplot2::ylab("ch(x)") +
      ggplot2::annotate("text", x = mean(xvals), y = 0.5,
                       label = "No dPsi defined", size = 5)
  }
  else {
    p = ggplot2::ggplot(mapping = ggplot2::aes(y = yvals, x = xvals)) +
      ggplot2::geom_line(color = "blue") +
      ggplot2::geom_hline(yintercept = 0) +
      #ggplot2::geom_point(aes(x = design_points, y = pts_ch), col = "red", size = 3) +
      ggplot2::geom_vline(xintercept = x, color = "red", linetype = "dashed") +
      ggplot2::theme_bw() +
      ggplot2::labs(title = "Equivalence Theorem Check") +
      ggplot2::xlab("x") +
      ggplot2::ylab("ch(x)")
  }


  return(p)
}
