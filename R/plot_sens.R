# plot sensitivity function for a given design
# problem is same list from toxODmeta
# x, w are design point and weight vectors
# M: pre-computed information matrix
# sens_fun is a function from sens_fun.R
plot_sens = function(x, w, problem, M) {

  # x values
  step = problem$bound/1000
  xvals = seq(0, problem$bound, step)

  # select sensitivity function
  if (problem$obj == "D" & problem$model == "logistic") {
    sens_fun = sens.logistic.D
  }
  else if (problem$obj == "D" & problem$model == "logistic-quadratic") {
    sens_fun = sens.logistic.quad.D
  }
  else if (problem$obj == "D" & problem$model == "exponential") {
    sens_fun = sens.exponential.D
  }
  else if (problem$obj == "D" & problem$model == "weibull") {
    sens_fun = sens.weibull.D
  }
  else if (problem$obj == "D" & problem$model == "loglogistic") {
    sens_fun = sens.loglogistic.D
  }
  else {
    stop("Sensitivity function does not exist for problem")
  }

  # compute sensitivity function
  yvals = sapply(xvals, sens_fun, M, problem$theta)

  # plot
  p = ggplot2::ggplot(mapping = ggplot2::aes(y = yvals, x = xvals)) +
    ggplot2::geom_line(color = "blue") +
    ggplot2::geom_hline(yintercept = 0) +
    #ggplot2::geom_point(aes(x = design_points, y = pts_ch), col = "red", size = 3) +
    ggplot2::geom_vline(xintercept = x, color = "red", linetype = "dashed") +
    ggplot2::theme_bw() +
    ggplot2::labs(title = "Equivalence Theorem Check") +
    ggplot2::xlab("x") +
    ggplot2::ylab("ch(x)")

  return(p)
}
