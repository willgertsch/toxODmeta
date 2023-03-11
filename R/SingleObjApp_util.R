# utility functions for single objective app

# function that displays latex formulas for models in app
model_display = function(model) {

  if (model == "Dichotomous Hill")
    "$$ P(d) = \\theta_1 + \\frac{(\\theta_2 - \\theta_2 \\theta_1)}{1 + \\exp(-\\theta_3 - \\theta_4\\log (d))} $$"
  # else if (model == "Gamma") # don't know how to do this => Elvis' paper reparameterizes
  #   "$$ P(d) = $$"
  else if (model == "Logistic")
    "$$ P(d) = \\frac{1}{1 + \\exp(-\\theta_1 - \\theta_2 d)} $$"
  else if (model == "Logistic quadratic")
    "$$P(d) = \\frac{1}{1 + \\exp(-\\theta_1 - \\theta_2 d - \\theta_3 d^2)}$$"
  else if (model == "Logistic cubic")
    "$$P(d) = \\frac{1}{1 + \\exp(-\\theta_1 - \\theta_2 d - \\theta_3 d^2 - \\theta_4 d^3)}$$"
  else if (model == "Logistic fractional polynomial")
    "$$P(d) = \\frac{1}{1 + \\exp(-\\theta_1 - \\theta_2 d^{\\theta_4} - \\theta_3 d^{\\theta_5})}$$"
  else if (model == "Log-logistic")
    "$$ P(d) = \\theta_1 +  \\frac{1-\\theta_1}{1 + \\exp(-\\theta_1 - \\theta_2 \\log d)}$$"
  else if (model == "Log-probit")
    "$$ P(d) = \\theta_1 + (1 - \\theta_1) \\Phi(\\theta_2 + \\theta_3 \\log(d))$$"
  else if (model == "Multistage degree 1")
    "$$P(d) = \\theta_1 + (1 - \\theta_1)(1 - \\exp(-\\theta_2 d))$$"
  else if (model == "Multistage degree 2")
    "$$P(d) = \\theta_1 + (1 - \\theta_1)(1 - \\exp(-\\theta_2 d - \\theta_3 d^2))$$"
  else if (model == "Multistage degree 3")
    "$$P(d) = \\theta_1 + (1 - \\theta_1)(1 - \\exp(-\\theta_2 d - \\theta_3 d^2 - \\theta_4 d^3))$$"
  else if (model == "Probit")
    "$$P(d)=\\Phi(\\theta_1 + \\theta_2 d)$$"
  else if (model == "Quantal linear")
    "$$P(d) = \\theta_1 + (1-\\theta_1)(1-\\exp(-\\theta_2 d))$$"
  else if (model == "Weibull")
    "$$P(d) = \\theta_1 + (1-\\theta_1)(1-\\exp(-\\theta_2 d^\\theta_3))$$"
  else if (model == "Mixture multistage")
    "$$P(d) = \\theta_6 \\left[1 - \\exp(-\\theta_1-\\theta_2 d - \\theta_3 d^2) \\right] + (1-\\theta_6)\\left[1 - \\exp(-\\theta_1 - \\theta_4 d - \\theta_5 d^2) \\right]$$"
  else if (model == "Box-Cox Weibull")
    "$$P(d)=1-\\exp \\left[ -\\exp \\left(\\theta_1 + \\theta_2 \\frac{d^{\\theta_3}-1}{\\theta_3}\\right)\\right]$$"
  else
    "Model not supported"

}

# convert raw text input to a vector of parameter values
# pulling this out into its own function because input checking could be complex
# useful in multiple places where there is text input
process_theta = function(text) {
  as.numeric(strsplit(text, ",")[[1]])
}

