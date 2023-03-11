# multi objective design using two methods
# based off example 2.1 from Tox book

# data
# source: Matoni et al (1986)
df = data.frame(
  dose = c(0, 100, 300, 600),
  number_alive = c(134, 130, 130, 129),
  number_cancer = c(9, 13, 14, 15),
  prop_cancer = c(0.067, 0.1, 0.108, 0.116),
  intercept = rep(1, 4)
)

plot(df$prop_cancer ~ df$dose)

# fit multistage models up to degree 3
# P(d) = g + (1-g)*(1-exp())
# used EPA's online BMDS tool
mod1_coef = list(g = 0.07929, b1 = 8.147e-5)
mod2_coef = list(g = 0.07929, b1 = 8.147e-5, b2 = 0)
mod3_coef = list() # problem

