# shiny app for single objective
SingleObjApp = function(...) {

  # using EPA BMDS as source for models
  models = c(
    #"Dichotomous Hill",
    #"Gamma",
    "Logistic",
    "Logistic quadratic",
    "Logistic cubic",
    "Logistic fractional polynomial",
    "Mixture multistage",
    "Box-Cox Weibull"
    #"Log-logistic",
    #"Log-probit",
    #"Multistage degree 1",
    #"Multistage degree 2",
    #"Multistage degree 3",
    #"Probit",
    #"Quantal linear",
    #"Weibull"
  )

  objectives = c(
    "D",
    "A"
    #"E"
  )

  algorithms = c(
    "PSO",
    "DE",
    "GWO",
    "HS"
  )

  ui = fixedPage(
    withMathJax(),
    tabsetPanel(
      selected = "Find optimal designs",
      type = "tabs",
      id = "toptabpanel",
      tabPanel(
        "User manual",
        tags$h3(
          "Overview",
          style = "text-align:left;"
        ),
        tags$p(
          "This app allows the user to find optimal experimental designs for several
          nonlinear models used in toxicology. It also provides a tool for
          comparing the efficiency of any two designs. This page explains how to use
          the app and the optimal design theory used to generate and interpret the results."
        ),
        tags$p(
          "This app uses metaheuristic algorithms, which are a class of
          optimization methods that have been increasingly used in recent years
          to solve complex problems that are difficult to solve using standard
          methods. They are inspired by natural phenomena and simulate the
          behavior of a group of entities, such as animals, insects, or particles,
          in search of an optimal solution. Some examples of metaheuristic algorithms
          are Particle Swarm Optimization (PSO), Differential Evolution (DE),
          Harmony Search (HS), and Grey Wolf Optimizer (GWO). These algorithms have
          shown great success in solving a wide range of problems, including engineering,
          economics, logistics, and data analysis. They are especially useful in optimal
          design of experiments because they can easily be applied to a wide variety
          of design problems."
        ),
        tags$h3(
          "The design tab",
          style = "text-align:left;"
        ),
        tags$p(
          "The design tab allows the user to use metaheuristic algorithms to find
          the optimal design for a selected nonlinear model. The sidebar allows
          the user to set several algorithm options. The algorithm selector chooses
          metaheuristic to use to find the design and the iterations and swarm size
          options tell the algorithm how long to run and the diversity of the search.
          Increasing both the number of iterations and the swarm size will make it
          more likely that the optimal design will be found but increases the run
          time. The dose limit option contols the highest dose allowed.
          The limit should be chosen based on background knowledge of the
          experiment. A lower dose limit will limit the search space and make the
          optimal design easier to find. The design points option controls how
          many doses will be allowed in the optimal design. If too few doses are
          specified, then the optimal design might be impossible to find. A general
          rule of thumb is that the number of doses in the optimal design should
          be the same as the number of parameters in the model."
        ),
        tags$p(
          "The main pane alloes the user to select the model, parameter values,
          and the design objective. Theta values should be entered separated by
          commas. Pressing the find design button will run the selected algorithm
          options on the design problem, display the final design, and display a
          graphical check of optimality.
          If plotted function is less than 0 with equality at the doses in the design,
          then the optimal design has been found. If the design is not optimal,
          try rerunning with more iterations and a larger swarm size. A different
          algorithm might also help to find the optimal design. If the plot displays
          a message about a singular information matrix, then there is likely a
          problem with how the design problem is specified. The most common issue
          is that the number of doses is too few. Another common issue is that
          the dose range may be too small for the model in question."
        ),
        tags$h3(
          "Compare tab",
          style = "text-align:left;"
        ),
        tags$p(
          "The compare tab allows the user to compare two designs to see which
          is more efficient. This is especially useful when the goal is to compare
          a naive or more practical design against the optimal design.
          Design 1 is compared relative to design design 2.
          If design 1 is less efficient, then the efficiency will be less than 1.
          If design 1 is more efficient, then the efficiency will be greater than 1.
          Parameter values, dose levels, and weights should be entered as comma
          separated values."
        ),
        tags$h3(
          "Background",
          style = "text-align:left;"
        ),
        tags$p(
          "
          Suppose \\(Y\\) is an outcome variable of interest and \\(d\\) is the dosage or
          concentration of the substance under investigation. For this work, we
          additionally assumed that \\(Y\\) is a binary variable that is equal to one
          if toxicity has occured and equal to 0 otherwise. We also assume the
          that the dose \\(d\\) is in the range \\([0, D]\\) where \\(D\\) is highest
          experimentally feasible dose. The goal of a dose response experiment
          is to establish a mathematical relationship of the form
$$
P(Y = 1| d) = P(d) = f(d, \\theta) + \\epsilon
$$
where \\(f\\) is a possibly nonlinear function of the dose and parameters \\(\\theta\\).
The error term \\(\\epsilon\\) is most commonly assumed to have a normal distribution,
\\(N(0, \\sigma^2)\\). Common forms of \\(f\\) include the exponential and inverse logit functions.

The goal of optimal design is to choose good values for the doses included in the
experiment and the number of subjects to assign to each dose. Let \\(d_i\\) denote the
ith dose in the design and let \\(n_i\\) be the number of subjects assigned to the ith
dose for \\(i = 1, \\dots, k\\).  For generalizabillity and theoretical convenience,
we assume a fixed sample size \\(N\\) and work with the proportions assigned to each
dose instead of the raw \\(n_i\\). Define \\(w_i\\) as \\(w_i = n_i/N\\) such that \\(\\sum_i^k w_i = 1\\).
Using this weight-based notation, we can define a \\(k\\) dose design as a probability measure on \\([0, D]\\).
$$
\\xi = \\begin{pmatrix}
d_1, & \\dots, & d_k\\\
w_1, & \\dots, & w_k
\\end{pmatrix}
$$

The design \\(\\xi\\) is a compact representation of all the parameters that must be
chosen optimally in order to maximize some objective function.
The most common objective functions in optimal design are based on the model information matrix,
\\(M(\\xi, \\theta)\\). This information matrix can be thought of the potential observed
information matrix if the experiment is run using the design \\(\\xi\\). Note that \\(M(\\xi, \\theta)\\)
also depends on the parameter values of the model, meaning that a prior value for
the parameters must be supplied. This value can come from previous data or theoretical
models. The designs generated are referred to as \\textit{locally optimal} with
respect to the prior \\(\\theta\\).

For the nonlinear models under consideration, the information matrix can be written as
$$
M = M(\\xi, \\theta) = \\sum_{i=1}^k w_i M_i(\\xi, \\theta) = \\sum_{i=1}^k w_i \\nabla f(d_i, \\theta) (\\nabla f(d_i, \\theta))'
$$
Note that this method of writing the information matrix decomposes the information
into the sum of the information contributed at each dose level.
The last equality says that the information matrix is a function of the gradient
with respect to the model parameters. This result follows from a linearization of
the nonlinear model function using the Delta method."
        ),
        tags$p(
          "Objective functions for optimal design are based around minimizing the
          variance estimated quantities or, equivalently, maximizing information
          gain of the experiment. For example, a common design strategy is to maximize
$$
\\Psi_D(M) = \\log |M|
$$
Maximizing \\(\\Psi_D\\) is equivalent to minimizing the size of the confidence
region of the parameter estimates and a design that minimizes \\(\\Psi_D\\) is called
D-optimal. Another approach is to minimize the objective function
$$
\\Psi_A(M) = \\operatorname{tr} M^{-1}
$$
which is equivalent to minimizing the sum of the variances of the parameter estimates.
Another useful objective function is
$$
\\Psi_c(M) = c'M^{-1}c
$$
which is minimized in order to minimize the variance of some linear combination
of the parameters. The reason why \\(\\Psi_c\\) is so useful is because it can be
applied to construct a wide variety of domain specific objectives. Suppose \\(g(d, \\theta)\\)
is some quantity of interest to be estimated. The variance of the estimated \\(g(d, \\theta)\\)
can be approximated using the Delta method as
$$
\\operatorname{Var}(g(d, \\hat\\theta)) = \\nabla g(d, \\hat\\theta)' M^{-1} \\nabla g(d, \\hat \\theta)
$$
This means that for an arbitrary function \\(g\\), an objective function for finding
the relevant optimal design can be found using the framework of c-optimality.

 In order to make comparisons between different designs, it is useful to consider
 efficiency relative to the optimal design. For example, we may wish to see how a
 naive design compares to the optimal design. In the case of D-optimality,
 let \\(\\xi_D\\) be the D-optimal design. The D-efficiency of a design \\(\\xi\\) is defined as
 $$
 \\operatorname{eff}_D(\\xi) = \\left(\\frac{|\\Psi_D(\\xi)|}{|\\Psi_D(\\xi_D)|}\\right)^{1/p}
 $$
 where \\(p\\) is the number of parameters in the model. Similar efficiency functions
 may be derived for the other optimality criteria.

 A final useful tool optimal design is the equivalence theorem of Kiefer (1960).
 This theorem says that if a design \\(\\xi\\) is optimal, then the directional
 derivative of the design criterion evaluated at \\(\\xi\\) must satisfy an inequality
 for all dose values in the design space with equality attained when the dose is
 in the design. This provides an easy graphical test to check if the design is
 optimal. These inequalities are well known for the most common design criteria and
 can easily be derived using matrix calculus for more uncommon criteria.
"
        )
      ),
      tabPanel(
        "Find optimal designs",

        tabsetPanel(
          selected = "Design",
          type = "pills",
          id = "designtabpanel",
          tabPanel(
            "Design",
            # sidebar layout for algorithm options
            sidebarLayout(
              sidebarPanel(
                "Algorithm options",
                selectInput(
                  "algorithm",
                  "Algorithm",
                  algorithms
                ),
                numericInput(
                  "iter",
                  "Iterations",
                  200,
                  1,
                  Inf,
                  10
                ),
                numericInput(
                  "swarm",
                  "Swarm size",
                  30,
                  10,
                  Inf,
                  10
                ),
                numericInput(
                  "bound",
                  "Dose limit",
                  10,
                  0.1,
                  Inf,
                  1
                ),
                numericInput(
                  "pts",
                  "Max design points",
                  2,
                  1,
                  10,
                  1
                ),
                numericInput(
                  "seed",
                  "Seed",
                  155,
                  1,
                  Inf,
                  1
                )

              ),
              mainPanel(
                fluidRow(
                  column(
                    6,
                    selectInput("model_selector", "Model", models),
                    uiOutput("model_formula_display")
                  ),
                  column(
                    6,
                    textInput("theta_input", "Theta ( enter values separated by , )"),
                    selectInput("objective", "Objective", objectives)
                  )
                )
                ,

                actionButton("find", "Find design"),
                plotOutput("sens_plot"),
                waiter::use_waiter(),
                verbatimTextOutput("design_out")
              )
            )

          ),
          tabPanel(
            "Compare",
            "Compare the design efficiency of design \\(\\xi_1\\) relative to the design \\(\\xi_2\\).",
            selectInput("model_selector_compare", "Model", models),
            textInput("theta_input_compare", "Theta ( enter values separated by , )"),
            selectInput("objective_compare", "Objective", objectives),
            fluidRow(
              column(
                6,
                textInput("\\xi1_doses", "\\(\\xi_1 doses\\)"),
                textInput("\\xi1_weights", "\\(\\xi_1 weights\\)")
              ),
              column(
                6,
                textInput("\\xi2_doses", "\\(\\xi_2 doses\\)"),
                textInput("\\xi2_weights", "\\(\\xi_2 weights\\)")
              ),
              actionButton("compute_eff", "Compute efficiency"),
              textOutput("eff_out")
            )


          )
        )


      )
    )
  )

  server = function(input, output, session) {

    # reactive data structure
    values <- reactiveValues()
    values$upload_state = NULL # flag for managing file uploads
    values$DT <- data.frame(x = numeric(),
                            y = numeric(),
                            yhat = numeric()
                            #color = factor(),
                            #shape = factor()
    )
    values$eff_crit = c("NA", "?")

    # set up reactive data structure
    # initialize with empty arrays and plots
    values$OD <- list(
      design = numeric(),
      sens_plot = ggplot2::ggplot(),
      msg = character()
    )

    # display model formula
    output$model_formula_display = renderUI({
      p(withMathJax(model_display(input$model_selector)))
    })

    # sensitivity plot
    output$sens_plot = renderPlot({

      # load plot from reactive data
      ggp = values$OD$sens_plot

      # display plot
      ggp
    })

    # action for Find button
    observeEvent(
      input$find,
      {
        # set up loading indicator
        waiter <- waiter::Waiter$new(
          id = "sens_plot",
          html = waiter::spin_terminal(),
          color = "grey"
        )$show()
        waiter$show()
        on.exit(waiter$hide())

        # grab and process theta from raw input
        theta = process_theta(input$theta_input)


        # select gradient function
        model = input$model_selector
        if (model == "Logistic")
          grad_fun = grad.logistic
        else if (model == "Logistic quadratic")
          grad_fun = grad.logistic.quad
        else if (model == "Logistic cubic")
          grad_fun = grad.logistic.cubic
        else if (model == "Logistic fractional polynomial")
          grad_fun = grad.logistic.fp
        else if (model == "Mixture multistage")
          grad_fun = grad.mix2
        else if (model == "Box-Cox Weibull")
          grad_fun = grad.boxcoxweibull

        # find optimal design
        out = find_design_single(
          grad_fun,
          input$objective,
          theta,
          input$bound,
          input$pts,
          input$algorithm,
          input$swarm,
          input$iter,
          input$seed
        )

        # update reactive data with new design data
        values$OD$msg = ""
        values$OD$design = out$result
        values$OD$sens_plot = out$plot
        #values$OD$response_plot = response_plot
        values$OD$val = out$result$optimumValue

      }
    )

    # run when compare designs button
    observeEvent(
      input$compute_eff,
      {

        d1 = process_theta(input$`\\xi1_doses`)
        d2 = process_theta(input$`\\xi2_doses`)
        w1 = process_theta(input$`\\xi1_weights`)
        w2 = process_theta(input$`\\xi2_weights`)
        theta = process_theta(input$theta_input_compare)
        eff = compute_eff(
          input$model_selector_compare,
          theta,
          input$objective_compare,
          d1,
          d2,
          w1,
          w2
          )

        # save eff to reactive data
        values$eff_crit = c(as.character(eff), input$objective_compare)

      }
    )

    # display design efficiency
    output$eff_out = renderText({
      sprintf("The %s-efficiency of design 1 relative to design 2 is %s",
              values$eff_crit[2], values$eff_crit[1])
    })


  }

  shinyApp(ui, server, ...)
}
