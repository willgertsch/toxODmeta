# code for Shiny app

toxODmetaApp = function(...) {

  # global variables accessible to shiny app
  models = c(
    "logistic",
    "logistic-quadratic",
    "loglogistic",
    "exponential",
    "weibull"
  )
  objectives = c(
    "D"
  )
  algorithms = c(
    "PSO",
    "DE"
  )

  ui = fixedPage(
    withMathJax(),
    tabsetPanel(
      selected = "Find",
      type = "pills",
      id = "tabpanel",
      tabPanel(
        "Background",
        titlePanel(
          "Background information for optimal design, models, and algorithms"
        ),
        tags$h3(
          "Overiew",
          style = "text-align:left;"
        ),
        tags$p(
          "This tab gives a short introduction to some of the background theory
          used in this app. For more information, refer to to our paper."
        ),
        tags$h3(
          "Optimal Design",
          style = "text-align:left;"
        ),
        tags$p(
          "This section explains the optimal design concepts used in this app."
        ),
        tags$h3(
          "Models",
          style = "text-align:left;"
        ),
        tags$p(
          "This section describes the models available in the app. Currently,
          these are all models for single variable binary dose-response."
        ),
        tags$p(
          "The logistic model is defined as
          $$
          P(d) = \\frac{1}{1 + \\exp[-(\\theta_0 + \\theta_1 d )]}
          $$
          "
        ),
        tags$p(
          "The loglogistic model is defined as
          $$
          P(d) = \\theta_2 + \\frac{(1-\\theta_2)}{1 + \\exp\\left[ -(\\theta_0 + \\theta_1 \\log d)\\right]}
          $$
          Note that \\( \\theta_2\\) should be between 0 and 1. This is NOT strictly enforced in the app.
          "
        ),
        tags$p(
          "The quadratic logistic model is defined as
          $$
          P(d) = \\frac{1}{1 + \\exp[-(\\theta_0 + \\theta_1 d  + \\theta_2 d^2)]}
          $$
          "
        ),
        tags$p(
          "The exponential (one-hit) model is defined as
          $$
          P(d) = 1 - \\exp\\left[ -(\\theta_0 + \\theta_1 d)\\right]
          $$
          "
        ),
        tags$p(
          "The Weibull model is defined as
          $$
          P(d) = 1 - \\exp\\left[ -(\\theta_0 + \\theta_1 d^{ \\theta_2 })\\right]
          $$
          "
        ),
        tags$p(
          "The loglogistic model is defined as
          $$
          P(d) = \\theta_2 + \\frac{(1-\\theta_2)}{1 + \\exp\\left[ -(\\theta_0 + \\theta_1 \\log d)\\right]}
          $$
          Note that \\( \\theta_2\\) should be between 0 and 1. This is NOT strictly enforced in the app.
          "
        ),
        tags$h3(
          "Algorithms",
          style = "text-align:left;"
        ),
        tags$p(
          "This section describes the algorithms available to use in the app.
          The app currently supports a selection of algorithms from the
          metaheuristicOpt package. The algorithms in this selection were
          included because they had the best performance."
        )
      ),
      tabPanel(
        "Model"
      ),
      tabPanel(
        "Find",
        titlePanel(
          "Find the optimal design"
        ),
        "Find the optimal design for different models using metaheuristics.
        Use the sidepanel to set up the design problem you want to solve and
        the algorithm options you wish to use. Clicking the \"Find\" button will
        run the algorithm with selected options on the design problem specified.",
        sidebarLayout(
          sidebarPanel(
            "Design problem options",
            selectInput(
              "model",
              "Model",
              models
            ),
            numericInput(
              "theta0",
              "\\(\\theta_0\\)",
              0,
              -Inf,
              Inf,
              0.01
            ),
            numericInput(
              "theta1",
              "\\(\\theta_1\\)",
              1,
              -Inf,
              Inf,
              0.01
            ),
            numericInput(
              "theta2",
              "\\(\\theta_2\\)",
              1,
              -Inf,
              Inf,
              0.01
            ),
            selectInput(
              "objective",
              "Objective",
              objectives
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
              "seed",
              "Seed",
              155,
              1,
              Inf,
              1
            )
          ),
          mainPanel(
            plotOutput("sens_plot"),
            #plotOutput("model_plot"),
            actionButton("find", "Find"),
            #actionButton("plot_eff", "Plot efficiencies"),
            waiter::use_waiter(),
            verbatimTextOutput("design_out")
          )
        )
      ),
      tabPanel(
        "Compare"
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

    ############################################################################
    # code for Find tab
    ############################################################################
    # set up reactive data structure
    # initialize with empty arrays and plots
    values$OD <- list(
      design = numeric(),
      sens_plot = ggplot2::ggplot(),
      response_plot = ggplot2::ggplot(),
      msg = character()
    )

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

        # set up design problem
        problem = list()
        problem$model = input$model
        problem$theta = c(input$theta0, input$theta1, input$theta2)
        problem$obj = input$objective
        problem$bound = input$bound
        problem$pts = input$pts

        # adjust theta to only keep the number of parameters in the model
        if (problem$model == "logistic")
          problem$theta = problem$theta[1:2]
        else if (problem$model == "logistic-quadratic")
          problem$theta == problem$theta[1:3]
        else if (problem$model == "exponential")
          problem$theta = problem$theta[1:2]
        else if (problem$model == "weibull")
          problem$theta = problem$theta[1:3]
        else if (problem$model == "loglogistic")
          problem$theta = problem$theta[1:3]

        # set up algorithm options
        alg_options = list()
        alg_options$algorithm = input$algorithm
        alg_options$iter = input$iter
        alg_options$swarm = input$swarm

        # find optimal design
        seed = input$seed
        out = toxODmeta(problem, alg_options, seed)

        # update reactive data with new design data
        values$OD$msg = ""
        values$OD$design = out$result
        values$OD$sens_plot = out$sens_plot
        #values$OD$response_plot = response_plot
        values$OD$val = out$optimumValue

      }
    )

    # update design output textbox
    output$design_out = renderPrint({

      # load objective value and design from reactive data
      obj_val = values$OD$val
      raw = values$OD$design

      # handle case where nothing has been run
      if (length(raw) == 0) {
        cat("No design")
      }
      else { # all other cases

        # show objective criteria function
        if (input$objective == "D") {
          cat("log(Det(M)) = ", obj_val, "\n", sep = "")
        }

        # process and display design
        l = length(raw)
        labbs = names(raw)
        cat(labbs[1:(l/2)], "\n", sep = "    ")
        cat(round(raw[1:(l/2)], 3), "\n")
        cat(labbs[(l/2 + 1):l], "\n", sep = "    ")
        cat(round(raw[(l/2 + 1):l], 3))
      }

    })
  }

  shinyApp(ui, server, ...)
}
