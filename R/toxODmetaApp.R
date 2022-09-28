# code for Shiny app

toxODmetaApp = function(...) {

  # global variables accessible to shiny app
  models = c(
    "logistic",
    "logistic-quadratic",
    "logistic-cubic",
    "logistic-fp",
    "loglogistic",
    "exponential",
    "weibull"
  )
  objectives = c(
    "D",
    "A"
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
          "The quadratic logistic model is defined as
          $$
          P(d) = \\frac{1}{1 + \\exp[-(\\theta_0 + \\theta_1 d  + \\theta_2 d^2)]}
          $$
          "
        ),
        tags$p(
          "The cubic logistic model is defined as
          $$
          P(d) = \\frac{1}{1 + \\exp[-(\\theta_0 + \\theta_1 d  + \\theta_2 d^2 + \\theta_3 d^3)]}
          $$
          "
        ),
        tags$p(
          "The fractional polynomial logistic model is defined as
          $$
          P(d) = \\frac{1}{1 + \\exp[-(\\theta_0 + \\theta_1 d^{\\theta_3} + \\theta_2 d^{\\theta_4})]}
          $$
          Note that \\(\\theta_3, \\theta_4\\) are considered fixed and their estimation is not considered in the optimal design objectives.
          For repeated powers, the linear predictor will include log terms. Refer to Royston and Altman (1994) for more information.
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
        "Model",
        sidebarLayout(
          sidebarPanel(
            numericInput("model_bound", "Upper bound", 10, 1, NA, 1),
            selectInput("fit_model", "Model", models, selected = "logistic")
          ),
          mainPanel(
            plotOutput("model_plot", click = "plot_click"),
            actionButton("fit", "Fit"),
            actionButton("rem_point", "Remove Last Point"),
            actionButton("clear", "Clear all"),
            fileInput("upload", "Import data from file", accept = ".csv"),
            verbatimTextOutput("model_out"),
            actionButton("copymodel", "Copy model to design input")
          )
        )
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
            numericInput(
              "theta3",
              "\\(\\theta_3\\)",
              1,
              -Inf,
              Inf,
              0.01
            ),
            numericInput(
              "theta4",
              "\\(\\theta_4\\)",
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
    # code for Model tab
    ############################################################################
    output$model_plot = renderPlot({

      # update data frame if there is user imported ata
      #file_input()
      # update upper bound based on data
      if (length(values$DT$x > 0)) { # only correct if there is data
        if (input$model_bound < max(values$DT$x)) {
          updateNumericInput(session, "model_bound", value = max(values$DT$x))
        }
      }

      ggp = ggplot2::ggplot(values$DT, ggplot2::aes(x = x, y = y)) +
        # geom_point(aes(color = color,
        #                shape = shape), size = 5) +
        ggplot2::geom_point(color = "red", shape = "circle", size = 5, alpha = 1) +
        ggplot2::lims(x = c(0.1, input$model_bound), y = c(0, 1)) +
        ggplot2::theme_bw() +
        # include so that colors don't change as more color/shape chosen
        #scale_color_discrete(drop = FALSE) +
        #scale_shape_discrete(drop = FALSE) +
        ggplot2::labs(y = "probability of response", x = "X",
             title = "Model fit") +
        ggplot2::geom_abline(slope = 0, intercept = 0.5, linetype = 4) +
        ggplot2::annotate(geom = "text", x = input$model_bound, y = 0.55, label = "ED50")


      # if there non NA values for the predicted values, plot these as well
      if (sum(!is.na(values$DT$yhat)) > 0) {
        ggp = ggp + ggplot2::geom_line(ggplot2::aes(x=x, y=yhat))
      }


      # display plot
      ggp
    })

    ## add new row to reactive dataframe upon clicking plot ##
    observeEvent(input$plot_click, {
      # each input is a factor so levels are consistent for plotting characteristics
      # I modify this code to produce an outcome for logistic regression
      # each point represents 100 observations at that x value
      # y is the number of positive responses received
      # this gives a binomial response

      # make sure x and y are positive
      # this fixes missing values and is required for FP anyways
      x_coord = max(0.01, input$plot_click$x)
      y_coord = max(0, input$plot_click$y)
      add_row <- data.frame(x = x_coord,
                            y = y_coord,
                            yhat = NA
                            #color = factor(input$color, levels = c("Pink", "Green", "Blue")),
                            #shape = factor(input$shape, levels = c("Circle", "Triangle"))
      )
      # add row to the data.frame
      values$DT <- rbind(values$DT, add_row)
    })

    ## 4. remove row on actionButton click ##
    observeEvent(input$rem_point, {
      rem_row <- values$DT[-nrow(values$DT), ]
      values$DT <- rem_row
    })

    # clear data frame
    observeEvent(input$clear, {
      values$DT <- data.frame(x = numeric(),
                              y = numeric(),
                              yhat = numeric()
                              #color = factor(),
                              #shape = factor()
      )
    })

    # model fitting button
    observeEvent(input$fit, {

      # save model data
      model_data = values$DT

      # calculate number of successes
      successes = round(model_data$y * 100)

      # x: model_data$x
      # y: successes
      # fit model
      out = fit_nonlinear_model(successes, model_data$x, input$fit_model)

      # save to reactive object
      values$DT$yhat = out$yhat

      # save data
      # values$p1 = out$p1
      # values$p2 = out$p2
      # values$beta0 = out$beta0
      # values$beta1 = out$beta1
      # values$beta2 = out$beta2
      # values$bound = input$fp_bound
      # values$aic = out$aic
      # values$bic = out$bic

      # save degree values
      # if (input$fpdegree == 3 | input$fpdegree == "Standard cubic") {
      #   values$beta3 = out$beta3
      #   values$p3  = out$p3
      # }

    })

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
        problem$theta = c(input$theta0, input$theta1, input$theta2, input$theta3, input$theta4)
        problem$obj = input$objective
        problem$bound = input$bound
        problem$pts = input$pts

        # adjust theta to only keep the number of parameters in the model
        if (problem$model == "logistic")
          problem$theta = problem$theta[1:2]
        else if (problem$model == "logistic-quadratic")
          problem$theta = problem$theta[1:3]
        else if (problem$model == "logistic-cubic")
          problem$theta = problem$theta[1:4]
        else if (problem$model == "logistic-fp")
          problem$theta = problem$theta[1:5]
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
        else if (input$objective == "A") {
          # maximizing negation of objective => negate objective value
          cat("trM^-1 = ", -obj_val, "\n", sep = "")
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
