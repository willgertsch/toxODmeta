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
      selected = "Find",
      type = "pills",
      id = "toptabpanel",
      tabPanel(
        "User manual"
      ),
      tabPanel(
        "Find",

        # sidebar layout with tabs
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
            tabsetPanel(
              selected = "Design",
              type = "pills",
              id = "designtabpanel",
              tabPanel(
                "Design",
                selectInput("model_selector", "Model", models),
                uiOutput("model_formula_display"),
                textInput("theta_input", "Theta (enter values separated by ,)"),
                selectInput("objective", "Objective", objectives),
                actionButton("find", "Find design"),
                plotOutput("sens_plot"),
                waiter::use_waiter(),
                verbatimTextOutput("design_out")
              ),
              tabPanel(
                "Compare"
              )
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


  }

  shinyApp(ui, server, ...)
}
