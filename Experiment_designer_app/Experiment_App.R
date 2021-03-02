# Amin Shoari Nejad
rm(list = ls())
library(DiceOptim)
library(ggplot2)
library(dplyr)
source("singlerunnoisyoptimizer.R")
source("optim_sithara.R")
library(shinycssloaders)

set.seed(123)


   ui <- fluidPage(
     titlePanel("Adaptive Experiment Designer"), # App's title
     hr(),
     p(div(HTML(""))),
     sidebarLayout(
       sidebarPanel(
         fluidRow(
           column(
             width = 12,
   
   
             # Data Input:
             fileInput("datafile", "Select your data CSV file",
               accept = c("csv", "comma-separated-values", ".csv")
             ),
   
             # Variables Inputs:
             uiOutput("selectize1"),
             uiOutput("selectize2"),
             uiOutput("action1"),
             hr(),
             uiOutput("lower1"),
             uiOutput("higher1"),
   
   
   
             # Run Button
             actionButton(inputId = "run", label = "Run"),
             hr(),
           )
         )
       ),
   
   
   
   
   
       # Main panel for displaying outputs ----
       mainPanel(
   
         # Output: HTML table with requested number of observations ----
         navbarPage(
           "Output:",
           tabPanel(
             "Main",
             fluidPage(
               fluidRow(
                 textOutput("text1"),
                 hr(),
                 tableOutput("setting") %>% withSpinner(color = "#1E90FF"),
                 uiOutput("new_value"),
                 uiOutput("proc"),
                 # uiOutput("reRun"),
                 plotOutput("plot") %>% withSpinner(color = "#1E90FF"),
                 hr(),
                 uiOutput("plott"),
               )
             )
           ),
           tabPanel(
             "Dataset",
             fluidPage(
               fluidRow(
                 tableOutput("table")
               )
             )
           ),
           tabPanel(
             "Notes",
             fluidPage(
               fluidRow(
                 h5(div(HTML("1- <em>Having carried out your experiment, please update the dataset to include the factors and experimental result(s). The app can then be refreshed to optimise the settings for your next adaptive experiment.</em>."))),
                 h5(div(HTML("2- <em>This app provides 1 up to 4 settings per batch (# experiments per run).</em>"))),
                 h5(div(HTML("3- <em>The smaller the batch the better for adaptive design! </em>"))),
                 h5(div(HTML("4- <em>I suggest cycling through the acquisitions in turn. First RANDOM, then AKG, then EQI, then GRID (following Nature Comms paper).</em>."))),
                 h5(div(HTML("5- <em>Another option is to choose an acquisition function at random each time.</em>"))),
                 h5(div(HTML("6- <em>Should you need more than 4 in a batch, an easy option is to generate more random searches. (There are many other options such as using different models and or different acquisition functions but these require further work.)</em>"))),
                 # #
               )
             )
           )
         )
       )
   
       ########################
     )
   )
   
   
   
   
   server <- function(input, output, session) {
     df_dim <- reactiveVal(NULL)
     df_text <- reactiveVal(NULL)
     df_text1 <- reactiveVal(NULL)
   
     values <- reactiveValues()
   
   
     observeEvent(input$datafile, { # 1. Getting data and variable determination
   
       Dataset <- read.csv(input$datafile$datapath)
       df_dim(rep("Prior", nrow(Dataset)))
   
       varnames <- names(Dataset)
   
       output$selectize1 <- renderUI({
         selectizeInput("invar", "Select Input Variables", choices = varnames, multiple = TRUE)
       })
   
       output$selectize2 <- renderUI({
         selectizeInput("dvar", "Select target Variable", choices = varnames, multiple = TRUE)
       })
   
       output$action1 <- renderUI({
         actionButton("next1", "Next")
       })
     }) # End of observation 1 for dataset
   
   
     observeEvent(input$next1, { # 2 Giving lower/higher bound values
   
   
   
       output$lower1 <- renderUI({
         textInput("lower", "Replace each variable's name with its lower bound value", paste0(input$invar, collapse = "   ,   "))
       })
   
       output$higher1 <- renderUI({
         textInput("higher", "Replace each variable's name with its higher bound value", paste0(input$invar, collapse = "   ,   "))
       })
     }) # End of observation 2 after setting input and response variables
   
   
     data <- reactive({ # Check if there is new data, if not use the previous one
   
   
       if (is.null(values$DF)) {
   
       
         read.csv(input$datafile$datapath)
  
       }
       else {
         values$DF
       }
     })
   
   
   
     observeEvent(input$run, { # 3 Running the model
   
       req(input$higher != paste0(input$invar, collapse = " ,"))
  
       values$ACQ1 <- "AKG"
       values$ACQ2 <- "EQI"
       values$ACQ3 <- "Grid"
       values$ACQ4 <- "Random"
   
   
       req(input$datafile)
   
       tryCatch(
         {
           doe.size <- 3
   
           lower_x <- as.numeric(unlist(strsplit(input$lower, ",")))
           upper_x <- as.numeric(unlist(strsplit(input$higher, ",")))
   
           prior_data <- data()[, input$invar]
           prior_data <- na.omit(prior_data)
           prior_response <- -data()[, input$dvar]
           prior_response <- na.omit(prior_response)
   

           # First fit a noiseless model to estimate the nugget effect using MLE
           model_nugget <- km(y ~ 1, # constant linear trend
             design = prior_data, # current design
             response = prior_response, # experimental results
             covtype = "matern5_2", # Matern covariance with smoothness 5/2 same as Weaver et al
             optim.method = "BFGS", # genetic optimizer, could also use bfgs here
             nugget.estim = TRUE,
             # noise.var=rep(noise.var,1,doe.size), # variance in the noise in the observations
             lower = lower_x, # lower bounds of the correlation parameters for optimization
             upper = upper_x, # upper bounds of the correlation parameters for optimization
             control = list(trace = FALSE)
           )
   
           # pull out the maximum likelihood estimate of the nugget
           noise.var <- model_nugget@covariance@nugget
           # compare with var(prior_response[10:14])
           # Now use this nugget as our homogeneous noise estimate for the noisy GP model fit (noise.var and nugget are mutually exclusive in the diceoptim package)
           gp_model <- km(y ~ 1,
             design = prior_data,
             response = prior_response,
             covtype = "matern5_2",
             optim.method = "BFGS",
             noise.var = rep(noise.var, 1, dim(prior_data)[1]),
             lower = lower_x,
             upper = upper_x,
             control = list(trace = FALSE)
           )
   
           ################################################################################################################
           ########### STEP 3. Optimise the aquisition to find the next design points #############################
           ################################################################################################################
   
   
           # This is the optimisation step. It can take from a few seconds (acq="Random") to a couple of minutes (acq="Grid")
           res1 <- optim_sithara(values$ACQ1, gp_model, prior_data, prior_response, lower_x, upper_x)
           res1 <- round(t(res1), 1) # might need to be changed to 2 decimal places or a user input in a later version of the app
           values$RES1 <- cbind(res1, values$ACQ1)
           values$RES1 <- data.frame(values$RES1)
           names(values$RES1) <- c(names(prior_data), "Acquisition")
   
           res2 <- optim_sithara(values$ACQ2, gp_model, prior_data, prior_response, lower_x, upper_x)
           res2 <- round(t(res2), 1) # might need to be changed to 2 decimal places or a user input in a later version of the app
           values$RES2 <- cbind(res2, values$ACQ2)
           values$RES2 <- data.frame(values$RES2)
           names(values$RES2) <- c(names(prior_data), "Acquisition")
   
           res3 <- optim_sithara(values$ACQ3, gp_model, prior_data, prior_response, lower_x, upper_x)
           res3 <- round(t(res3), 1) # might need to be changed to 2 decimal places or a user input in a later version of the app
           values$RES3 <- cbind(res3, values$ACQ3)
           values$RES3 <- data.frame(values$RES3)
           names(values$RES3) <- c(names(prior_data), "Acquisition")
   
           doe.size <- 3
           res4 <- optim_sithara(values$ACQ4, gp_model, prior_data, prior_response, lower_x, upper_x)
           res4 <- round(t(res4[1:3]), 1) # might need to be changed to 2 decimal places or a user input in a later version of the app
           values$RES4 <- cbind(res4, values$ACQ4)
           values$RES4 <- data.frame(values$RES4)
           names(values$RES4) <- c(names(prior_data), "Acquisition")
   
    
         },
         error = function(err) {
           showNotification(paste0("Something went wrong! Please make sure you entered correct values."), type = "err")
         }
       )
     }) # End of 1st run
   
   
   
     observe({
   
   
       # req(values$RES)
       req(input$run)
   
   
       df_text1(
         paste0("PLEASE SET UP YOUR NEXT ADAPTIVE EXPERIMENT(S) AT ONE OR MORE OF THE FOLLOWING SETTINGS: ")
       )
     })
   
     output$text1 <- renderText({ # Report based on the run
   
       req(values$RES1)
       req(input$run)
       df_text1()
     })
   
     output$setting <- renderTable({ # Report based on the run
   
       # req(values$RES)
       req(input$run)
   
       rbind(values$RES1, values$RES2, values$RES3, values$RES4)
     })
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
     max_response <- reactive({
       req(input$run)
       req(!is.null(values$RES1))
   
   
       Dataset <- read.csv(input$datafile$datapath)
   
       prior_response <- -Dataset[, input$dvar]
       # prior_response = -data()[,4]
       max_response <- -min(prior_response)
       return(max_response)
     })
     #
     min_response <- reactive({
       req(input$run)
       req(!is.null(values$RES1))
   
       Dataset <- read.csv(input$datafile$datapath)
   
       prior_response <- -Dataset[, input$dvar]
       # prior_response = -data()[,4]
       min_response <- -max(prior_response)
       return(min_response)
     })
     #
     #
     #
     output$plot <- renderPlot({
       req(input$run)
       req(!is.null(values$RES1))
   
       Dataset <- read.csv(input$datafile$datapath)
   
       target_response <- Dataset[, input$dvar]
   
   
       p <- ggplot(data = Dataset) +
         geom_point(aes(x = c(1:nrow(Dataset)), y = target_response), size = 5) +
         xlab("Experiment number") +
         ylab("Target response") +
         ggtitle("Uploaded data") +
         expand_limits(y = c(min_response(), max_response()))
   
       p + theme_bw() + theme(text = element_text(size = 20))
     })
     #
   
   
   
   
   
   
   
   
   
   
     output$table <- renderTable({
       req(input$datafile)
       data()
     })
   
   
   
     # Reset all inputs:
     observeEvent(input$datafile, { # 4 Reseting everything when a new dataset is observed
       #
   
       df_text(NULL)
       values$DF <- NULL
       values$RES <- NULL
       df_text1(NULL)
       values$RES1 <- NULL
       values$RES2 <- NULL
       values$RES3 <- NULL
       values$RES4 <- NULL
     }) # End of observation 4
  # ,
}

# Run the application
shinyApp(ui = ui, server = server)
