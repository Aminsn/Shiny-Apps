# Amin Shoari Nejad
rm(list = ls())
library(ggplot2)
library(shinycssloaders)
library(lubridate)
library(tidyverse)
library(plotly)
library(dplyr)
library(randomForest)
library(caret)
library(corrplot)
library(recipes)
library(PerformanceAnalytics)
library(linkspotter)



load("merged_bay2.RData")
varnames <- names(df2)



ui <- fluidPage(
  titlePanel("Buoy Data Analysis"), # App's title
  hr(),
  p(div(HTML(""))),
  sidebarLayout(
    sidebarPanel(
      fluidRow(
        column(
          width = 12,


          # Data Input:


          # Variables Inputs:
          #
          varSelectInput("variables", "Select Input Variables", df2, multiple = TRUE),
          selectizeInput("outvar", "Select Response Variable", choices = varnames, "turbidity", multiple = F),
          sliderInput("mtry", "Select mtry parameter", 1, 7, 1, step = 1),
          sliderInput("tree", "Number of trees", 100, 5000, 500, step = 100),
          sliderInput("perc", "% of data to be used for training", 10, 100, 80, step = 10),


          # Run Button
          actionButton(inputId = "run", label = "Run")
          # hr(),
          # selectInput("outvar", label = h3("Outcome"),
          #             choices = list("Turbidity" = "turbidity"), selected = 1),
          #
          # selectInput("variables", label = h3("Explanatory variable"),
          #             choices = list("DO" = "do",
          #                            "Temp" = "temp"), selected = 1)
        )
      )
    ),





    # Main panel for displaying outputs ----
    mainPanel(
      tabsetPanel(
        type = "tabs",

        # textOutput("text1"),
        # hr(),
        # uiOutput("new_value"),
        # uiOutput("proc"),
        # #uiOutput("reRun"),
        tabPanel(
          "Model performance",
          hr(),
          plotlyOutput("plot") %>% withSpinner(color = "#1E90FF"),
          plotOutput("varimp")
        ),

        # verbatimTextOutput("summary")
        # hr(),
        # uiOutput("plott"),
        tabPanel(
          "Data",
          hr(),
          DT::dataTableOutput("tbl")
        ), # Data as datatable

        tabPanel(
          "Correlogram",
          hr(),
          radioButtons("action", "Plot type: ", choices = list("Correlation" = "cor", "Scatterplot" = "scatter", "Importance" = "imp"), inline = T),
          plotOutput("correlation")
        ),
        tabPanel(
          "Timeseries",
          hr(),
          selectizeInput("var_plt", "Select a Variable", choices = varnames, "turbidity", multiple = F),
          plotOutput("timeseries")
        )
      )
    )

    ########################
  )
)




server <- function(input, output, session) {
  rfmod <- reactiveVal(NULL)

  df3 <- reactive({
    df2 <- ungroup(df2)
    df2 <- as.data.frame(df2)
    df3 <- df2 %>% dplyr::select(!!!input$variables, input$outvar)
    df3 <- na.omit(df3)
    return(df3)
  })


  # Data output
  output$tbl <- DT::renderDataTable({
    DT::datatable(df3(), options = list(lengthChange = FALSE))
  })



  plt <- eventReactive(input$run, {
    samp <- sample(nrow(df3()), input$perc * nrow(df3()) / 100)
    train <- df3()[samp, ]
    test <- df3()[-samp, ]

    #-------------------REGRESSION-------------------#


    rfMod <- randomForest(train[, input$outvar] ~ ., data = train[, !(names(train) %in% c(paste(input$outvar)))], n.tree = input$tree, mtry = input$mtry)
    rfmod(rfMod)
    pred <- predict(rfMod, newdata = test)

    fit <- lm(test[, input$outvar] ~ pred)
    r2 <- paste("R2: ", format(summary(fit)$adj.r.squared, digits = 4))

    test$pred <- pred
    p <- ggplot(test, aes(test[, input$outvar], pred)) +
      geom_smooth(method = "lm", se = FALSE, color = "blue", formula = y ~ x) +
      labs(x = "Observed", y = "Predicted") +
      ggtitle(r2) +
      geom_point() +
      theme_bw()

    ggplotly(p)
  })


  output$plot <- renderPlotly({
    plt()
  })

  output$correlation <- renderPlot({
    if (input$action == "cor") {
      
      df4 <- df3() %>% select((where(is.numeric)))
      c <- cor(df4)
      plt <- corrplot(c, order = "hclust", type = "lower")
   
       } else {
         
      if(input$action == "scatter"){
        
      df4 <- df3() %>% select((where(is.numeric)))
      plt <- chart.Correlation(df4, histogram = TRUE, pch = 19)
      
      } else{
        
        corCouples <- multiBivariateCorrelation(df3(), corMethods = "mic") 
        cor <-corCouplesToMatrix(x1_x2_val = corCouples[,c('X1','X2',"mic")])
        c = as.matrix(cor)
        corrplot(c, cl.lim = c(0, 1))

        
      }
      }

    plt
  }) %>% bindCache(input$action, input$variables)

  output$varimp <- renderPlot({
    req(!is.na(rfmod()))
    varImpPlot(rfmod())
  })


  output$timeseries <- renderPlot({
    df2 <- ungroup(df2)
    df2 <- as.data.frame(df2)
    df5 <- df2 %>% dplyr::select(input$var_plt, date)

    strt <- min((df5$date[!is.na(df5[, input$var_plt])]))
    endd <- max((df5$date[!is.na(df5[, input$var_plt])]))

    ggplot(df5, aes(date, df5[, input$var_plt])) +
      geom_line() +
      labs(y = paste0(input$var_plt)) +
      scale_x_date(limits = c(strt, endd)) +
      ggtitle(paste0("Start date: ", strt)) +
      theme_bw()
  })
}

# Run the application
shinyApp(ui = ui, server = server)
