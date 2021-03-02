# Amin Shoari Nejad
library(readxl)
library(leaflet)
library(dplyr)
library(lubridate)
library(Jmisc)
library(tidyverse)
library(reshape2)
library(leaflet.extras)
library(corrplot)

points <- read_excel("~/Sea Level Analysis/Dublin_Bay_Project-master/Gaugeslocs.xlsx")
varnames <- points$Location

ui <- fluidPage(
  titlePanel("SLR in Irish sea"),
  hr(),
  sidebarLayout(
    sidebarPanel(
      # Input: Selector for choosing dataset ----
      numericInput(
        inputId = "date1",
        label = "From",
        value = 2000
      ),

      # Input: Numeric entry for number of obs to view ----
      numericInput(
        inputId = "date2",
        label = "To",
        value = 2016
      ),
      tableOutput("summary")
    ),



    # Main panel for displaying outputs ----
    mainPanel(
      tabsetPanel(
        type = "tabs",
        tabPanel(
          "Map",
          hr(),
          leafletOutput("view")
        ),
        tabPanel(
          "Correlogram",
          hr(),
          radioButtons("action", "Plot type: ", choices = list("Yearly" = "yearly", "Monthly" = "monthly"), inline = T),
          plotOutput("plot2")
        ),
        tabPanel(
          "Trend comparison",
          hr(),
          radioButtons("action2", "Plot type: ", choices = list("Yearly" = "yearly", "Monthly" = "monthly"), inline = T),
          plotOutput("plot3"),
          selectizeInput("variables", "Select two locations", choices = varnames, "Dublin", options = list(maxItems = 2) ,multiple = T)
          
        )
      )
    )
  )
)

server <- function(input, output) {
  df <- read_csv("~/Sea Level Analysis/Dublin_Bay_Project-master/IrishSeaComplete.csv")
  df$oriel[which.min(df$oriel)] <- NA
  df$oriel[which.min(df$oriel)] <- NA

  # df[,13] <-  df[,13]*1000

  names(df)[-1] <- points$Location
  points$lng <- points$lng * (-1)
  points$Rate <- rep(1, 15)

  points$Months <- as.character(rep(0, 15))


  # Generate a summary of the dataset ----
  output$summary <- renderTable({
    df2 <- df %>% filter(time >= input$date1 & time <= input$date2)

    for (i in 2:16) {
      x <- unlist(df2[, i])
      y <- unlist(df2[, 1])

      if (all(is.na(x))) {
        points[i - 1, 4] <- NA
      }
      else {
        lm.fit <- lm(x ~ y)
        points[i - 1, 4] <- coefficients(lm.fit)[2]
        points[i - 1, 5] <- as.character(length(na.omit(x)))
      }
    }
    
    names(points)[4] <-  "Rates (mm/yr)"
    names(points)[5] <-  "# of months"
    points[, 3:5]
  })


  # Show the first "n" observations ----
  output$view <- renderLeaflet({
    df3 <- df %>% filter(time >= input$date1 & time <= input$date2)


    for (i in 2:16) {
      x <- unlist(df3[, i])
      y <- unlist(df3[, 1])

      if (all(is.na(x))) {
        points[i - 1, 4] <- NA
      }
      else {
        lm.fit <- lm(x ~ y)
        points[i - 1, 4] <- coefficients(lm.fit)[2]
      }
    }

    points$r <- points$Rate
    points$r[points$r >= 10] <- 10
    points$r[points$r <= -1] <- -1

    colors <- c("green", "red")
    pal <- colorNumeric(colors, domain = c(-1:10))
    leaflet() %>%
      addTiles() %>%
      setView(-5, 54, zoom = 6) %>%
      addProviderTiles("Esri.WorldGrayCanvas") %>%
      addCircleMarkers(
        data = points, lat = ~lat, lng = ~lng, label = ~Location, radius = 10,
        color = ~ pal(r)) %>%
      leaflet.extras::addResetMapButton() %>%
      leaflet.extras::addSearchOSM(options = searchOptions(
        collapsed = T, zoom = 9, hideMarkerOnCollapse = T, moveToLocation = FALSE,
        autoCollapse = T
      ))
  })

  observeEvent(input$view_marker_click, {
    output$plot <- renderPlot({

      # df4 <- df %>% filter(time >= input$date1 & time <= input$date2)
      loc <- gather(df, Location, value, "Bangor":"Arklow")
      loc <- left_join(loc, points)

      loc <- loc %>% filter(lng == input$view_marker_click$lng)
      loc <- loc %>% filter(time >= input$date1 & time <= input$date2 & time >= loc$time[min(which(!is.na(loc$value)))])
      x_axis <- seq(floor(min(loc$time)), floor(max(loc$time)), by = 2)
      
      ggplot(loc, aes(time, value)) +
        geom_line() +
        theme_bw() +
        geom_smooth(method = "lm", formula = y ~ x) +
        labs(
          x = "Year",
          y = "Sea Level (mm)"
        ) +
        scale_x_continuous(breaks = x_axis) +
        theme(axis.text.x = element_text(angle = 45, size = 10,  vjust = 0.5))+
        ggtitle(paste0(min(loc$Location)))
    })
  })

  observeEvent(input$view_marker_click, {
    showModal(modalDialog(
      title = "",
      size = "l",
      footer = actionButton("close", "Close"),
      plotOutput("plot")
    ))
  })

  observeEvent(input$close, { # Removing modal and erasing previous plot

    output$plot <- NULL
    removeModal()
  })


  output$plot2 <- renderPlot({
    
    if (input$action == "monthly") {
    df_cor <- df %>% filter(time >= input$date1 & time <= input$date2)

    c <- cor(df_cor[, -1], use = "pairwise.complete.obs")
    corrplot(c)
    }else{
    df_cor <- df %>% filter(time >= input$date1 & time <= input$date2) %>% group_by(time = floor(time)) %>% summarise_all(~ mean(., na.rm = T))
    c <- cor(df_cor[, -1], use = "pairwise.complete.obs")
    corrplot(c)
    }
  })
  
  
  output$plot3 <- renderPlot({
    
    if (input$action2 == "monthly") {
      df_comp <- df %>% filter(time >= input$date1 & time <= input$date2) %>% mutate_at(vars(-("time")), ~ . - mean(., na.rm = T))
      
      
    }else{
      df_comp <- df %>% filter(time >= input$date1 & time <= input$date2) %>% group_by(time = floor(time)) %>% 
        summarise_all(~ mean(., na.rm = T)) %>% ungroup() %>% mutate_at(vars(-("time")), ~ . - mean(., na.rm = T))
      
    }
    
    df_comp <- df_comp %>% dplyr::select(!!!input$variables, time)
    df_comp <- gather(df_comp, location, values, -time)
    
    ggplot(data = df_comp, aes(time, values, color = location)) + geom_line()
    
  })
  
  
  
}

# Run the application
shinyApp(ui = ui, server = server)
