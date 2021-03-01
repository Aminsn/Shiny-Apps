library(readxl)
library(leaflet)
library(dplyr)
library(lubridate)
library(Jmisc)
library(shinythemes)
library(tidyverse)
library(reshape2)
library(shinyBS)
library(plotly)

rm(list = ls())

coords <- readRDS("coordinates.rds")
df_total <- readRDS("df_total3.rds")

ui <- fluidPage(
  theme = shinytheme("flatly"),
  titlePanel("Monitoring Ireland's Traffic"),
  hr(),
  sidebarLayout(
    sidebarPanel(
      fluidRow(
        h4(div(HTML("<em> Select two periods ... </em>"))),
        dateRangeInput("dateRange",
          label = "Set 1st period's dates",
          start = as.Date("2019-08-01"), end = as.Date("2019-09-27")
        ),
        dateRangeInput("dateRange2",
          label = "Set 2nd period's dates",
          start = as.Date("2020-08-01"), end = as.Date("2020-09-27")
        ),
        plotOutput("plot2"),
      ),
    ),



    # Main panel for displaying outputs ----
    mainPanel(
      navbarPage(
        "",
        tabPanel(
          "Map",
          tags$style(
            type = "text/css", "html, body {width:100%;height:100%}",
            ".leaflet .legend i{
      border-radius: 50%;
      width: 10px;
      height: 10px;
      margin-top: 4px;
      }
    "
          ),
          leafletOutput("view"),
          hr(),
          actionButton("go", "Plot"),
          # textOutput("temp"),
          # tableOutput("view"),
          # Output: HTML table with requested number of observations ---
          bsModal("modalExample", "", "go", size = "large", plotlyOutput("plot")),
          # plotOutput("plot")
          br(),
          br(),
          HTML('Click on a traffic station and then click on the "Plot" button to show the traffic patterns.'),
          br(),
          br(),
        ),
        tabPanel(
          "About",
          fluidPage(
            fluidRow(
              p("This app is developed for comparing traffic patterns in Ireland between different time periods and uses data from", span("https://www.nratrafficdata.ie/.", style = "color:blue")),
              p("Note that there could be some issues with the data such as missing values and etc. Feel free to use the 'Plot' button and observe the data in more detail.")
            )
          )
        )
      )
    )
  )
)

server <- function(input, output) {



  # Show the first "n" observations ----
  output$view <- renderLeaflet({
    df_app_a <- df_total %>% filter(date <= input$dateRange2[2] & date >= input$dateRange2[1])
    df_app_b <- df_total %>% filter(date <= input$dateRange[2] & date >= input$dateRange[1])

    df_app_b <- df_app_b %>%
      group_by(location) %>%
      summarise(value = mean(value, na.rm = T))
    df_app_a <- df_app_a %>%
      group_by(location) %>%
      summarise(value = mean(value, na.rm = T))

    names(df_app_a)[2] <- "value2"

    check <- full_join(df_app_a, df_app_b)

    check <- check %>% mutate(diff = value2 - value)

    # coords <- `2020_complete` %>% group_by(location) %>% summarise(lat = mean(lat, na.rm = TRUE), lng = mean(lng, na.rm = TRUE))

    y <- inner_join(check, coords)
    y <- y %>% filter(is.na(lat) == F)


    y$r <- abs(y$diff)

    y$perc <- (y$diff) / y$value * 100


    y$size <- y$perc
    y$size[which(abs(y$size) > 100)] <- 100
    y$size <- y$size / 10

    y$bins <- cut(y$perc,
      breaks = c(-10e10, -50, -5, 5, 50, 10e10), labels = c("-50% or less", "-5% to -50%", "-5% to 5%", "5% to 50%", "50% or more"),
      include.lowest = T
    )

    y <- y %>% mutate(popup = paste0(round(perc), "%"))

    Levels <- c("-50% or less", "-5% to -50%", "-5% to 5%", "5% to 50%", "50% or more")

    colors <- c("darkblue", "blue", "gray", "red", "darkred")
    pal <- colorFactor(palette = colors, levels = Levels)

    y <- y %>% filter(is.na(perc) == F)

    addLegendCustom <- function(map, colors, labels, sizes, opacity = 0.7, title = "Map info") {
      colorAdditions <- paste0(colors, "; width:", sizes, "px; height:", sizes, "px; margin-left:", c(0, 2, 4, 2, 0), "px; margin-top:", c(4, 4, 5, 4, 4), "px;")
      labelAdditions <- paste0("<div style='display: inline-block;height: ", sizes, "px; margin-left:", c(3, 0, 2, 5, 3), "px;margin-top: 4px;line-height: ", sizes, "px;'>", labels, "</div>")

      return(addLegend(map, colors = colorAdditions, labels = labelAdditions, opacity = opacity, title = title))
    }

    leaflet(y) %>%
      addTiles() %>%
      setView(-7.5959, 53.5, zoom = 6) %>%
      addCircleMarkers(data = y, lat = ~lat, lng = ~lng, popup = ~ paste0(popup, " change in traffic levels between the selected periods"), radius = ~ abs(size) + 3, color = ~ pal(bins), stroke = F, fillOpacity = 0.7) %>%
      addLegendCustom(colors = c("darkblue", "blue", "gray", "red", "darkred"), labels = c("< -50%", "-5% to -50%", "-5% to 5%", "5% to 50%", "> 50%"), sizes = c(20, 15, 12, 15, 20))
  })



  output$plot <- renderPlotly({
    validate(
      need(input$view_marker_click != "", "Please select a station")
    )

    df_app_a <- df_total %>% filter(date <= input$dateRange2[2] & date >= input$dateRange2[1])
    df_app_b <- df_total %>% filter(date <= input$dateRange[2] & date >= input$dateRange[1])

    df_app_b <- df_app_b %>%
      group_by(location) %>%
      summarise(value = mean(value, na.rm = T))
    df_app_a <- df_app_a %>%
      group_by(location) %>%
      summarise(value = mean(value, na.rm = T))

    names(df_app_a)[2] <- "value2"

    check <- full_join(df_app_a, df_app_b)

    check <- check %>% mutate(diff = value2 - value)

    coords <- distinct(coords)
    y <- inner_join(check, coords)
    y <- y %>% filter(is.na(lat) == F)

    y$r <- abs(y$diff)

    y$perc <- (y$diff) / y$value * 100

    y$per <- y$per
    y$size <- y$perc
    y$size[which(abs(y$size) > 600)] <- 100
    y$size <- y$size / 10

    y <- y %>% mutate(perc = paste0(round(perc), "%"))


    if (is.null(input$view_marker_click$lng) | nrow(df_app_a) < 1 | nrow(df_app_b) < 1) {
      return()
    }
    else {
      per1 <- df_total %>% filter(date >= input$dateRange[1] & date <= input$dateRange[2] & location == y$location[which(y$lng == input$view_marker_click$lng)])
      per1 <- per1 %>% mutate(days = seq(1, nrow(per1), 1))

      per2 <- df_total %>% filter(date >= input$dateRange2[1] & date <= input$dateRange2[2] & location == y$location[which(y$lng == input$view_marker_click$lng)])
      per2 <- per2 %>% mutate(days = seq(1, nrow(per2), 1))



      # ggplot(aes(x = date, y = value, colour = location)) +
      # geom_line() +
      # theme_bw(8) +
      # scale_x_date(breaks = "1 month", date_labels = "%d-%b") +
      # scale_y_continuous(breaks = scales::pretty_breaks(n = 7))  +
      # theme( legend.position = "none",
      #        axis.text.x = element_text(size = 10)) +
      # labs(x = 'Date', y = 'Number of cars per day',
      #      title = paste0("Total cars per day at ", y$location[which(y$lng == input$view_marker_click$lng)]))


      plt <- ggplot() +
        geom_line(data = per1, aes(x = days, y = value, label = date, colour = "1st period")) +
        geom_line(data = per2, aes(x = days, y = value, label = date, colour = "2nd period")) +
        theme_bw(8) +
        scale_y_continuous(breaks = scales::pretty_breaks(n = 7)) +
        theme(axis.text.x = element_text(size = 10)) +
        labs(
          x = "Days", y = "Number of cars per day",
          title = paste0("Total cars per day at ", y$location[which(y$lng == input$view_marker_click$lng)])
        )

      ggplotly(plt, tooltip = c(Date = "label"))
    }
  })


  output$plot2 <- renderPlot({
    df_app_a <- df_total %>% filter(date <= input$dateRange2[2] & date >= input$dateRange2[1])
    df_app_b <- df_total %>% filter(date <= input$dateRange[2] & date >= input$dateRange[1])

    validate(
      need(nrow(df_app_a) > 0 & nrow(df_app_b) > 0, "Please select valid dates")
    )

    if (nrow(df_app_a) > 0 & nrow(df_app_b) > 0) {
      df_app_b <- df_app_b %>%
        group_by(location) %>%
        summarise(value = mean(value, na.rm = T))
      df_app_a <- df_app_a %>%
        group_by(location) %>%
        summarise(value = mean(value, na.rm = T))

      names(df_app_a)[2] <- "value2"

      check <- full_join(df_app_a, df_app_b)

      check <- check %>% mutate(diff = value2 - value)

      # coords <- `2020_complete` %>% group_by(location) %>% summarise(lat = mean(lat, na.rm = TRUE), lng = mean(lng, na.rm = TRUE))

      y <- inner_join(check, coords)

      y$perc <- (y$diff) / y$value * 100
      y$perc[which(y$perc > 600)] <- 120

      # ggplot(y, aes(x=perc)) +
      #   geom_histogram(color="white", fill="green", bins=150) +
      #   geom_vline(data=y, aes(xintercept= 0), linetype="dashed") +
      #   labs(x = "%") +
      #   theme_minimal()

      # ggplot(y , aes(y = perc)) +
      #   geom_boxplot()


      if (median(y$perc, na.rm = T) < 0) {
        c <- "blue"
      }
      else {
        c <- "red"
      }

      # boxplot(y$perc, col = c, outline=FALSE, ylab = "Change in %", main = "Overal Traffic Change in Ireland")

      ggplot(y, aes(x = perc)) +
        geom_histogram(color = c, fill = c, bins = 50) +
        geom_vline(data = y, aes(xintercept = 0), linetype = "dashed") +
        labs(x = "%", title = "Overal Traffic Change in Ireland") +
        theme_minimal()
    }
    else {
      return()
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)
