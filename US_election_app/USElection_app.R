# Amin Shoari Nejad
library(readxl)
library(leaflet)
library(dplyr)
library(lubridate)
library(Jmisc)
library(tidyverse)
library(reshape2)
library(plotly)
require(scales)
library(shinycssloaders)


rm(list = ls())

ui <- fluidPage(
  titlePanel("Visualizing 2020 US Election Results"),
  hr(),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "state", "Choose a state:",
        c(
          "Pennsylvania" = "Pennsylvania",
          "Georgia" = "Georgia",
          "Nevada" = "Nevada",
          "Arizona" = "Arizona",
          "North Carolina" = "North Carolina",
          "Alaska" = "Alaska"
        )
      ),
    ),



    # Main panel for displaying outputs ----
    mainPanel(
      plotlyOutput("plot") %>% withSpinner(color = "#1E90FF"),
    )
  )
)


server <- function(input, output) {
  output$plot <- renderPlotly({
    ad <- read.csv("https://alex.github.io/nyt-2020-election-scraper/battleground-state-changes.csv")

    ad <- ad %>% mutate(timestamp = as.POSIXct(strptime(timestamp, "%Y-%m-%d %H:%M:%S")))

    df <- ad %>%
      filter(grepl(input$state, state)) %>%
      arrange(timestamp)

    df <- df %>% mutate(added_votes = (leading_candidate_votes - lag(leading_candidate_votes, 1)) + ((trailing_candidate_votes - lag(trailing_candidate_votes, 1))))

    df[1, 17] <- 0

    df <- df %>% mutate(total_votes = cumsum(added_votes))

    index <- df$leading_candidate_name != head(df$leading_candidate_name, 1)

    df$vote_differential[index] <- df$vote_differential[index] * (-1)

    df <- df %>% rename(time = timestamp)

    df$votes_remaining[which(df$state == "Georgia (EV: 16)")] <- df$votes_remaining + 25000

    p <- ggplot() +
      geom_point(data = df, aes(total_votes, vote_differential, label = time)) +
      geom_line(data = df, aes(total_votes, vote_differential)) +
      geom_vline(xintercept = min(df$votes_remaining) + tail(df$total_votes, 1), linetype = "dotted") +
      geom_hline(yintercept = 0, linetype = "dotted") +
      scale_y_continuous(labels = comma) +
      scale_x_continuous(labels = comma) +
      labs(y = paste0(head(df$leading_candidate_name, 1), "'s vote lead"), x = paste0("Votes counted since ", min(df$time), " GMT"), title = paste0(input$state, " vote difference")) +
      expand_limits(x = max(df$votes_remaining), y = -quantile(df$vote_differential, 0.5)) +
      annotate(
        geom = "text", x = min(df$votes_remaining) + tail(df$total_votes, 1),
        y = mean(df$vote_differential, na.rm = T), label = "End of counting", size = 4, fontface = "bold", color = "red", angle = 90
      ) +
      theme_bw()

    ggplotly(p)
  })
}

# Run the application
shinyApp(ui = ui, server = server)
