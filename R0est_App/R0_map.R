# Amin Shoari Nejad
library(dplyr)
library(shinydashboard)
library(shinyjs)
library(tidycovid19)
library(lubridate)
library(tidyverse)
library(reshape2)
library(ggplot2)
library(flipTime)
library(shiny)
library(shinyBS)
library(shinyWidgets)
library(shinythemes)
library(plotly)
library(shinycssloaders)
library(sf)
library(rgdal)
library(leaflet)
library(leaflet.extras)
library(tigris)
library(spdplyr)
library("ggdendro")
library("reshape2")
library(scales)
library(shinyalert)
library(shinybusy)
library("gapminder")

rm(list = ls())

latest = download_merged_data(silent = TRUE, cached = TRUE)
load("r0_predictions.rda")
world = readOGR(dsn="world", layer="World_Countries__Generalized_")
#world = readOGR(dsn="Americas", layer="Americas")
#world = readOGR(dsn="Europe", layer="Europe")
#world = readOGR(dsn="Africa", layer="afr_adm")

#names(world)[1] <- "COUNTRY"
#names(world)[6] <- "COUNTRY"

# America <- data.frame(
# country = c("Greenland", "Canada", "United States", "Bermuda", "Cayman Islands", "Turks & Caicos Islands", "Cuba", "Mexico", "Anguilla", "British Virgin Islands", "Haiti", "Jamaica", "Sint Maarten", "Puerto Rico", "Dominican Republic", "Antigua & Barbuda", "Guatemala", "El Salvador", "U.S. Virgin Islands", "Curaçao", "Dominica", "Honduras", "Bahamas", "Panama", "Belize", "St. Kitts & Nevis", "Barbados", "Nicaragua", "Costa Rica", "Guyana", "Aruba", "Suriname", "Grenada", "St. Vincent & Grenadines", "Ecuador", "Venezuela", "Peru", "Caribbean Netherlands", "Colombia", "Brazil", "St. Lucia", "Trinidad & Tobago", "Paraguay", "Bolivia", "Uruguay", "Chile", "Argentina", "Falkland Islands")
# )
# 
# Asia 
# name = c("Kyrgyzstan", "Japan", "Mongolia", "Kazakhstan", "Uzbekistan", "Tajikistan", "Armenia", "Azerbaijan", "Georgia", "Turkey", "Lebanon", "Syria", "Cyprus", "Iraq", "Iran", "Afghanistan", "Nepal", "China", "South Korea", "Taiwan", "Laos", "Myanmar (Burma)", "Bhutan", "Bangladesh", "India", "Pakistan", "Bahrain", "Kuwait", "Jordan", "Israel", "Palestinian Territories", "Saudi Arabia", "Qatar", "United Arab Emirates", "Thailand", "Cambodia", "Vietnam", "Philippines", "Malaysia", "Sri Lanka", "Oman", "Yemen", "Indonesia", "Brunei", "Singapore", "Maldives", "Timor-Leste"),


ui <- dashboardPage(
  dashboardHeader(title = "COVID-19 R Estimator"),
  dashboardSidebar(useShinyjs(),
                   fluidRow(
                     column(width=12,
                            
                            
                            dateInput("date_end",
                                      label = "End of two week period to estimate R:",
                                      max(latest$date)),
                            
                            pickerInput("sel_cty",
                                        "Select a region:", 
                                        choices = c("World","Europe", "North America","South America", "Asia", "Africa", "Oceania"),
                                        selected = c("World"),
                                        options = list(`actions-box` = TRUE,
                                                       `live-search` = TRUE),
                                        multiple = FALSE),
                     ))
  ),
  
  dashboardBody(
    tags$style(type = "text/css", "#map {height: calc(100vh - 80px) !important;}"),
    
    # Boxes need to be put in a row (or column)
    fluidRow(
      
      p(HTML("<center>Click on the map to see the country-specific estimated R confidence intervals.</center>")),
      leafletOutput("view", height=1000) %>% withSpinner(color="#1E90FF"),
      #plotOutput("R_estim", width = "100%", height = "800px") %>% withSpinner(color="#1E90FF"),
      

    )
  )
)

server <- function(input, output) {
  
  
  
  
  output$view <- renderLeaflet({
    
    data_use = latest %>% 
      group_by(country) %>% 
      mutate(cum_cases = ecdc_cases,
             cases = c(cum_cases[1], diff(ecdc_cases))) %>% 
      ungroup() %>% 
      dplyr::select(country,date,cum_cases,cases,population) %>% 
      filter(date >= input$date_end - 14, date <= input$date_end) %>% 
      na.omit()
    
    data_use$country[which(data_use$country == "Czechia")] <- "Czech Republic"
    r0_predictions$country[which(r0_predictions$country == "Czechia")] <- "Czech Republic"
    
    
    # COVID generation time
    #America = gapminder %>% filter(continent == input$sel_cty) %>% select(country)
    #r0_predictions <- left_join(America,r0_predictions)
    
    estR0 = r0_predictions %>% 
      group_by(country) %>% 
      dplyr::mutate(n_rows = length(country) - 1) %>% 
      do( data.frame(., date = seq.Date(Sys.Date() - min(.$n_rows), Sys.Date(),  by = 1))) %>% 
      dplyr::filter(date == input$date_end)
    
    estR0 = estR0 %>% mutate(R_est = signif(pred, 3))
    
    
    df3 <- geo_join(world, estR0,"COUNTRY", "country")
    
    df3 <- df3 %>% dplyr::mutate(pop = paste0("Estimated R0: ",df3$R_est))
    
    pal <- colorNumeric(palette = c("white","gray","darkblue"), domain = df3$R_est)
    
    popup_sb <- df3$pop
    
     # df3 <- st_as_sf(df3)
     # names(America) <- "COUNTRY"
     # df3 <- left_join(America,df3)
     # df3 <- st_as_sf(df3)
    
    if(input$sel_cty == "Europe"){
                                     long = 9.5217 
                                     latt = 50.3996
                                     zm = 4
    }
    
    if(input$sel_cty == "North America"){
      long = -101.088
      latt = 39.682
      zm = 3
    }
    
    if(input$sel_cty == "Africa"){
      long = 24.74055
      latt = -20.3560
      zm = 3
    }
    
    if(input$sel_cty == "South America"){
      long = -63.15007
      latt = -37.9017
      zm = 3
    }
    
    if(input$sel_cty == "Asia"){
      long = 92.4182
      latt = 23.8828
      zm = 4
    }
    
    if(input$sel_cty == "Oceania"){
      long = 156.4685
      latt = -34.7081
      zm = 3
    }
    
    if(input$sel_cty == "World"){
      long = -7.5959
      latt = 0
      zm = 2
    }
    
    leaflet() %>%
      addTiles()  %>% setView(long, latt, zoom = zm) %>%
      addPolygons(data = df3, fillColor = ~pal(df3$R_est), layerId= ~COUNTRY,
                  fillOpacity = 0.8,
                  weight = 0.2,
                  smoothFactor = 0.2,
                  highlight = highlightOptions(
                    weight = 5,
                    color = "#666",
                    fillOpacity = 0.2,
                    bringToFront = TRUE),
                  label=popup_sb,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto")) %>%
      addLegend(pal = pal, values = df3$R_est, title = "R0 Estimates", opacity = 0.7) %>%
      leaflet.extras::addResetMapButton() %>% 
      leaflet.extras::addSearchOSM(options = searchOptions(collapsed = T,zoom = 9,hideMarkerOnCollapse = T, moveToLocation = FALSE,
                                                           autoCollapse =T))
    
    
    
  }) %>% bindCache(input$sel_cty, input$date_end) 
  
  
  observeEvent(input$view_shape_click,{#Plotting R plots for each region after clicking on the map
    
    
    output$plot2 <- renderPlotly({
      
      
      current_country <- input$view_shape_click$id
      date_max <- input$date_end
      
      
      latest_filter <- latest %>% 
        dplyr::filter(country == current_country) %>% 
        dplyr::mutate(cum_cases = ecdc_cases,
                      cases = c(cum_cases[1], diff(ecdc_cases))) %>%
        dplyr::select(date, cases, population) %>%
        dplyr::filter(date >= date_max - 14, date <= date_max) %>%
        na.omit()
      
      estR0 = r0_predictions %>%
        dplyr::filter(country == current_country) 
      
      n_dates <- seq.Date(Sys.Date() - nrow(estR0) + 1, Sys.Date(),  by = 1)
      
      estR0 = estR0 %>% 
        dplyr::mutate(date = n_dates) %>% 
        dplyr::filter(date == date_max)
      
      
      
      p = ggplot(data = latest_filter, aes(x = date, y = cases)) + 
        geom_point() + 
        labs(x = 'Date',
             y = 'Cases',
             title = paste('Cases in',current_country, 'from', 
                           format(input$date_end - 14, '%d-%b'), 'to',
                           format(input$date_end, '%d-%b'))) + 
        theme_bw() + 
        geom_smooth(se = FALSE)
      
      ggp <- ggplot_build(p)
      yrange = ggp$layout$panel_params[[1]]$y.range
      xrange = ggp$layout$panel_params[[1]]$x.range
      
      # Add the annotation
      a <- list(
        x = ggp$layout$panel_scales_x[[1]]$range$range[1],
        y = ggp$layout$panel_scales_y[[1]]$range$range[2],
        xref = "x",
        yref = "y",
        xanchor = 'left',
        showarrow = FALSE,
        font = list(size = 20)
      )
      
      #if(nrow(estR0) == 0 | any(data_use$cases < 10)) {
      if(nrow(estR0) == 0) {
        a$text = "R0 not estimated (bad case values or date range)"
        a$font = list(size = 14)
      } else {
        #if(input$R_method == "SB") {
        R_est = signif(estR0$pred, 3)
        R_low = signif(estR0$low, 3)
        R_high = signif(estR0$upp, 3)
        #} else {
        
        a$text = paste0("Estimated R = ", R_est,
                        ",  10-90 Quantile Interval: (", R_low,', ',
                        R_high, ')')
      }
      
      ggplotly(p) %>% layout(annotations = a)  
      
      
    }) #%>% bindEvent(input$view_shape_click)
    
    
  }) #End of Observation
  
  observeEvent(input$view_shape_click,{
    
    showModal(modalDialog(
      title = "",
      size = "l",
      footer = actionButton("close", "Close"),
      plotlyOutput("plot2") %>% withSpinner(color="#1E90FF") ))
    
  })
  
  
  observeEvent(input$close, { #Removing modal and erasing previous plot
    
    output$plot2 <- NULL
    removeModal()
    
  })
  
}

shinyApp(ui, server)
