# Install packages ----
#devtools::install_github("hadley/tidyverse")

# Time zone ----
Sys.setenv(TZ="Africa/Bamako") #Sys.getenv("TZ") # to check

# Load packages ----
library(shiny)
library(tidyverse)
library(plotly)
#library(lubridate)
#library(RColorBrewer)

# Load data ----
load(url("https://github.com/fousseynoubah/ssm/blob/master/ssm_data.RData?raw=true"))

# User interface ----
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      selectInput("criterion", 
                  label = "Variable",
                  choices = c("Total" = "total",
                              "Location" = "location" ,
                              "Type" = "type"),
                  selected = "total"),
      selectInput("variable", 
                  label = "Statistics",
                  choices = c("Simple count" = "incidents",
                              "Cumulative count (year)" = "cumsum_year" ,
                              "Cumulative count (overall)" = "cumsum_all"),
                  selected = "incidents")
      ),

    mainPanel(plotlyOutput("plot"))
  )
  )

# Server logic ----
server <- function(input, output) {

  # Data frame
  dataset <- reactive({
    datatest <- 
      incidents %>%
      mutate(total = "Total",
             location = case_when(!is.na(point) ~ "Point", 
                                  !is.na(depart) & !is.na(arrivee) ~ "Ligne",
                                  !is.na(point) & !is.na(depart) & !is.na(arrivee) ~ "ND"),
             location = ifelse(is.na(location), "ND", location),
             type = ifelse(!(type_incident == "Attaque" | type_incident == "Affrontement"), "Autre", type_incident),
             type = ifelse(is.na(type), "Autre", type) ) %>% 
      group_by(year = annee, month = mois, total, location, type) %>%
      summarise(incidents = n()) %>% 
      mutate(date = as.Date(paste(year, month, "01", sep = "-"))) %>%
      gather(key = criterion, value = category, -date, -incidents, -year, -month ) %>% 
      mutate(category = factor(category,
                               levels = c("Total", "Point", "Ligne", "ND", "Attaque", "Affrontement", "Autre"), # 
                               labels = c("Total", "Point", "Line", "ND", "Attack", "Clash", "Other"), # 
                               ordered = TRUE)) %>% 
      group_by(year, criterion, category) %>% 
      mutate(cumsum_year = cumsum(incidents))%>% 
      ungroup() %>% 
      group_by(criterion, category) %>% 
      mutate(cumsum_all = cumsum(incidents)) %>% 
      gather(key = variable, value = nombre, -year, - month, -date, - criterion, - category) %>% 
      filter(variable == input$variable, 
             criterion == input$criterion)
      })

  output$plot <- renderPlotly({
    plot_ly(dataset()) %>% 
      add_trace(x = ~date, 
                y = ~nombre,
                color = ~category,
                type = 'scatter', 
                mode = 'lines') %>% 
      layout(title = "Number of incidents, per month",
             xaxis = list(title = "Date"),
             yaxis = list(title = "Incidents"),
             legend = list(x = 0.05, y = 0.925)
             )
    }
  )
  }

# Run app ----
shinyApp(ui, server)