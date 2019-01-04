# Install packages ----
#devtools::install_github("hadley/tidyverse")

# Load packages ----
library(shiny)
library(tidyverse)
library(plotly)
#library(lubridate)
#library(RColorBrewer)

# Time zone ----
Sys.setenv(TZ="Africa/Bamako") #Sys.getenv("TZ") # to check


# Load data ----
load(url("https://github.com/fousseynoubah/ssm/blob/master/ssm_data.RData?raw=true"))

# User interface ----
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      selectInput("variable", 
                  label = "Variable",
                  choices = c("Total" = "total",
                              "Location" = "location" ,
                              "Type" = "type",
                              "Deadly" = "deadly"),
                  selected = "total")
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
             type = ifelse(is.na(type), "Autre", type),
             deadly = ifelse(!is.na(nbr_morts) & nbr_morts > 0, 1, 0),
             deadly = factor(deadly,
                             levels = c(0, 1), 
                             labels = c("Not deadly", "Deadly"),
                             ordered = TRUE) ) %>% 
      group_by(year = annee, month = mois, total, location, type, deadly) %>%
      summarise(incidents = n()) %>% 
      mutate(date = as.Date(paste(year, month, "01", sep = "-"))) %>%
      ungroup() %>% 
      select(-c(year, month)) %>% 
      gather(key = variable, value = category, -date, -incidents) %>% 
      mutate(category = factor(category,
                               levels = c("Total", "Point", "Ligne", "ND", "Attaque", "Affrontement", "Autre", "Not deadly", "Deadly"), # 
                               labels = c("Total", "Point", "Line", "ND", "Attack", "Clash", "Other", "Not deadly", "Deadly"), # 
                               ordered = TRUE)) %>% 
      filter(variable == input$variable)
      })

  output$plot <- renderPlotly({
    plot_ly(dataset()) %>% 
      add_trace(x = ~date, y = ~incidents, color =~category, type = "bar") %>% 
      layout(title = "Number of incidents, per month",
             xaxis = list(title = "Date"), #, domain = c(0.075, 1)
             yaxis = list(title = "Incidents"),
             barmode = 'stack',
             legend = list(x = 0.05, y = 0.925,
                           bgcolor = "transparent")) #orientation = 'h'
    
    }
  )
  }

# Run app ----
shinyApp(ui, server)