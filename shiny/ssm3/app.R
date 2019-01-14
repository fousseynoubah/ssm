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
      selectInput("varchoice", 
                  label = "Variable",
                  choices = c("Incidents" = "incidents",
                              "Dead" = "dead" ,
                              "Injured" = "injured"),
                  selected = "incidents")
      ),

    mainPanel(plotlyOutput("plot"))
  )
  )

# Server logic ----
server <- function(input, output) {

  # Data frame (1)
  dataoverall <- reactive({
    incidents %>%
      group_by(year = annee, month = mois) %>%
      summarise(incidents = n(), dead = sum(nbr_morts, na.rm = TRUE), injured = sum(nbr_blesses, na.rm = TRUE)) %>% 
      mutate(date = as.Date(paste(year, month, "01", sep = "-"))) %>%
      ungroup() %>% 
      select(-year, -month) %>% 
      gather(key = variable, value = value, -date) %>% 
      group_by(variable) %>%
      mutate(cumsum = cumsum(value),
             id = case_when(variable == "incidents" ~ 1,
                            variable == "dead" ~ 2,
                            variable == "injured" ~ 3)) %>% 
      filter(variable == input$varchoice)
      })

  # Data frame (2)
  datayear <- reactive({
    incidents %>%
      mutate(week = lubridate::week(date)) %>% 
      group_by(year = annee, week) %>%
      summarise(incidents = n(), dead = sum(nbr_morts, na.rm = TRUE), injured = sum(nbr_blesses, na.rm = TRUE)) %>% 
      gather(key = variable, value = value, -year, -week) %>% 
      mutate(id = case_when(variable == "incidents" ~ 1,
                            variable == "dead" ~ 2,
                            variable == "injured" ~ 3)) %>% 
      group_by(year, variable) %>%
      mutate(cumsum = cumsum(value)) %>% 
      filter(variable == input$varchoice)
  })
  
    output$plot <- renderPlotly({
      p1 <- plot_ly(dataoverall(), x = ~date, y = ~cumsum, mode = "lines", name = "Overall") %>% 
        layout(title = "Cumulative count",
               xaxis = list(title = "Month"),
               yaxis = list(title = "Cumulative count"),
               legend = list(x = 0.05, y = 0.95))
      
      p2 <- plot_ly(datayear(), x = ~week, y = ~cumsum, linetype = ~year, mode = "lines") %>% 
        layout(title = "Cumulative count",
               xaxis = list(title = "Week"),
               yaxis = list(title = "Cumulative count"),
               legend = list(x = 0.05, y = 0.95,
                             bgcolor = 'transparent'))
      
      subplot(p1, p2, margin = 0.05, titleY = TRUE, titleX = TRUE)
    }
  )
  }

# Run app ----
shinyApp(ui, server)