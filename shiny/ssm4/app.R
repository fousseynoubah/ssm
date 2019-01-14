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
  datamap <- reactive({
    incidents.point %>%
      group_by(year = annee, longitude, latitude, point) %>% 
      summarise(incidents = n(), dead = sum(nbr_morts, na.rm = TRUE), injured = sum(nbr_blesses, na.rm = TRUE)) %>% 
      gather(key = variable, value = value, -year, -point, - longitude, -latitude) %>% 
      mutate(label = paste(point,"-", year, ":", value, variable) ) %>% 
      filter(value > 0) %>% # removing all the zeros to only show places with more than 1
      filter(variable == input$varchoice)
      })

    output$plot <- renderPlotly({
      
      geo <- c(
        list(
          scope = 'africa',
          showframe = F,
          showland = T,
          landcolor = toRGB("grey30")),
        resolution = 50,
        showcoastlines = T,
        countrycolor = toRGB("white"),
        coastlinecolor = toRGB("white"),
        projection = list(type = 'Mercator'), #azimuthal equal area
        list(lonaxis = list(range = c(-12.5, +4.5))),
        list(lataxis = list(range = c(10, 25))),
        list(domain = list(x = c(0, 1), y = c(0, 1)))
      )
      
      datamap() %>% 
        plot_geo(locationmode = 'country names', sizes = c(1, 600), color = I("black")) %>% 
        add_markers(x = ~longitude, y = ~latitude, 
                    color = ~factor(year), size = ~value, geo = geo, 
                    #hoverinfo = "text",
                    text = ~label
                    ) %>% 
        layout(geo = geo)
      }
  )
  }

# Run app ----
shinyApp(ui, server)