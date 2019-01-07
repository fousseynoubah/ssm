# Install packages ----
#devtools::install_github("hadley/tidyverse") 

# Load packages ----
library(shiny)
library(shiny.i18n)
library(tidyverse)

translation <- readxl::read_excel(path = "shiny/ssm5/translation/translation.xlsx") 
jsonlite::write_json(translation, "shiny/ssm5/translation/translation.json")

i18n <- Translator$new(translation_json_path = "shiny/ssm5/translation/translation.json")



# Time zone ----
Sys.setenv(TZ="Africa/Bamako") #Sys.getenv("TZ") # to check


# Load data ----
load(url("https://github.com/fousseynoubah/ssm/blob/master/ssm_data.RData?raw=true"))

# User interface ----
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "myyear", 
                  label = "Year", # en
#                  label = "Year", # fr
                  choices = c("2015", "2016", "2017", "2018"), # en / fr
                  selected = "2015"
),
      selectInput(inputId = "myseverity", 
                  label = "Severity",  # en
                  choices = c("Dead, Injured", "Dead, No injured", "No dead, Injured", "No dead, No injured"),  # en
#                  choices = ("Morts, Bléssés", "Morts, Pas de bléssés", "Pas de morts, Bléssés", "Pas de morts, Pas de bléssés"),  # fr
                  selected = "Dead, Injured" # en
#                  selected = "Pas de morts, Pas de bléssés" # fr
)
    ),
    
    mainPanel(plotOutput("plot"))
  )
)

# Server logic ----
server <- function(input, output) {

  # Data frame
  dataset <- reactive({
    datatest <- 
      incidents.point %>% 
      filter(!is.na(date)) %>% 
      select(year = annee, dead = nbr_morts, injured = nbr_blesses, point, longitude, latitude) %>% 
      mutate(id = case_when((dead > 0 & !is.na(dead)) & (injured > 0 & !is.na(injured)) ~ 1,
                            (dead > 0 & !is.na(dead)) & (injured == 0 | is.na(injured)) ~ 2,
                            (dead == 0 | is.na(dead)) & (injured > 0 & !is.na(injured)) ~ 3,
                            (dead == 0 | is.na(dead)) & (injured == 0 | is.na(injured)) ~ 4),
             severity = factor(id, 
                                  levels = c(1, 2, 3, 4),
                                  labels = c("Dead, Injured", "Dead, No injured", "No dead, Injured", "No dead, No injured"), 
                                  ordered = TRUE)
      ) %>% 
      #  mutate(severity = severity_en) %>% # en
      #  mutate(severity = severity_fr) %>% # fr
      group_by(year, severity, point, latitude, longitude) %>%
      count() %>% 
      filter(year == input$myyear, severity == input$myseverity)
    #    filter(year == 2018, severity == "Dead, No injured")
  })
  
  
  # Plot
  output$plot <- renderPlot({
    
    ggplot() +
      # Map / Carte
      geom_sf(data = map_adm2_sf, fill = "grey35", colour = "grey45", size = 0.015) +
      theme(
        axis.text = element_blank(),
        axis.ticks = element_blank()
      ) +
      # Points / Points
      geom_point(
        data = dataset(),
        mapping = aes(x = longitude, y = latitude, size = n),
        alpha = 0.4, colour = "darkorange"
        ) + 
      theme(plot.caption = element_text(size = 8)) +
      labs(x = "", y = "", size = "Number of incidents", # en
           #  labs(x = "", y = "", size = "Nombre d'incidents", color = "Sévérité" , # en
           subtitle = paste("Mapping the points, by severity"),
           caption = "Based on data from http://malilink.net/attaques-terroriste-au-mali/" # en
           #       caption = "A partir de données tirées de http://malilink.net/attaques-terroriste-au-mali/" # fr
      )
    
  }
  )
}

# Run app ----
shinyApp(ui, server)
