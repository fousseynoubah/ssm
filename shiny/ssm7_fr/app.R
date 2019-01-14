# Load packages ----
library(shiny)
library(shiny.i18n)
library(dplyr)
library(ggplot2)


# Time zone ----
Sys.setenv(TZ="Africa/Bamako") #Sys.getenv("TZ") # to check

# The json file with translations ----
i18n <- Translator$new(translation_json_path = "https://raw.githubusercontent.com/fousseynoubah/ssm/master/shiny/translations/translation.json")

# Choose the language
#i18n$set_translation_language("en")
i18n$set_translation_language("fr")

# Load data ----
load(url("https://github.com/fousseynoubah/ssm/blob/master/ssm_data.RData?raw=true"))


# User interface ----
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "myyear", 
                  label = i18n$t("Year"),
                  choices = c(2015, 2016, 2017, 2018), 
                  selected = 2015
      ),
      selectInput(inputId = "myseverity", 
                  label = i18n$t("Severity"), 
                  choices = c(i18n$t("Dead, Injured"), 
                              i18n$t("Dead, No injured"), 
                              i18n$t("No dead, Injured"), 
                              i18n$t("No dead, No injured")),
                  selected = i18n$t("Dead, Injured")
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
                               labels = c(
                                 i18n$t("Dead, Injured"), 
                                 i18n$t("Dead, No injured"), 
                                 i18n$t("No dead, Injured"), 
                                 i18n$t("No dead, No injured")),
                               ordered = TRUE)
      ) %>% 
      group_by(year, severity, point, latitude, longitude) %>%
      count() %>% 
      filter(year == input$myyear, severity == input$myseverity)
  })
  
  
  # Plot
  output$plot <- renderPlot({
    ggplot() +
      # Map / Carte
      geom_sf(data = map_adm2_sf, fill = "grey20", colour = "grey30", size = 0.015) +
      theme(
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_line(colour = "transparent"),
        panel.background = element_rect(fill = "transparent", colour = "transparent")
      ) +
      # Density / Densité
      stat_density2d(
        data = dataset(), 
        mapping = aes(x = longitude, y = latitude, fill = ..level.., alpha = ..level..), 
        geom = "polygon" # the polygons
      ) +
      geom_density2d(
        data = dataset(), 
        mapping = aes(x = longitude, y = latitude) # the lines
      ) + 
      theme(plot.caption = element_text(size = 8),
            legend.position = "none") + 
      labs(x = "", y = "", size = i18n$t("Number of incidents"), # en
           subtitle = i18n$t("Identifying the hotspots"),
           caption = i18n$t("Based on data from http://malilink.net/attaques-terroriste-au-mali/"))
    
  }
  )
}

# Run app ----
shinyApp(ui, server)
