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
      selectInput(inputId = "myregion", 
                  label = i18n$t("Region"), 
                  choices = c("Kayes",
                              "Koulikouro",
                              "Sikasso",
                              "Segou",
                              "Mopti",
                              "Tombouctou",
                              "Gao",
                              "Kidal",
                              "Bamako"
                  ),
                  selected = "Bamako"
      )
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel(i18n$t("Map") , plotOutput("map")),
        tabPanel(i18n$t("Graph") , plotOutput("plot"))
      )
    )
  )
)

# Server logic ----
server <- function(input, output) {
  
  # Data frame
  dataset <- reactive({
    datatest <- 
      incidents.point %>% 
      filter(!is.na(date)) %>% 
      select(adm1_cod, adm1_name, year = annee, dead = nbr_morts, injured = nbr_blesses, point, longitude, latitude) %>% 
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
      group_by(adm1_cod, adm1_name, year, severity) %>%
      count() %>% 
      filter(year == input$myyear, adm1_name == input$myregion)
  })
  
  
  # Map
  output$map <- renderPlot({
    ggplot() +
      # Map / Carte
      geom_sf(data = map_adm1_sf %>% mutate(myfill = (Admin1_Nam == input$myregion )),
              mapping = aes(fill = myfill),
              colour = "grey30", size = 0.015) +
      theme(
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_line(colour = "transparent"),
        panel.background = element_rect(fill = "transparent", colour = "transparent"),
        legend.position = "none"
      ) +
      theme(plot.caption = element_text(size = 8)) +
      scale_fill_manual(values = c("grey20", "grey80")) +
      labs(x = "", y = "",
           subtitle = i18n$t("Identifying the region")
      )
  }
  )
  
  # Plot
  output$plot <- renderPlot({
    ggplot() +
      # Plot
      geom_col(data = dataset(),
               mapping = aes(x = severity, y = n, fill = severity)) +
      theme(plot.caption = element_text(size = 8)) +
      scale_fill_brewer(type = "seq", palette = "Reds", direction = -1) + 
      labs(x = "", y = "", fill = i18n$t("Severity"),
           subtitle = i18n$t("Agregating the incidents, by severity"),
           caption = i18n$t("Based on data from http://malilink.net/attaques-terroriste-au-mali/"))
    
  }
  )
  
}

# Run app ----
shinyApp(ui, server)
