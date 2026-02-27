library(shiny)
library(dplyr)
library(ggplot2)
library(ggsoccer)

############################################################
# 1) KLARGØR SKUDDATA KUN FOR SÆSON 189918 + 191611
############################################################

valid_seasons <- c(189918, 191611)

# Tæl mål i skuddata pr. kamp
goal_count <- shots_plotdata %>%
  filter(is_goal == "Mål") %>%
  count(MATCH_WYID, name = "goals_in_shots")

# Tilføj mål fra df_oversigt og marker mismatch

match_choices <- df_oversigt %>%
  filter(SEASON_WYID %in% valid_seasons) %>%
  left_join(goal_count, by = "MATCH_WYID") %>%
  mutate(
    goals_in_shots = ifelse(is.na(goals_in_shots), 0, goals_in_shots),
    label_with_season = paste0(MATCHLABEL, " (", SEASON_WYID, ")")
  ) %>%
  arrange(label_with_season)


names(shots_master)


############################################################
# 2) UI
############################################################

ui <- fluidPage(
  titlePanel("Opgave 4.3 – Skud i åben spil i en kamp"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "match_id",
        "Vælg kamp:",
        choices = setNames(match_choices$MATCH_WYID,
                           match_choices$label_with_season)
      )
    ),
    mainPanel(
      plotOutput("shotplot", height = "650px")
    )
  )
)

############################################################
# 3) SERVER
############################################################

server <- function(input, output, session) {
  
  kamp_skud <- reactive({
    shots_plotdata %>%
      filter(MATCH_WYID == input$match_id)
  })
  
  output$shotplot <- renderPlot({
    df <- kamp_skud()
    
    ggplot() +
      annotate_pitch(colour = "grey70", fill = "white") +
      theme_pitch() +
      
      geom_point(
        data = df,
        aes(
          x = x,
          y = y,
          color = TEAMNAME,
          shape = is_goal,
          size = is_goal
        ),
        alpha = 0.85
      ) +
      
      # Mål vs ikke mål
      scale_shape_manual(values = c("Ikke mål" = 1, "Mål" = 16)) +
      scale_size_manual(values = c("Ikke mål" = 3, "Mål" = 6)) +
      
      # Automatisk farveskala (virker for ALLE holdnavne)
      scale_color_discrete() +
      
      labs(
        title = "Skud i kampen",
        subtitle = match_choices$MATCHLABEL[
          match_choices$MATCH_WYID == input$match_id
        ],
        color = "Hold",
        shape = "Mål?",
        size = "Mål?"
      ) +
      
      coord_flip() +
      theme(
        legend.position = "bottom",
        plot.title = element_text(size = 18, face = "bold"),
        plot.subtitle = element_text(size = 14)
      )
  })
}

############################################################
# 4) RUN APP
############################################################

shinyApp(ui, server)

