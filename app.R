library(shiny)
library(ggplot2)
library(readr)
library(dplyr)
library(httr)
library(knitr)
library(kableExtra)
library(DT)

# UI definition
ui <- fluidPage(
  titlePanel("Texas Longhorns Turnovers 2023-24"),
  sidebarLayout(
    sidebarPanel(
      selectInput("selectedPlayer", "Select Player:", choices = c("All" = "All"), selected = "All"),
      selectInput("selectedAdvancedType", "Select Advanced Turnover Type:", choices = c("All" = "All"), selected = "All"),
      selectInput("selectedHalf", "Select Game Half:", choices = c("All" = "All"), selected = "All"),
      checkboxGroupInput("homeAway", "Game Location:", choices = c("Home", "Away"), selected = c("Home", "Away"))
    ),
    mainPanel(
      plotOutput("basketballCourt"),
      plotOutput("turnoverDistributionChart") # Placeholder for the bar chart
    )
  )
)

# Server logic
server <- function(input, output, session) {
  url <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vRS6oSXOdGnnuEnx2-IHY3-Z5FIxh1o7kuO7UbdDN7zUlif6pnCiRP4XU5f93nfkuVmlGBf2Rmhdegn/pub?output=csv"
  res <- GET(url)
  turnovers <- read_csv(content(res, "text"), col_types = cols())  
  
  observe({
    updateSelectInput(session, "selectedPlayer", choices = c("All" = "All", unique(turnovers$Player)))
    updateSelectInput(session, "selectedAdvancedType", choices = c("All" = "All", unique(turnovers$`Advanced Type`)))
    updateSelectInput(session, "selectedHalf", choices = c("All" = "All", "1" = "1", "2" = "2", "OT" = "OT"))
  })
  output$basketballCourt <- renderPlot({
    zoomed_x_min <- 20
    zoomed_x_max <- 74
    zoomed_y_min <- 15
    zoomed_y_max <- 35
    outer_circle_radius <- 6
    paint_width <- 12
    paint_length <- 19
    free_throw_line_radius <- 6
    three_point_distance_to_baseline <- 25.5
    paint_top_edge <- (50 - paint_width) / 2
    paint_bottom_edge <- paint_top_edge + paint_width
    
    filtered_data <- turnovers %>%
      filter((Player == input$selectedPlayer | input$selectedPlayer == "All") &
               (`Advanced Type` == input$selectedAdvancedType | input$selectedAdvancedType == "All") &
               (`Home/Road` %in% input$homeAway) &
               (if (input$selectedHalf == "All") TRUE else if (input$selectedHalf == "OT") Half > 2 else Half == as.numeric(input$selectedHalf)))
    
    ggplot(filtered_data) +
      annotate("rect", xmin = 0, xmax = 94, ymin = 0, ymax = 50, fill = "burlywood2") +
      geom_rect(xmin = 0, xmax = 94, ymin = 0, ymax = 50, color = "#BF5700", fill = NA, size = 2) +
      geom_segment(aes(x = 47, xend = 47, y = 0, yend = 50), color = "#BF5700", linetype = "solid", size = 1) +
      annotate("rect", xmin = 0, xmax = paint_length, ymin = paint_top_edge, ymax = paint_bottom_edge, fill = NA, color = "#BF5700", size = 1.5) +
      annotate("rect", xmin = 94 - paint_length, xmax = 94, ymin = paint_top_edge, ymax = paint_bottom_edge, fill = NA, color = "#BF5700", size = 1.5) +
      geom_curve(aes(x = 0, y = 3, xend = 0, yend = 47), curvature = 1, color = "#BF5700", size = 1) +
      geom_curve(aes(x = 94, y = 3, xend = 94, yend = 47), curvature = -1, color = "#BF5700", size = 1) +
      geom_curve(aes(x = 47, y = 25 - outer_circle_radius, xend = 47, yend = 25 + outer_circle_radius), 
                 curvature = -1, color = "#BF5700", size = 1) +
      geom_curve(aes(x = 47, y = 25 + outer_circle_radius, xend = 47, yend = 25 - outer_circle_radius), 
                 curvature = -1, color = "#BF5700", size = 1) +
      geom_curve(aes(x = 47, y = 25 - outer_circle_radius, xend = 47, yend = 25 + outer_circle_radius), 
                 curvature = -1, color = "#BF5700", size = 1) +
      geom_curve(aes(x = 47, y = 25 + outer_circle_radius, xend = 47, yend = 25 - outer_circle_radius), 
                 curvature = 1, color = "#BF5700", size = 1) +
      scale_color_manual(values = c("Live Ball" = "black", "Dead Ball" = "red")) +
      geom_curve(aes(x = 19, y = 19, xend = 19, yend = 31), 
                 curvature = 1, color = "#BF5700", size = 1) +
      geom_curve(aes(x = 94 - paint_length, y = 19, xend = 94 - paint_length, yend = 25 + free_throw_line_radius), 
                 curvature = -1, color = "#BF5700", size = 1) +
      geom_point(aes(x = X, y = Y, color = Type), size = 4, alpha = 0.6) +
      xlim(0, 94) +
      ylim(0, 50) +
      labs(title = "Texas Longhorns Turnovers on the Basketball Court", x = "Court Length (feet)", y = "Court Width (feet)") +
      coord_fixed(ratio = 94/70) +
      theme_minimal()
  })
  output$turnoverDistributionChart <- renderPlot({
    if (input$selectedPlayer != "All") {
      filtered_data <- turnovers %>%
        filter(Player == input$selectedPlayer) %>%
        count(`Advanced Type`) %>%
        mutate(percentage = n / sum(n))
      
      ggplot(filtered_data, aes(x = `Advanced Type`, y = n, fill = `Advanced Type`)) +
        geom_bar(stat = "identity") +
        theme_minimal() +
        labs(title = paste("Distribution of Turnover Types for", input$selectedPlayer),
             x = "Type of Turnover", y = "Count") +
        scale_fill_brewer(palette = "Pastel1") +
        geom_text(aes(label = n), vjust = -0.5)
    } else {
      ggplot() + 
        labs(title = "Please select a player to see turnover distribution",
             x = "", y = "") +
        theme_void()
    }
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)

