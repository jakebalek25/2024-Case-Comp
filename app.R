# load in packages
library(shiny)
library(DT)
library(ggplot2)
library(dplyr)

# Load in data
fopa_hitter_final <- read.csv("hitter_data.csv")
fopa_pitcher_final <- read.csv("pitcher_data.csv")
team_avg_hitter <- read.csv("team_average_hitter.csv")

# Define a function to generate plots for each team
generate_team_plots <- function(team_avg) {
  max_fopa <- max(team_avg_hitter$sum_fopa_bat_order)
  min_fopa <- min(team_avg_hitter$sum_fopa_bat_order)
  team_plots <- lapply(unique(team_avg_hitter$Team.x), function(team) {
    team_data <- team_avg[team_avg_hitter$Team.x == team, ]  # Filter data for the current team
    # Create a bar plot for the current team
    ggplot(team_data, aes(x = factor(BatOrder), y = sum_fopa_bat_order)) +
      geom_bar(stat = "identity", fill = "blue") +
      labs(title = paste("Average Foul Ball Plus for", team),
           x = "Bat Order Position",
           y = "Sum FOPA") +
      coord_cartesian(ylim = c(min_fopa, max_fopa)) +  # Set y-axis limits
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
  })
  return(team_plots)
}


# Define server function
server <- function(input, output) {
  
  # Reactive function to select data based on user's choice
  selected_data <- reactive({
    if (input$select_data == "Hitters") {
      # Order hitters data frame by fopa in ascending order
      return(fopa_hitter_final[order(fopa_hitter_final$fopa), ])
    } else if (input$select_data == "Pitchers") {
      # Order pitchers data frame by fopa in descending order
      return(fopa_pitcher_final[order(fopa_pitcher_final$fopa, decreasing = TRUE), ])
    }
  })
  
  # Filter data based on selections and select desired columns
  output$table <- DT::renderDataTable(DT::datatable({
    data <- selected_data()  # Use the reactive data
    data$fopa <- round(data$fopa, 3)
    data <- data[, c("last_name..first_name", "fopa")]
    # Set custom column names for display
    colnames(data) <- c("Player Name", "FOPA")  # Reordered column names
    data
  }, rownames = FALSE))
  
  # Generate plot outputs for selected teams
  output$team_plot1 <- renderPlot({
    if (!is.null(input$select_team1)) {
      max_fopa <- max(team_avg_hitter$sum_fopa_bat_order)
      min_fopa <- min(team_avg_hitter$sum_fopa_bat_order)
      
      team_data <- team_avg_hitter[team_avg_hitter$Team.x == input$select_team1, ]
      ggplot(team_data, aes(x = factor(BatOrder), y = sum_fopa_bat_order)) +
        geom_bar(stat = "identity", fill = "blue") +
        labs(title = paste("FOPA by Lineup", input$select_team1),
             x = "Bat Order Position",
             y = "Sum FOPA") +
        coord_cartesian(ylim = c(min_fopa, max_fopa)) +  # Set y-axis limits
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
    }
  })
  
  output$team_plot2 <- renderPlot({
    if (!is.null(input$select_team2)) {
      max_fopa <- max(team_avg_hitter$sum_fopa_bat_order)
      min_fopa <- min(team_avg_hitter$sum_fopa_bat_order)
      
      team_data <- team_avg_hitter[team_avg_hitter$Team.x == input$select_team2, ]
      ggplot(team_data, aes(x = factor(BatOrder), y = sum_fopa_bat_order)) +
        geom_bar(stat = "identity", fill = "red") +
        labs(title = paste("FOPA by Lineup", input$select_team2),
             x = "Bat Order Position",
             y = "Sum FOPA") +
        coord_cartesian(ylim = c(min_fopa, max_fopa)) +  # Set y-axis limits
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
    }
  })
  
  
  # Determine which plot to render based on the selection in the dropdown menu
  observe({
    if (input$select_plot == "HITTER Two-Strike Analysis") {
      output$plot_output <- renderPlot({
        max_fopa <- max(fopa_hitter_final$fopa)
        min_fopa <- min(fopa_hitter_final$fopa)
        
        fopa_hitter_final %>% 
          filter(n_obs >= 100) %>% 
          ggplot(aes(x = avg_two_s_foul, y = fopa, color = n_obs)) +
          geom_point() +
          theme_bw() +
          labs(title = "Hitters FOPA vs. Two-Strike Foul %",
               subtitle = "Minimum 100 Foul Balls",
               color = "# Foul Balls") +
          xlab("Two-Strike Foul %") +
          ylab("FOPA") +
          theme(plot.title = element_text(hjust = 0.5),
                plot.subtitle = element_text(hjust = 0.5))
      })
    } else if (input$select_plot == "HITTER Chase Percentage Analysis") {
      output$plot_output <- renderPlot({
        fopa_hitter_final %>% 
          filter(n_obs >= 100) %>% 
          ggplot(aes(x = oz_swing_percent/100, y = fopa, color = n_obs)) +
          geom_point() +
          theme_bw() +
          labs(title = "Hitters FOPA vs. Chase %",
               subtitle = "Minimum 100 Foul Balls",
               color = "# Foul Balls") +
          xlab("Chase %") +
          ylab("FOPA") +
          theme(plot.title = element_text(hjust = 0.5),
                plot.subtitle = element_text(hjust = 0.5))
      })
    } else if (input$select_plot == "HITTER At-Bat Length Analysis") {
      output$plot_output <- renderPlot({
        fopa_hitter_final %>% 
          filter(n_obs >= 100) %>% 
          ggplot(aes(x = avg_ab_len, y = fopa, color = n_obs)) +
          geom_point() +
          theme_bw() +
          labs(title = "Hitters FOPA vs. At-Bat Length",
               subtitle = "Minimum 100 Foul Balls",
               color = "# Foul Balls") +
          xlab("Average At-Bat Length") +
          ylab("FOPA") +
          theme(plot.title = element_text(hjust = 0.5),
                plot.subtitle = element_text(hjust = 0.5))
      })
    } else if (input$select_plot == "PITCHER Two-Strike Analysis") {
      output$plot_output <- renderPlot({
        fopa_pitcher_final %>% 
          filter(n_obs >= 100) %>% 
          ggplot(aes(x = avg_two_s_foul, y = fopa, color = n_obs)) +
          geom_point() +
          theme_bw() +
          labs(title = "Pitchers FOPA vs. Two-Strike Foul %",
               subtitle = "Minimum 100 Foul Balls",
               color = "# Foul Balls") +
          xlab("Two Strike Foul %") +
          ylab("FOPA") +
          theme(plot.title = element_text(hjust = 0.5),
                plot.subtitle = element_text(hjust = 0.5))
      })
    } else if (input$select_plot == "PITCHER Chase Percentage Analysis") {
      output$plot_output <- renderPlot({
        fopa_pitcher_final %>% 
          filter(n_obs >= 100) %>% 
          ggplot(aes(x = oz_swing_percent/100, y = fopa, color = n_obs)) +
          geom_point() +
          theme_bw() +
          labs(title = "Pitchers FOPA vs. Chase %",
               subtitle = "Minimum 100 Foul Balls",
               color = "# Foul Balls") +
          xlab("Chase %") +
          ylab("FOPA") +
          theme(plot.title = element_text(hjust = 0.5),
                plot.subtitle = element_text(hjust = 0.5))
      })
    } else if (input$select_plot == "PITCHER At-Bat Length Analysis") {
      output$plot_output <- renderPlot({
        fopa_pitcher_final %>% 
          filter(n_obs >= 100) %>% 
          ggplot(aes(x = avg_ab_len, y = fopa, color = n_obs)) +
          geom_point() +
          theme_bw() +
          labs(title = "Pitchers FOPA vs. At-Bat Length",
               subtitle = "Minimum 100 Foul Balls",
               color = "# Foul Balls") +
          xlab("Average At-Bat Length") +
          ylab("FOPA") +
          theme(plot.title = element_text(hjust = 0.5),
                plot.subtitle = element_text(hjust = 0.5))
      })
    }
  })
}

# Define UI
ui <- fluidPage(
  tags$head(
    tags$style(".title { color: white; background-color: #10137e; text-align: center; padding: 20px 0; }
                .footer { background-color: #10137e; color: white; text-align: center; padding: 10px 0; }
                .dataTables_length { display: none; }")  # Hide the "Show entries" dropdown
  ),
  titlePanel(HTML("<h1 class='title'>FOPA Metric</h1>")),
  
  # Tabbed layout
  tabsetPanel(
    tabPanel("Leaderboard",  # First tab for leaderboard
             sidebarLayout(
               sidebarPanel(width = "100px",
                            # Select data type
                            selectInput("select_data", "Select Player Type:", 
                                        choices = c("Hitters", "Pitchers"),
                                        selected = "Hitters")
               ),
               mainPanel(
                 # Create a new row for the table
                 DT::dataTableOutput("table")
               )
             )
    ),
    tabPanel("Plots", 
             selectInput("select_plot", "Select Plot:", 
                         choices = c("HITTER Two-Strike Analysis", "HITTER Chase Percentage Analysis", "HITTER At-Bat Length Analysis", 
                                     "PITCHER Two-Strike Analysis", "PITCHER Chase Percentage Analysis", "PITCHER At-Bat Length Analysis")),
             plotOutput("plot_output") # Placeholder for selected plot
    ),  # Second tab for plots (to be added later)
    tabPanel("Team Distributions",
             selectInput("select_team1", "Select Team 1:", 
                         choices = unique(team_avg_hitter$Team.x), selected = NULL),
             selectInput("select_team2", "Select Team 2:", 
                         choices = unique(team_avg_hitter$Team.x), selected = NULL),
             plotOutput("team_plot1"),
             plotOutput("team_plot2")
    )
  ),
  
  # Footer
  fluidRow(
    column(
      width = 12,
      div(
        HTML("<p class='footer'>Contributors: Nathan Backman, Jake Balek, Hunter Geise, Danielle Napierski, and Nolan Pittman.</p>")
      )
    )
  )
)

# Run the application
shinyApp(ui = ui, server = server)
