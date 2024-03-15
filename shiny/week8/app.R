library(shiny)
library(tidyverse)
#week8_skinny_tbl <- readRDS("week8_skinny.RDS")

ui <- fluidPage(
    titlePanel("shiny_week8"),  # Application title
    sidebarLayout(
      sidebarPanel(
        radioButtons("Gender",
                     "I would like to display:",
                     choices = c("All", "Male", "Female")),
        radioButtons("Error Band",
                     "I would like to:",
                     choices = c("Display Error Band", "Suppress Error Band")),
        radioButtons("Participants",
                    "Would you like to include participants that completed 
                    the assessment before July 1, 2017?",
                    choices = c("Yes", "No"))
      ),
      mainPanel(
        plotOutput("week8plot")
      )
    )
)

server <- function(input, output) {

    output$week8Plot <- renderPlot({
        ggplot(week8_skinny_tbl, aes(x=average1, y=average2)) +
        geom_point() +
        geom_smooth(method="lm", color="purple") +
        labs(title = "Scatterplot between Mean Scores",
             x = "Mean Scores on Q1-Q6",
             y = "Mean Scores on Q8-Q10")
      })
}

# Run the application 
shinyApp(ui = ui, server = server)
