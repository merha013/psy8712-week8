library(shiny)
library(tidyverse)

ui <- fluidPage(
    titlePanel("shiny_week8"),  # Application title
    sidebarLayout(
      sidebarPanel(
        # add radio buttons to select gender
        radioButtons("Gender",
                     "I would like to display:",
                     choices = c("All", "Male", "Female")),
        # add radio buttons to select error band or not
        radioButtons("Error Band",
                     "I would like to:",
                     choices = c("Display Error Band", "Suppress Error Band")),
        # add radio buttons to include early participants or not
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

  skinny_tbl <- readRDS("skinny.RDS")
  
    output$week8Plot <- renderPlot({
      # sliced_tbl <- skinny_tbl %>%
        # filter(gender == input$Gender) 
      ggplot(skinny_tbl, aes(x=average1, y=average2)) +
        geom_point() +
        geom_smooth(method="lm", se=TRUE, color="purple") +
        labs(title = "Scatterplot between Mean Scores",
             x = "Mean Scores on Q1-Q6",
             y = "Mean Scores on Q8-Q10")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
