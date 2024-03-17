library(shiny)
library(tidyverse)

ui <- fluidPage(
  # created a title called shiny_week8 in both the shinyapps account and in the URL to open it
  titlePanel("shiny_week8"),
  sidebarLayout(
    sidebarPanel(
      # I used radio buttons for all three options for consistency
      # add radio buttons to select which gender to display
      # placing "All" at the beginning of the choice options makes it the default
      radioButtons("Gender",
                   "I would like to display:",
                   choices = c("All", "Male", "Female")),
      # add radio buttons to select whether or not to display the error band
      # placing "Display Error Band" at the beginning of the choice options makes it the default
      radioButtons("Errorband",
                   "I would like to:",
                   choices = c("Display Error Band", "Supress Error Band")),
      # add radio buttons to select participants prior to July 2017 or not
      # placing "Yes" at the beginning of the choice options makes it the default
      radioButtons("Time",
                   "Would you like to include participants that completed the assessment before July 1, 2017?",
                   choices = c("Yes", "No"))
    ),
    mainPanel(
      # define what the output should be referenced as
      plotOutput("week8plot")
    )
  )
)

server <- function(input, output) {
  
  output$week8plot <- renderPlot({
  
    # read the minimal/skinny version of the dataset appropriate for shiny (saved in Part 2)
    skinny_tbl <- readRDS("skinny.rds")
  
    # Since "All" is not an option in the gender column, 
    # create an if(){} code that allows "All" to keep the full dataset
    # but then keeps only Male or Female in the dataset if they are selected
    if(input$Gender!="All"){
      skinny_tbl <- skinny_tbl %>%
        filter(gender == input$Gender)
    }
    
    # create an if(){} code that excludes participants who completed the 
    # assessment before July 1, 2017 if "No" is selected in the radio buttons
    if(input$Time=="No"){
      skinny_tbl <- skinny_tbl %>%
        filter(timeEnd>"2017-06-30 23:59:59")
    }

    # create the plot to show the data selected
    skinny_tbl %>%
      ggplot(aes(x=average1, y=average2)) +
      geom_point() +
      geom_smooth(method="lm", color="purple", 
                  # only display the error bands if "Display Error Band" 
                  # is selected in the radio buttons
                  se=if(input$Errorband=="Display Error Band"){TRUE}else{FALSE}) +
      labs(title = "Scatterplot between Mean Scores",
           x = "Mean Scores on Q1-Q6",
           y = "Mean Scores on Q8-Q10")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)