library(shiny)
library(tidyverse)
library(rsconnect)

ui <- fluidPage(
  # I made shiny_week8 the title in the shinyapps account and the URL.
  titlePanel("shiny_week8"),
  sidebarLayout(
    sidebarPanel(
      # I used radio buttons for all three options for consistency.
      # I created radio buttons to select which gender to display, and
      # placing "All" at the beginning of the choice options makes it
      # the default.
      radioButtons("Gender",
                   "I would like to display:",
                   choices = c("All", "Male", "Female")),
      # I added radio buttons to select whether or not to display the 
      # error band. Placing "Display Error Band" at the beginning of 
      # the choice options makes it the default.
      radioButtons("Errorband",
                   "I would like to:",
                   choices = c("Display Error Band", "Supress Error Band")),
      # I added radio buttons to select participants prior to July 2017 
      # or not. Placing "Yes" at the beginning of the choice options makes 
      # it the default.
      radioButtons("Time",
                   "Would you like to include participants that completed 
                   the assessment before July 1, 2017?",
                   choices = c("Yes", "No"))
    ),
    mainPanel(
      # I defined week8plot as what the output should be referenced as.
      plotOutput("week8plot")
    )
  )
)

server <- function(input, output) {
  
  output$week8plot <- renderPlot({
  
    # I read the minimal/skinny version of the dataset appropriate 
    # for shiny (saved in Part 2) inside the renderPlot() function.
    skinny_tbl <- readRDS("skinny.rds")
  
    # Since "All" is not an option in the gender column, I
    # created an if(){} code that allows "All" to keep the full dataset
    # but then keeps only Male or Female data if they are selected.
    if(input$Gender!="All"){
      skinny_tbl <- skinny_tbl %>%
        filter(gender == input$Gender)
    }
    
    # I created an if(){} code that excludes participants who completed 
    # the assessment before (timeEnd) July 1, 2017 if "No" is selected 
    # in the radio buttons. 
    if(input$Time=="No"){
      skinny_tbl <- skinny_tbl %>%
        filter(timeEnd>"2017-06-30 23:59:59")
    }

    # I re-created the same plot from the .rmd file to show the data selected.
    skinny_tbl %>%
      ggplot(aes(x=average1, y=average2)) +
      geom_point() +
      geom_smooth(method="lm", color="purple", 
                  # Is used an if(){}ele{} statement to only display the 
                  # error bands if "Display Error Band" is selected in 
                  # the radio buttons.
                  se=if(input$Errorband=="Display Error Band"){TRUE}else{FALSE}) +
      labs(title = "Scatterplot of Mean Scores",
           x = "Mean Scores of Q1-Q6",
           y = "Mean Scores of Q8-Q10")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)