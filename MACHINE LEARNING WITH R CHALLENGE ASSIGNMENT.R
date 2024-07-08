library(shiny)
library(shinydashboard)
library(ggplot2)

housing <-read.csv('housing.csv')

ui <- dashboardPage(
  dashboardHeader(title = "Housing Data Dashboard"),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    fluidRow(
      box(plotOutput("plot1"), title = "Housing Categories"),
      box(plotOutput("plot2"), title = "Population vs. Households"),
      box(plotOutput("plot3"), title = "Total Rooms vs. Total Bedrooms")
    )
  )
)
  # Define server logic required to generate and display the plot
ui <- dashboardPage(
  dashboardHeader(title = "Housing Data Dashboard"),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    fluidRow(
      box(plotOutput("plot1"), title = "Housing Categories"),
      box(plotOutput("plot2"), title = "Population vs. Households"),
      box(plotOutput("plot3"), title = "Total Rooms vs. Total Bedrooms")
    )
  )
)

server <- function(input, output) {
  # Plot 1: Housing Categories
  output$plot1 <- renderPlot({
    housing_summary <- table(housing$ocean_proximity)
    ggplot(data = as.data.frame(housing_summary), aes(x = Var1, y = Freq)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      xlab("Categories") + ylab("Number of Homes") +
      ggtitle("Home Counts by Proximity to Ocean")
  })
  
  # Plot 2: Population vs. Households
  output$plot2 <- renderPlot({
    ggplot(data = housing, aes(x = households, y = population)) +
      geom_point(alpha = 0.5) +
      geom_smooth(method = lm, col = "blue") +
      xlab("Households") + ylab("Population") +
      ggtitle("Population vs. Households")
  })
  
  # Plot 3: Total Rooms vs. Total Bedrooms
  output$plot3 <- renderPlot({
    ggplot(data = housing, aes(x = total_rooms, y = total_bedrooms)) +
      geom_point(alpha = 0.5, color = "red") +
      geom_smooth(method = lm, col = "black") +
      xlab("Total Rooms") + ylab("Total Bedrooms") +
      ggtitle("Total Rooms vs. Total Bedrooms")
  })
}


  
  # Run the application 
  shinyApp(ui = ui, server = server)
  