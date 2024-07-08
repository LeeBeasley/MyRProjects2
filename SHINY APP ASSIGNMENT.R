library(shiny)
library(shinydashboard)
library(ggplot2)

ui <- dashboardPage(
  dashboardHeader(title = "Car Performance Analysis"),
  dashboardSidebar(),
  dashboardBody(
    fluidRow(
      box(plotOutput("weightDist"), title = "Weight Distribution"),
      box(plotOutput("mpgPerformance"), title = "MPG Performance"),
      box(plotOutput("dispVsHp"), title = "Displacement vs Horsepower")
    )
  )
)

server <- function(input, output) {
  output$weightDist <- renderPlot({
    ggplot(mtcars, aes(x = wt)) + geom_histogram(bins = 10)
  })
  output$mpgPerformance <- renderPlot({
    ggplot(mtcars, aes(x = factor(cyl), y = mpg)) + geom_boxplot()
  })
  output$dispVsHp <- renderPlot({
    ggplot(mtcars, aes(x = disp, y = hp, color = factor(gear))) + geom_point()
  })
}

shinyApp(ui, server)
