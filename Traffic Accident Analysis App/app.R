library(shiny)

ui <- fluidPage(
  titlePanel("🚗 Traffic Accident Analysis"),
  
  sidebarPanel(
    sliderInput("year", "Select Year:",
                min = 2021,
                max = 2030,
                value = 2026,
                step = 1),
    actionButton("predict", "Predict")
  ),
  
  mainPanel(
    h3("Prediction"),
    textOutput("result"),
    plotOutput("plot")
  )
)

server <- function(input, output) {
  
  data <- data.frame(
    year = 2021:2025,
    accidents = c(120,145,160,180,200)
  )
  
  model <- lm(accidents ~ year, data = data)
  
  prediction <- eventReactive(input$predict, {
    predict(model,
            data.frame(year = input$year))
  })
  
  output$result <- renderText({
    paste("Estimated accidents in",
          input$year, ":",
          round(prediction()))
  })
  
  output$plot <- renderPlot({
    plot(data$year, data$accidents,
         type = "o",
         pch = 19,
         xlab = "Year",
         ylab = "Accidents",
         main = "Traffic Accident Analysis")
  })
}

shinyApp(ui, server)