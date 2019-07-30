library(shiny)
library(dplyr)
library(broom)
library(readr)
# https://www.kaggle.com/uciml/pima-indians-diabetes-database/downloads/pima-indians-diabetes-database.zip/1
diabetes <- read_csv("diabetes.csv")

ui <- shinyUI(
  pageWithSidebar(
    headerPanel('diabetes k-means clustering'),
    sidebarPanel(
      selectInput('xcol', 'X Variable', names(diabetes),selected=names(diabetes)[[2]]),
      selectInput('ycol', 'Y Variable', names(diabetes),
                  selected=names(diabetes)[[6]]),
      numericInput('clusters', 'Cluster count', 2,
                   min = 2, max = 5)
    ),
    mainPanel(
      plotOutput("plot1")
    )
  )
)

server <- shinyServer(function(input, output, session) {
  selectedData <- reactive({
    diabetes[, c(input$xcol, input$ycol)]
  })
  
  clusters <- reactive({
    kmeans(selectedData(), input$clusters)
  })
  
  output$plot1 <- renderPlot({
    wss <- rep(0,15)
    for (i in 1:15) wss[i] <- sum(kmeans(selectedData(),centers=i)$withinss)
    palette(c("#F781BF","#FFFF33", "#A65628","#FF7F00"))
    par(mar = c(5.1, 4.1, 0, 1),mfcol=c(2,1))
    plot(selectedData(),
         col = clusters()$cluster,
         pch = 20, cex = 3)
    points(clusters()$centers, pch = 4, cex = 4, lwd = 4)
    plot(1:15, wss[1:15], 
         type="b", 
         xlab="Number of Clusters",
         ylab="Within groups sum of squares")
    
  })
})


# Run the application 
shinyApp(ui = ui, server = server)

