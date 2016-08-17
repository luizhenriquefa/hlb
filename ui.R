library(shiny)

shinyUI(fluidPage(
  
  titlePanel("HLB"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("fileP", "Carregar condicao inicial das plantas"),
      fileInput("fileI", "Carregar condicao inicial dos insetos"),
      sliderInput("prob", "Probabilidade de infeccao", 0.25, min=0, max=1, step=0.05),
      actionButton("Start", "Start"),
      actionButton("Stop", "Stop"),
      h4("Iteracao"),
      verbatimTextOutput("contador")
    ),
    mainPanel(
      fluidRow(
        splitLayout(cellWidths = c("50%","50%"),plotOutput("plantaPlot"),plotOutput("insetoPlot"))
      ),
      fluidRow(
        splitLayout(cellWidths = c("50%","50%"),plotOutput("contadorPlanta"),plotOutput("contadorInseto"))
      )
    )
  )
))