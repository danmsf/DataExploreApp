library(shiny)
library(ggplot2)
# Define UI for application that draws a histogram
shinyUI(fluidPage(

  # Application title
  titlePanel("categorize"),

  # Sidebar with a slider input for the number of bins
  sidebarLayout(
    sidebarPanel(
      sliderInput("bins",
                  "Number of bins:",
                  min = 1,
                  max = 50,
                  value = 30),

      sliderInput("receive", "Axis Range:", min=0, max=20, value=c(0,10)),

      selectInput("data", label = h3("Select data"),
                  choices = ls(.GlobalEnv),selected="sample_data" ),

      radioButtons("type", "Variable Type",
                   choices = c("Continuous" = FALSE,
                               "Discrete" = TRUE),
                   selected = FALSE),

      uiOutput("columns"),

      uiOutput("bycolumns"),

     actionButton("action", label = "Submit")
    ),

    # Show a plot of the generated distribution
    mainPanel(
      # verbatimTextOutput("summary"),
      tableOutput("summary"),
      plotOutput("distPlot", height = 300,
                 dblclick = "plot2_dblclick",
                 brush = brushOpts(
                   id = "plot2_brush",
                   resetOnNew = TRUE
                 )
                 ),

      # plotOutput("boxPlot"),
      plotOutput("spline", height = 300)
    )
  )
))