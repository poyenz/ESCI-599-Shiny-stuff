# Title: Methane App
# Name: Zachary Poyen
# Date: May 14th, 2025
# Class: ESCI 599
# Description: This is the start of my methane app. Not quite functional yet
# but it has the bare bones and shows where I'm starting. 

# getting the necessary libraries
library(shiny)
library(vroom)
library(ggplot2)
library(plotly)

# setting working directory because I was getting weird errors
setwd("~/Users/poyenzl/Downloads")
getwd

# getting file
if (!exists("ch4_emissions")) {
  ch4_data <- vroom::vroom("methane_hist_emissions.csv")
  ch4_data
}

# creating UI
ui <- fluidPage(
  titlePanel("Methane Visualization"),
  fluidRow(
    column(4,
           selectInput("sector", "Select source:",
                       choices = unique(ch4_data$Sector),
                       multiple = TRUE)
    )
  ),
  fluidRow(
    column(6,
           sliderInput("years", "Select time range:",
                       min = min(ch4_data$year),
                       max = max(ch4_data$year),
                       value = c(2000,2015))
    )
  ),
  fluidRow(
    column(8, plotlyOutput("ch4_plot"))
  )
)

# I'm still working on these parts of the app, hold tight
# will eventually have table and plot stuff
# server <- function(input, output, session) {
#  ch4_filtered <- reactive(ch4_data %>% filter(Sector %in% input$Sector,
#                                               Year >= input$years[1],
#                                               Year >= input$years[2])
#})


                                                 
# output$ch4_plot <- renderPlotly({
#    plot_1 <- ggplot(ch4_filtered, aes(year, , colour = sex)) +
#      geom_line() +
#     labs(y = "Estimated number of injuries")
#    ggplotly(plot_1)
#  })
shinyApp(ui, server)