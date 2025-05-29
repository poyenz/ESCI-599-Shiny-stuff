# Title: Methane App (refinements v1)
# Name: Zachary Poyen
# Date: May 14th, 2025
# Class: ESCI 599
# Description: Still definitley a work in progress. I actually got the app to
# run in shiny but the data manipulation and plotting is proving to be more 
# difficult than I anticipated. 

# getting the necessary libraries
library(shiny)
library(vroom)
library(ggplot2)
library(plotly)
library(dplyr)
library(tidyr)

# getting the data
ch4_data <- vroom::vroom("methane_hist_emissions.csv")
# filtering out the NaN and NA values and replacing with 0
ch4_data <- ch4_data %>%
  mutate(across(everything(), ~ ifelse(is.na(.) | is.nan(.), 0, .)))
# commented out here, wanted to inspect after filtering to see if it worked. 
# ch4_data

# messing around with converting columns to rows as that seems to be the main
# issue with current version and is getting in the way of plotting efforts. This
# did NOT work, and actually broke the sector/country buttons. 
# ch4_dataframe <- as.data.frame(t(ch4_data))
# ch4_dataframe

# TBD second attempt at data manipulation, seems like dplyr and tidyr are my
# best bets
  

# creating UI
ui <- fluidPage(
  titlePanel("Methane Emission Visualization"),
  fluidRow(
    column(4,
           selectInput("sector", "Select Source:",
                       choices = unique(ch4_data$Sector),
                       selected = unique(ch4_data$Sector)[1],
                       multiple = TRUE)
    )
  ),
# messing around with country selection as an option here. Keeping it set
# to one country at a time for now
  fluidRow(
    column(6,
           selectInput("country", "Select Country:",
                        choices = unique(ch4_data$Country),
                        selected = unique(ch4_data$Country)[1],
                        multiple = FALSE)
     )
  ),
# slider input for years
  fluidRow(
    column(8,
           sliderInput("years", "Select time range:",
                       min = 1990,
                       max = 2018,
                       value = c(2000, 2015),
                       step = 1)
   )
  ),
  fluidRow(
    column(10, plotlyOutput("ch4_plot"))
  )
)

# server stuff
server <- function(input, output, session) {
  ch4_filtered <- reactive({
    ch4_data %>%
      filter(Sector %in% input$sector,
             year >= input$years[1],
             year <= input$years[2],)
# This is just for the country selection stuff I was playing with
#            Country %in% input$country)
  })
  
# plotting
  output$ch4_plot <- renderPlotly({
    ch4_plot <- ggplot(ch4_filtered(), aes(x = year, y = Emissions, color = Sector)) +
      geom_line() +
      labs(x = year, y = "Methane Emissions", title = "Methane Emissions by Sector")
    ggplotly(ch4_plot)
  })
}

# running the app
shinyApp(ui, server)
