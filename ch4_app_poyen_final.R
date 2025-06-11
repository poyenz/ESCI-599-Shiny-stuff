# Title: Methane App (final version)
# Name: Zachary Poyen
# Date: June 11th
# Class: ESCI 599
# Description: This is pretty close to where I want it. I considered adding
# additional stats functionalities but I ended up not because it made the plots
# a bit too messy in my opinion. Regardless, I'm not displeased with how it is
# now. Be sure to set to the right working directory!

# getting the necessary libraries
library(shiny)
library(shinyWidgets)
library(vroom)
library(ggplot2)
library(plotly)
library(tidyverse)

# getting the data
ch4_data <- vroom::vroom("methane_hist_emissions.csv")
# filtering out the NaN and NA values and replacing with 0
ch4_data <- ch4_data %>%
  mutate(across(everything(), ~ ifelse(is.na(.) | is.nan(.), 0, .)))
# commented out here, wanted to inspect after filtering to see if it worked. 
# ch4_data

# transposing/converting columns to rows for plotting, also correcting error
# in formatting and cleaning data
ch4_data$`1990` <- as.numeric(ch4_data$`1990`)
ch4Long <- ch4_data %>% select(-Gas, -Unit) %>%
  pivot_longer(cols = -c(1:2),names_to = "Year", 
               values_to = "mtCO2e") %>%
  mutate(Year = as.numeric(Year))
# commented out here, wanted to inspect again to see if transposing worked
# ch4Long
  
# creating UI
ui <- fluidPage(
# adding title and background color
  titlePanel("Methane Emission Visualization"),
  setBackgroundColor(color = "MediumAquaMarine"),
# sector/source selection
  fluidRow(
    column(4,
           selectInput("sector", "Select Source:",
                       choices = unique(ch4Long$Sector),
                       selected = unique(ch4Long$Sector)[1],
                       multiple = TRUE)
    )
  ),
# country selection
  fluidRow(
    column(6,
           selectInput("country", "Select Country:",
                        choices = unique(ch4Long$Country),
                        selected = unique(ch4Long$Country)[1],
                        multiple = FALSE)
     )
  ),
# slider input for years
  fluidRow(
    column(8,
           sliderInput("Year", "Select time range:",
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

# server stuff allowing for the selection of sector, year, and country
server <- function(input, output, session) {
  ch4_filtered <- reactive({
    ch4Long %>%
      filter(Sector %in% input$sector,
             Year >= input$Year[1],
             Year <= input$Year[2],
             Country %in% input$country)
  })

# plotting
  output$ch4_plot <- renderPlotly({
     ch4_plot <- ggplot(data = ch4_filtered(), aes(x = Year, y = mtCO2e, color = Sector)) +
      geom_point() +
      theme_bw() +
      labs(x = "Year", y = "Methane Emissions", title = "Methane Emissions by Sector")
    ggplotly(ch4_plot)
  })
}

# running the app
shinyApp(ui, server)
