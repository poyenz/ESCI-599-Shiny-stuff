# Title: ER Case Study Assignment 2
# Name: Zachary Poyen
# Date: May 7th, 2025
# Class: ESCI 599
# Description: Updating the previous ER app. Added the changes outlined in
# exercise 3 and 4 from the chapter, as well as messing with color choices,
# titles, and interactive graphs. Had a lot of issues trying to get this to
# run, so it still is disorganized. I considered using thematic but wanted
# to find a different way to alter color as this used in some of my previous
# peer reviews. 

# getting the necessary libraries
library(shiny)
library(vroom)
library(tidyverse)
# added some new libraries for functionality and appearance, not sure if both are totally necessary
library(shinyWidgets)
library(plotly)

# injuries dataset
if (!exists("injuries")) {
  injuries <- vroom::vroom("neiss/injuries.tsv.gz")
  injuries
# products dataset
  products <- vroom::vroom("neiss/products.tsv")
  products
# population dataset
  population <- vroom::vroom("neiss/population.tsv")
  population
}

# building framework for app
prod_codes <- setNames(products$prod_code, products$title)

ui <- fluidPage(
# added title panel
  titlePanel("ER Case Study Data"),
# added background color
  setBackgroundColor(color = "LightCoral"),
# modified so you can input the number of rows you want displayed
  fluidRow(
    column(4,
           numericInput("num_rows", "Number of rows displayed", value=5, min=1)
    )
  ),
  fluidRow(
    column(6,
           selectInput("code", "Product", choices = prod_codes)
    )
  ),
  fluidRow(
    column(4, tableOutput("diag")),
    column(4, tableOutput("body_part")),
    column(4, tableOutput("location"))
  ),
# Modified this to have forwards and backwards button for narratives
  fluidRow(
    column(2, actionButton("next_story", "Tell me a new story!")),
    column(2, actionButton("prev_story", "Go back!")),
    column(10, textOutput("narrative"))
  ),
# changed from plotOutput to plotlyOutput so I could us interactive graphs
  fluidRow(
    column(12, plotlyOutput("age_sex"))
  ),
  fluidRow(
    column(12, plotlyOutput("age_sex_rate"))
  )
)
# added head(input$num_rows) to outputs so you can input how many rows you want displayed
server <- function(input, output, session) {
  selected <- reactive(injuries %>% filter(prod_code == input$code))
  
  output$diag <- renderTable(
    selected() %>% count(diag, wt = weight, sort = TRUE) %>%
    head(input$num_rows)
  )
  output$body_part <- renderTable(
    selected() %>% count(body_part, wt = weight, sort = TRUE)%>%
    head(input$num_rows)
  )
  output$location <- renderTable(
    selected() %>% count(location, wt = weight, sort = TRUE) %>%
    head(input$num_rows)
  )
  
  summary <- reactive({
    selected() %>%
      count(age, sex, wt = weight) %>%
      left_join(population, by = c("age", "sex")) %>%
      mutate(rate = n / population * 1e4)
  })
# modified to use plotly
  output$age_sex <- renderPlotly({
    plot_1 <- summary() %>%
      ggplot(aes(age, n, colour = sex)) +
      geom_line() +
      labs(y = "Estimated number of injuries")
    ggplotly(plot_1)
  })
# adding 2nd plot with rate instead of raw count
# modified to use plotly
  output$age_sex_rate <- renderPlot({
    plot_2 <- summary() %>%
      ggplot(aes(age, rate, colour = sex)) +
      geom_line(na.rm = TRUE) +
      labs(y = "Injuries per 10,000 people")
    ggplotly(plot_2)
  })
# narrative stuff, heavily modified from last time to allow for forwards/backwards buttons
# creating index values for narrative
  narrative_val <- reactiveValues(index = 1, narratives = NULL)
# modified for to allow the narrative list to be separated by the product selected by the user
  observeEvent(input$code, {
    new_narratives <- injuries %>% filter(prod_code == input$code) %>% pull(narrative)
    narrative_val$narratives <- new_narratives
  })
# this is the code for the forward button, adds to the index we defined earlier
  observeEvent(input$next_story, {
    if (!is.null(narrative_val$narratives)) {
      narrative_val$index <- min(narrative_val$index + 1,
      length(narrative_val$narratives))
    }
  })
# this is the code for the backwards button, it subtracts from the index
  observeEvent(input$prev_story, {
    if (!is.null(narrative_val$narratives)) {
      narrative_val$index <- max(narrative_val$index - 1, 1)
    }
  })
# this is responsible for displaying the narrative, or displaying a message that you'v run out
  output$narrative <- renderText({
    if (is.null(narrative_val$narratives) || length(narrative_val$narratives) == 0) {
      return("That's all of them! Back to the start")
      narrative_val$index <- 1
    }
    narrative_val$narratives[[narrative_val$index]]
  })
}

shinyApp(ui, server)
