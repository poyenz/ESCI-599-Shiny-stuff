# Title: ER Case Study Assignment 1
# Name: Zachary Poyen
# Date: May 7th, 2025
# Class: ESCI 599
# Description: Updating the previous ER app. Added the changes outlined in
# exercise 3 and 4 from the chapter, as well as messing with color choices,
# titles, and interactive graphs.

# getting the necessary libraries
library(shiny)
library(vroom)
library(tidyverse)
library(shinyWidgets)

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
  titlePanel("ER Case Study Data"),
  setBackgroundColor(color = "LightCoral"),
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
  fluidRow(
    column(2, actionButton("next_story", "Tell me a new story!")),
    column(2, actionButton("prev_story", "Go back!")),
    column(10, textOutput("narrative"))
  ),
  fluidRow(
    column(12, plotOutput("age_sex"))
  ),
  fluidRow(
    column(12, plotOutput("age_sex_rate"))
  )
)

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
  
  output$age_sex <- renderPlotly({
    summary() %>%
      ggplotly(aes(age, n, colour = sex)) +
      geom_line() +
      labs(y = "Estimated number of injuries")
  }, res = 96)
# adding 2nd plot with rate instead of raw count
  output$age_sex_rate <- renderPlot({
    summary() %>%
      ggplot(aes(age, rate, colour = sex)) +
      geom_line(na.rm = TRUE) +
      labs(y = "Injuries per 10,000 people")
  }, res = 96)
# narrative stuff
  narrative_val <- reactiveValues(index = 1, narratives = NULL)
  
  observeEvent(input$code, {
    new_narratives <- injuries %>% filter(prod_code == input$code) %>% pull(narrative)
    narrative_val$narratives <- new_narratives
  })
  
  observeEvent(input$next_story, {
    if (!is.null(narrative_val$narratives)) {
      narrative_val$index <- min(narrative_val$index + 1,
      length(narrative_val$narratives))
    }
  })
  
  observeEvent(input$prev_story, {
    if (!is.null(narrative_val$narratives)) {
      narrative_val$index <- max(narrative_val$index - 1, 1)
    }
  })
  
  output$narrative <- renderText({
    if (is.null(narrative_val$narratives) || length(narrative_val$narratives) == 0) {
      return("That's all of them! Back to the start")
      narrative_val$index <- 1
    }
    narrative_val$narratives[[narrative_val$index]]
  })
}

shinyApp(ui, server)