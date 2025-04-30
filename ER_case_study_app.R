# Title: ER Case Study Assignment 1
# Name: Zachary Poyen
# Date: Apr. 30, 2025
# Class: ESCI 599
# Description: Creating the ER app from Chapter 4. I had a lot of issues getting
# some of the code to run, particularly the later upgrades in the code. As a
# result, I modified it a bit so it would function better. I now have it 
# displaying both raw injury number and rate instead of choosing which to
# display, and also included the narrative selection option. I did not
# include the polish tables edit, as that was having trouble running (I
# commented out this code and left it at the end, if people want to look). 

# getting the necessary libraries
library(shiny)
library(vroom)
library(tidyverse)

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
    column(2, actionButton("story", "Tell me a story")),
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
    selected() %>% count(diag, wt = weight, sort = TRUE)
  )
  output$body_part <- renderTable(
    selected() %>% count(body_part, wt = weight, sort = TRUE)
  )
  output$location <- renderTable(
    selected() %>% count(location, wt = weight, sort = TRUE)
  )
  
  summary <- reactive({
    selected() %>%
      count(age, sex, wt = weight) %>%
      left_join(population, by = c("age", "sex")) %>%
      mutate(rate = n / population * 1e4)
  })
  
  output$age_sex <- renderPlot({
    summary() %>%
      ggplot(aes(age, n, colour = sex)) +
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
  narrative_sample <- eventReactive(
    list(input$story, selected()),
    selected() %>% pull(narrative) %>% sample(1)
  )
  output$narrative <- renderText(narrative_sample())
}

shinyApp(ui, server)

# In particular, this section of code was hard to work with. I kept getting
# random errors noting "output not found" and similar messages, and was unable
# to rectify it. Maybe something to do with the spacing or syntax, or the
# count_top function.

# polishing tables

#injuries %>%
#  mutate(diag = fct_lump(fct_infreq(diag), n = 5)) %>%
#  group_by(diag) %>%
#  summarise(n = as.integer(sum(weight)))
#
#count_top <- function(df, var, n = 5) {
#  var_fix <- enquote(var)
#  df %>%
#    mutate(!!var_fix := fct_lump(fct_infreq(!!var_fix), n = n)) %>%
#    group_by(!!var_fix) %>%
#    summarise(n = as.integer(sum(weight)))
#}

#output$diag <- renderTable(count_top(selected(), diag), width = "100%")
#output$body_part <- renderTable(count_top(selected(), body_part), width = "100%")
#output$location <- renderTable(count_top(selected(), location), width = "100%")
