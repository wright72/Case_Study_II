
library(shiny)
library(vroom)
library(tidyverse)
library(bslib)

prodPath <- "https://raw.githubusercontent.com/hadley/mastering-shiny/main/neiss/products.tsv"

products <- vroom::vroom(prodPath)

popPath <- "https://raw.githubusercontent.com/hadley/mastering-shiny/main/neiss/population.tsv"

population <- vroom::vroom(popPath)

injPath <- "https://raw.githubusercontent.com/hadley/mastering-shiny/main/neiss/injuries.tsv.gz"

injuries <- vroom::vroom("injuries.tsv.gz")

selected <- injuries %>% filter(prod_code == 649)
nrow(selected)

selected %>% count(location, wt = weight, sort = TRUE)

selected %>% count(body_part, wt = weight, sort = TRUE)

selected %>% count(diag, wt = weight, sort = TRUE)

summary <- selected %>% 
  count(age, sex, wt = weight)
summary

summary %>% 
  ggplot(aes(age, n, colour = sex)) + 
  geom_line() + 
  labs(y = "Estimated number of injuries")

summary <- selected %>% 
  count(age, sex, wt = weight) %>% 
  left_join(population, by = c("age", "sex")) %>% 
  mutate(rate = n / population * 1e4)

summary

summary %>% 
  ggplot(aes(age, rate, colour = sex)) + 
  geom_line(na.rm = TRUE) + 
  labs(y = "Injuries per 10,000 people")

selected %>% 
  sample_n(10) %>% 
  pull(narrative)

injuries %>%
  mutate(diag = fct_lump(fct_infreq(diag), n = 5)) %>%
  group_by(diag) %>%
  summarise(n = as.integer(sum(weight)))

count_top <- function(df, var, n = 5) {
  df %>%
    mutate({{ var }} := fct_lump(fct_infreq({{ var }}), n = n)) %>%
    group_by({{ var }}) %>%
    summarise(n = as.integer(sum(weight)))
}

prod_codes <- setNames(products$prod_code, products$title)

ui <- fluidPage(theme = bs_theme(),
  fluidRow(
    column(8,
           selectInput("code", "Product",
                       choices = setNames(products$prod_code, products$title),
                       width = "100%"
           )
    ),
    column(2, selectInput("y", "Y axis", c("Rate", "Count"))),
    column(2, sliderInput("rows", "Number of Rows",
                          min = 1, max = 10, value = 5))
    
  ),

  fluidRow(
    column(4, tableOutput("diag")),
    column(4, tableOutput("body_part")),
    column(4, tableOutput("location"))
  ),
  fluidRow(
    column(12, plotOutput("age_sex"))
  ),
  fluidRow(
    column(2, actionButton("back", "Previous story")),
    column(2, actionButton("forward", "Next story")),
    column(10, textOutput("narrative"))
  )
 
)

server <- function(input, output, session) { 
  
  bs_themer()
  
  selected <- reactive(injuries %>% filter(prod_code == input$code))
  
  slider_table_rows <- reactive(input$rows)
  
  output$diag <- renderTable(count_top(selected(), diag, n = slider_table_rows()), width = "100%")
  output$body_part <- renderTable(count_top(selected(), body_part, n = slider_table_rows()), width = "100%")
  output$location <- renderTable(count_top(selected(), location, n = slider_table_rows()), width = "100%")
  
  # Calculate the maximum length of the series
  max_no_stories <- reactive(length(selected()$narrative))
  
  # Set the initial position of the subset index
  place <- reactiveVal(1)
  
  # In cases where user changes product code, reset the place value
  observeEvent(input$code, {
    place(1)
  })
  
  # Observe for user button clicks, change place value accordingly
  observeEvent(input$forward, {
    place((place() %% max_no_stories()) + 1)
  })
  
  observeEvent(input$back, {
    place(((place() - 2) %% max_no_stories()) + 1)
  })
  
  # Output the text narrative to the UI using subsetting
  output$narrative <- renderText({
    selected()$narrative[place()]
  })
  
  summary <- reactive({
    selected() %>%
      count(age, sex, wt = weight) %>%
      left_join(population, by = c("age", "sex")) %>%
      mutate(rate = n / population * 1e4)
  })
  
  output$age_sex <- renderPlot({
    if (input$y == "Count") {
      summary() %>%
        ggplot(aes(age, n, colour = sex)) +
        geom_line() +
        labs(y = "Estimated number of injuries")
    } else {
      summary() %>%
        ggplot(aes(age, rate, colour = sex)) +
        geom_line(na.rm = TRUE) +
        labs(y = "Injuries per 10,000 people")
    }
  }, res = 96)
    }

shinyApp(ui, server)
