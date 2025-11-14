#Upload packages
if (!require(shiny)) install.packages("shiny")
if (!require(ggplot2)) install.packages("ggplot2")
if (!require(dplyr)) install.packages("dplyr")
if (!require(tidyverse)) install.packages("tidyverse")

library(shiny)
library(ggplot2)
library(dplyr)
library (tidyverse)

# Read data from CSV
data <- read.csv("EMDAT (3).csv")

#Data wrangling
data <- data %>% 
  select(Entity, Year, deaths_all_disasters, injured_all_disasters, homeless_all_disasters) %>%
  rename(deaths = deaths_all_disasters, injuries = injured_all_disasters, 
         homelessness = homeless_all_disasters, country = Entity)
data <- data %>% 
  mutate(high_death = ifelse(deaths > 500, 1, 0))
view(data)

#UI DESIGN
ui <-
  #Create title
  fluidPage(
  titlePanel("World Disaster Statistics"),
  
  #Creating side bar layout
  sidebarLayout(
    sidebarPanel(
      #Creating country selector
      selectInput(
        inputId = "country",
        label = "select country",
        choices = unique(data$country),
        selected = "Afghanistan"
      ),
      #Creating variable selector
      selectInput(
        inputId = "variable",
        label = "select variable",
        choices = c("deaths","injuries","homelessness"),
        selected = "deaths"
      ),
      #Creating year slider selector
      sliderInput(
        inputId = "year_range",
        label = "select year",
        min = min(data$Year),
        max = max(data$Year),
        value = c(min(data$Year),max(data$Year)),
        step = 1,
        sep = ""
      )
    ),
    #Main panel output
    mainPanel(
      plotOutput("plot")
    )
  )
)

#Creating output 
server <- function(input,output,session){
  
  output$plot <- renderPlot({
    variable <- switch(input$variable,
                       "deaths" = "deaths",
                       "injuries" = "injuries",
                       "homelessness" = "homelessness"
                       )
    
    data %>%
      filter(country == input$country,
             Year >= input$year_range[1],
             Year <= input$year_range[2])%>%
      ggplot(aes(Year,.data[[variable]]))+
      geom_line()+
      labs(y = input$variable)
  })
}

shinyApp(ui,server)
