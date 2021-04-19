# Title: POSC-3410 DAL 8
# Author: Sergio Gonzalez Varela
# Author's Email: sagonza@g.clemson.edu
# Date Created: 2021-04-19

# Purpose:

# Set Up####
# Libraries
library(shiny)
library(tidyverse)

library(readr)
gss_df <- read_csv("~/gss_df.csv")
View(gss_df)

gss_shiny <- gss_df %>%
  group_by(NATEDUC, YEAR, INTSPACE) %>%
  count()

# UI####
ui <- fluidPage(
  # Page title 
  titlePanel("POSC-3410 ShinyApp Deliverable"),
  
  #Configure layout
  sidebarLayout(
    # Control panel 
    sidebarPanel(
      # Slider input for Year
      sliderInput( 
        "gss_years",
        "Select years for analysis:",
        min = min(gss_shiny$YEAR, na.rm=TRUE),
        max = max(gss_shiny$YEAR, na.rm=TRUE),
        value=c(min(gss_shiny$YEAR, na.rm=TRUE), max(gss_shiny$YEAR, na.rm=TRUE)),
        sep=""
        )
    ),
    mainPanel(
      h1("Support of National Education and Space Exploration over Time"),
      plotOutput("gss_plot"),
      p("This ShinyApp deliverable observes support of National Funding of Education (NATEDUC) and National Interest in Space Exploration (INTSPACE). The research hypothesis is that there is a relationship between the two variables (NATEDUC and INTSPACE). The study was created using information from the GSS Data Explorer, and it uses information from 1972 to 2018. The data shows that there is indeed a relationship between NATEDUC and INTSPACE. Using the Chi-Square test, it is also clear that the funding for the nation's education system is related to interest in space exploration, as there is a greater amount of people that believe the education system to be underfunded are interested in space exploration. This proves that federal spending on education and interest might be related, or even interchangeable with one another. Whatever is causing the support of national funding of education and interest in space should then be explored in future studies. Sources: https://gssdataexplorer.norc.org/.")
    )
  )

)

# Server####
server<- function(input, output, session) {
  # GSS Plot 
  output$gss_plot <- renderPlot({
    data <- gss_shiny %>% 
      filter(YEAR >= input$gss_years [1] & YEAR <= input$gss_years [2])
    data %>% 
      ggplot(aes(x=NATEDUC, y=n, fill=INTSPACE)) +
      geom_bar(stat="identity")
  })
}

# App####
shinyApp(ui=ui, server=server)

# Copyright (c) Grant Allard, 2021
  