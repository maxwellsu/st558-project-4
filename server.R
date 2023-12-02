library(shiny)
library(tidyverse)
library(httr)
library(jsonlite)

function(input, output, session) {
  
  fbs2022records <- read.csv("data/fbs2022records.csv")
  fbs2022stats <- read.csv("data/fbs2022stats.csv")
  fbs2022stats <- fbs2022stats %>% pivot_wider(names_from = statName, values_from = statValue)
  
  fbs2022 <- fbs2022records %>% 
    left_join(fbs2022stats, by = "team") %>%
    rename(conference = conference.x) %>% 
    mutate(division = trimws(paste(conference, division))) %>%
    mutate(thirdDownConversionRate = thirdDownConversions / thirdDowns) %>% 
    mutate(fourthDownConversionRate = fourthDownConversions / fourthDowns) %>%
    mutate(turnoverDiff = (passesIntercepted + fumblesRecovered) - turnovers) %>%
    mutate(bowl.eligible = total.wins >= 6) %>% 
    select(!c(year, teamId, season, conference.y, games, 
              thirdDownConversions, thirdDowns, fourthDownConversions, 
              fourthDowns, passesIntercepted, fumblesRecovered, turnovers, fumblesLost, interceptions))
  
  fbs2022[,21:42] <- fbs2022[,21:42] / fbs2022$total.games

  getPlotType <- reactive({
    plotType <- input$plotType
  })
  getTableType <- reactive({
    tableType <- input$tableType
  })
  getX <- reactive({
    x <- input$x
  })
  getY <- reactive({
    y <- input$y
  })
  getZ <- reactive({
    z <- input$z
  })
  getW <- reactive({
    w <- input$w
  })

  output$Plot <- renderPlot({
    plotType <- getPlotType()
    g <- ggplot(data = fbs2022)
    if(plotType == 2) {
      g + geom_histogram(aes_string(x = input$x), bins = input$bins)
    } else if(plotType == 3 && input$z == "None") {
      g + geom_point(aes_string(x = input$x, y = input$y), position = 'jitter')
    } else if(plotType == 3) {
      g <- ggplot(data = fbs2022 %>% filter(conference == input$z))
      g + geom_point(aes_string(x = input$x, y = input$y, color = "division"), position = 'jitter')
    } else if(plotType == 4) {
      g + geom_boxplot(aes_string(x = input$x, y = input$w))
    } else {
      g + geom_bar(aes(x = total.wins, fill = conference))
    }

  })

}
