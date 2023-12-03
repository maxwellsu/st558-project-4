library(shiny)
library(tidyverse)
library(httr)
library(jsonlite)
library(randomForest)
library(caret)

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
    mutate(bowlEligible = total.wins >= 6) %>% 
    select(!c(year, teamId, season, conference.y, games, 
              thirdDownConversions, thirdDowns, fourthDownConversions, 
              fourthDowns, passesIntercepted, fumblesRecovered, turnovers, fumblesLost, interceptions))
  
  fbs2022[,21:42] <- fbs2022[,21:42] / fbs2022$total.games
  fbs2022$conference <- as.factor(fbs2022$conference)
  levels(fbs2022$conference) <- make.names(levels(fbs2022$conference))
  fbs2022$bowlEligible <- as.factor(fbs2022$bowlEligible)
  levels(fbs2022$bowlEligible) <- c("No", "Yes")
  
  getPlotType <- reactive({
    plotType <- input$plotType
  })
  
  getTrainProp <- reactive({
    trainProp <- input$trainProp
  })

  output$glmSummary <- renderPrint({
    trainProp <- getTrainProp()
    set.seed(1647)
    
    train <- createDataPartition(1:nrow(fbs2022), p = input$trainProp, list = FALSE)
    train_data <- fbs2022[train, c(2, 22:46)]
    test_data <- fbs2022[-train, c(2, 22:46)]
    
    glmfit <- train(bowlEligible ~ conference + rushingYards + 
                      netPassingYards + sacks + firstDowns +
                      interceptionYards + puntReturnYards +
                      thirdDownConversionRate + fourthDownConversionRate + 
                      turnoverDiff + possessionTime,
                    data = train_data,
                    method = "glm",
                    family = "binomial",
                    trControl = trainControl(method = "cv",
                                             number = 5,
                                             classProbs = TRUE,
                                             summaryFunction=mnLogLoss),
                    metric = "logLoss")
    summary(glmfit)
  })
  
  output$rfPlot <- renderPlot({
    trainProp <- getTrainProp()
    set.seed(1647)
    
    train <- createDataPartition(1:nrow(fbs2022), p = input$trainProp, list = FALSE)
    train_data <- fbs2022[train, c(2, 22:46)]
    test_data <- fbs2022[-train, c(2, 22:46)]
    
    rffit <- train(bowlEligible ~ conference + rushingYards + 
                     netPassingYards + sacks + firstDowns +
                     interceptionYards + puntReturnYards +
                     thirdDownConversionRate + fourthDownConversionRate + 
                     turnoverDiff + possessionTime,
                   data = train_data,
                   method = "rf",
                   trControl = trainControl(method = "cv",
                                            number = 5,
                                            classProbs = TRUE,
                                            summaryFunction=mnLogLoss),
                   tuneGrid = data.frame(mtry = 1:5),
                   metric = "logLoss")
    varImpPlot(rffit$finalModel)
  })
  
  output$glmLogLoss <- renderPrint({
    trainProp <- getTrainProp()
    glmfit$results$logLoss
  })
  
  output$rfLogLoss <- renderPrint({
    trainProp <- getTrainProp()
    min(rffit$results$logLoss)
  })
  
  output$glmErrorMatrix <- renderPrint({
    trainProp <- getTrainProp()
    predGlm <- predict(glmfit, newdata = train_data)
    confusionMatrix(predGlm, train_data$bowlEligible)
  })
  
  output$rfErrorMatrix <- renderPrint({
    trainProp <- getTrainProp()
    predRf <- predict(rffit, newdata = train_data)
    confusionMatrix(predRf, train_data$bowlEligible)
  })
  
  observeEvent(input$predict, {
    predData <- data.frame(conference = input$predConference,
                           kickReturnTDs = 0,
                           passAttempts = 0,
                           tacklesForLoss = 0,
                           rushingAttempts = 0,
                           firstDowns = input$predFirst,
                           kickReturnYards = 0,
                           passCompletions = 0,
                           puntReturnYards = input$predPuntYds,
                           interceptionTDs = 0,
                           kickReturns = 0,
                           penalties = 0,
                           possessionTime = input$predTOP,
                           rushingTDs = 0,
                           interceptionYards = input$predIntYds,
                           rushingYards = input$predRushYds,
                           sacks = input$predSacks,
                           netPassingYards = input$predPassYds,
                           puntReturnTDs = 0,
                           puntReturns = 0,
                           penaltyYards = 0,
                           passingTDs = 0,
                           thirdDownConversionRate = input$predThird,
                           fourthDownConversionRate = input$predFourth,
                           turnoverDiff = input$predTurnover)
    output$glmPred <- renderPrint({
      predict(glmfit, newdata = predData)
    })
    
    output$rfPred <- renderPrint({
      predict(rffit, newdata = predData)
    })
  })
  
  output$EDAPlot <- renderPlot({
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
