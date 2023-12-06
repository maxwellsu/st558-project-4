library(shiny)
library(ggplot2)
library(mathjaxr)

shinyUI(fluidPage(

    # Application title
    titlePanel("NCAA D-1A (FBS) Bowl Eligibility Modeling"),

    sidebarLayout(
        sidebarPanel(
            conditionalPanel(
                condition = "input.tabs1 != 'About'",
                radioButtons("plotType", "Select Plot Type",
                             choices = c("Barplot of Total Wins by Conference" = 1,
                                         "Histogram (1 - variable)" = 2,
                                         "Scatterplot (2 - variable)" = 3,
                                         "Boxplot (2 - variable)" = 4))
            ),
            conditionalPanel(
                condition = "input.plotType != 1 && input.tabs1 == 'Data Exploration'",
                br(),
                selectizeInput("x", "Select X Variable",
                               choices = c("Yards per Game" = "totalYards",
                                           "Pass Attempts per Game" = "passAttempts",
                                           "Pass Completions per Game" = "passCompletions",
                                           "Net Passing Yards per Game" = "netPassingYards",
                                           "Passing TDs per Game" = "passingTDs",
                                           "Rush Attempts per Game" = "rushingAttempts",
                                           "Rushing Yards per Game" = "rushingYards",
                                           "Rushing TDs per Game" = "rushingTDs",
                                           "First Downs per Game" = "firstDowns",
                                           "Third Down Conversion Rate" = "thirdDownConversionRate",
                                           "Fourth Down Completion Rate" = "fourthDownConversionRate",
                                           "Turnover Differential" = "turnoverDiff",
                                           "Average Time of Possession (Seconds)" = "possessionTime",
                                           "Defense Tackles for Loss per Game" = "tacklesForLoss",
                                           "Defense Sacks per Game" = "sacks",
                                           "Interception Yards per Game" = "interceptionYards",
                                           "Interception TDs per Game" = "interceptionTDs",
                                           "Punt Returns per Game" = "puntReturns",
                                           "Punt Return Yards per Game" = "puntReturnYards",
                                           "Punt Return TDs per Game" = "puntReturnTDs",
                                           "Kick Returns per Game" = "kickReturns",
                                           "Kick Return Yards per Game" = "kickReturnYards",
                                           "Kick Return TDs per Game" = "kickReturnTDs",
                                           "Penalties per Game" = "penalties",
                                           "Penalty Yards per Game" = "penaltyYards"))),
            conditionalPanel(
              condition = "input.plotType == 2 && input.tabs1 == 'Data Exploration'",
              
              sliderInput("bins", "Number of bins",
                          min = 5, max = 30, value = 15, step = 1)),
            conditionalPanel(
              condition = "input.plotType == 3 && input.tabs1 == 'Data Exploration'",
              selectizeInput("y", "Select Y Variable",
                             choices = c("Yards per Game" = "totalYards",
                                         "Pass Attempts per Game" = "passAttempts",
                                         "Pass Completions per Game" = "passCompletions",
                                         "Net Passing Yards per Game" = "netPassingYards",
                                         "Passing TDs per Game" = "passingTDs",
                                         "Rush Attempts per Game" = "rushingAttempts",
                                         "Rushing Yards per Game" = "rushingYards",
                                         "Rushing TDs per Game" = "rushingTDs",
                                         "First Downs per Game" = "firstDowns",
                                         "Third Down Conversion Rate" = "thirdDownConversionRate",
                                         "Fourth Down Completion Rate" = "fourthDownConversionRate",
                                         "Turnover Differential" = "turnoverDiff",
                                         "Average Time of Possession (Seconds)" = "possessionTime",
                                         "Defense Tackles for Loss per Game" = "tacklesForLoss",
                                         "Defense Sacks per Game" = "sacks",
                                         "Interception Yards per Game" = "interceptionYards",
                                         "Interception TDs per Game" = "interceptionTDs",
                                         "Punt Returns per Game" = "puntReturns",
                                         "Punt Return Yards per Game" = "puntReturnYards",
                                         "Punt Return TDs per Game" = "puntReturnTDs",
                                         "Kick Returns per Game" = "kickReturns",
                                         "Kick Return Yards per Game" = "kickReturnYards",
                                         "Kick Return TDs per Game" = "kickReturnTDs",
                                         "Penalties per Game" = "penalties",
                                         "Penalty Yards per Game" = "penaltyYards")),
              selectizeInput("z", "Filter by Conference",
                             choices = c("No Filter" = "None",
                                         "American Athletic" = "American.Athletic",
                                         "Atlantic Coast" = "ACC",
                                         "Big 12" = "Big.12",
                                         "Big Ten" = "Big.Ten",
                                         "Conference USA" = "Conference.USA",
                                         "FBS Independents" = "FBS.Independents",
                                         "Mid-American" = "Mid.American",
                                         "Mountain West" = "Mountain.West",
                                         "Pac-12" = "Pac.12",
                                         "Southeastern" = "SEC",
                                         "Sun Belt" = "Sun.Belt"))),
              conditionalPanel(
                condition = "input.plotType == 4 && input.tabs1 == 'Data Exploration'",
                selectizeInput("w", "Select Y Variable",
                               choices = c("Conference" = "conference",
                                           "Bowl Eligibility" = "bowlEligible"))),
            conditionalPanel(
                condition = "input.tabs1 == 'Modeling'",
                br(),
                sliderInput("trainProp", "Proportion of data to train",
                            min = 0.5, max = 0.9, value = 0.7, step = 0.05),
                br(),
                h4("Prediction Parameters"),
                selectizeInput("predConference", "Conference",
                               choices = c("American Athletic" = "American.Athletic",
                                           "Atlantic Coast" = "ACC",
                                           "Big 12" = "Big.12",
                                           "Big Ten" = "Big.Ten",
                                           "Conference USA" = "Conference.USA",
                                           "FBS Independents" = "FBS.Independents",
                                           "Mid-American" = "Mid.American",
                                           "Mountain West" = "Mountain.West",
                                           "Pac-12" = "Pac.12",
                                           "Southeastern" = "SEC",
                                           "Sun Belt" = "Sun.Belt")),
                numericInput("predPassYds", "Net Passing Yards per Game", value = 0),
                numericInput("predRushYds", "Rushing Yards per Game", value = 0),
                numericInput("predFirst", "First Downs per Game", value = 0),
                numericInput("predThird", "Third Down Conversion Rate", value = 0),
                numericInput("predFourth", "Fourth Down Conversion Rate", value = 0),
                numericInput("predTurnover", "Turnover Differential", value = 0),
                numericInput("predTOP", "Average Time of Possession (Seconds)", value = 0),
                numericInput("predSacks", "Defense Sacks per Game", value = 0),
                numericInput("predIntYds", "Interception Yards per Game", value = 0),
                numericInput("predPuntYds", "Punt Return Yards per Game", value = 0),
                actionButton("predict", "Predict"))
        ),

        mainPanel(
          tabsetPanel(id = "tabs1", type = "tabs",
                      tabPanel("About",
                               br(),
                               p("This app explores team statistics from the top level of college football
                                 (Division 1-A) in 2022 in order to project whether a team will finish the
                                 season bowl-eligible or not."),
                               p("In order for a colelge football team to achieve bowl eligibilty,
                                 they must finish with a record of .500 or above. In a 12-game regular season,
                                 this means that 6 games must be won. There are some extra rules, namely that any team
                                 that plays 13 games in its regular season (Hawaii and its opponents) only need to hit
                                 6 wins, and that at most one of these wins can be against a non-FBS (D1-A) opponent."),
                               p("Bowl games are useful for college football teams as an additional source of income
                                 (from ticket and merch sales, as well as TV rights deals) and a source of additional
                                 exposure, so reaching bowl-eligibility has meaningful benefits to a programs' financial
                                 and recruiting interests."),
                               p("In 2022 there were 41 bowl games for a total of 82 teams. 80 teams reached bowl eligibility,
                                 so the remaining spots were given to teams that filed waivers, and the 5-7 teams with the
                                 highest APR (Academic Progress Rank). New Mexico State (who finished 6-6 but had
                                 2 wins over non-FBS teams and filed a waiver) and Rice (5-7, with highest APR) were also
                                 given bowl spots. Due to the unreliability of making a bowl game without reaching bowl eligibility,
                                 it is generally best to aim for 6 wins rather than leaving a team's postseason fate in the hands
                                 of other teams."),
                               p("In order to predict a teams' bowl eligibility, team stats were pulled from the College Football
                                 Data (CFBD) API. This data includes information on a teams':"),
                               p("- Games played (including overall record, home/away record, conference record"),
                               p("- Conference and Division"),
                               p("- Passing statistics (yards, attempts, completions, touchdowns"),
                               p("- Rushing statistics (yards, attempts, touchdowns"),
                               p("- Other offensive statistics (total yards, first downs, 3rd/4th down attempts/conversions),
                                 time of possession)"),
                               p("- Punt return statistics (returns, yards, touchdowns)"),
                               p("- Kick return statistics (returns, yards, touchdowns)"),
                               p("- Defensive statistics (sacks, tackles for loss, interception yards and touchdowns)"),
                               p("- Turnover statistics (funmles lost/recovered, interceptions)"),
                               p("- Team discipline (penalties, penalty yards)"),
                               p("From this data the following can be derived:"),
                               p("- Per game counting stats"),
                               p("- 3rd/4th down conversion rates"),
                               p("- Turnover differential"),
                               p("From this data, models can be developed to predict a team's bowl eligibility 
                                 based on their stats, and can be used to set statistical targets for future teams 
                                 aiming to reach bowl eligibility."),
                               a(href = "https://collegefootballdata.com/", "Click here for more information on CFBD."),
                               br(),
                               br(),
                               img(src = "LetterLogo.png")),
                      tabPanel("Data Exploration",
                               plotOutput("EDAPlot", height = "700")),
                      tabPanel("Modeling", 
                               tabsetPanel(id = "tabs2", type = "tabs",
                                           tabPanel("Modeling Info",
                                                    br(),
                                                    p("Two models will be created to model FBS bowl eligibility. 
                                                      The two classification method will be generalized linear 
                                                      regression (GLM) and random forests."),
                                                    p("GLM models are a variant of linear models that connect the 
                                                      output to the prediction values by means of a link function. 
                                                      It produces coefficients for each parameter (including the 
                                                      intercept) that takes the form"),
                                                    withMathJax(p("$$Y=X\\beta$$")),
                                                    withMathJax(p("where Y is the predicted response, 
                                                                   X is the predictors,
                                                                  and the coefficients 
                                                                  $$\\beta = (X^tX)^{-1}(X^tY)$$")),
                                                    p("Random forest models build multiple tree-based claassification 
                                                      models, each using a subset of the available predictors. The 
                                                      resulting predictor is the most prevalent of each model's predictions. 
                                                      This provides an advantage over standard tree based models by reducing 
                                                      the variance of the model and increasing resillience to changes in values."),
                                                    p("One of the biggest advantages of random forests is their built-in 
                                                      variable selection, meaning that which variables are used to model do not have 
                                                      to be filtered beforehand to select relevant variables. However, this 
                                                      comes at a significant overhead when compared to GLM models."),
                                                    p("Due to heavy correlations between certain predictors, the only variables 
                                                      selected will be"),
                                                    p("- Conference"),
                                                    p("- Average net passing yards"),
                                                    p("- Average rushing yards"),
                                                    p("- Average first downs"),
                                                    p("- Third down conversion rate"),
                                                    p("- Fourth down conversion rate"),
                                                    p("- Turnover differential"),
                                                    p("- Average time of possession (seconds)"),
                                                    p("- Average sacks"),
                                                    p("- Average interception yards"),
                                                    p("- Average punt return yards."),
                                                    p("The aim of this variable selection is to reduce the correlation 
                                                      between predictors while covering all facets of the game 
                                                      (offense, defense, special teams). The models will be judged on 
                                                      log loss, where lower values are better. For replication purposes, 
                                                      the random seed 1647 will be used.")
                                                    ),
                                           tabPanel("Model Fitting",
                                                    "Summary of generalized linear model and fit statistics:",
                                                    verbatimTextOutput("glmSummary"),
                                                    "The generalized linear model has log loss:",
                                                    verbatimTextOutput("glmLogLoss"),
                                                    "Applying the model to the unselected testing data produces 
                                                        the following confusion matrix:",
                                                    verbatimTextOutput("glmErrorMatrix"),
                                                    "Most important variables in random forest model:",
                                                    plotOutput("rfPlot", height = "700"),
                                                    "The random forest model has log loss:",
                                                    verbatimTextOutput("rfLogLoss"),
                                                    "Applying the model to the unselected testing data produces 
                                                        the following confusion matrix:",
                                                    verbatimTextOutput("rfErrorMatrix")),
                                           tabPanel("Prediction",
                                                    "The generalized linear model predicts that this team will 
                                                    reach bowl eligibility:",
                                                    verbatimTextOutput("glmPred"),
                                                    "The random forest model predicts that this team will 
                                                    reach bowl eligibility:",
                                                    verbatimTextOutput("rfPred"))))
          )
        )
    )
))
