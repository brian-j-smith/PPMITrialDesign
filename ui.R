library(shiny)
library(shinyBS)
library(DT)
library(ggvis)

## Observed and predicted clinical outcomes
load("Outcomes.RData")

## User interface
shinyUI(
  fluidPage(
    div(class = "outer",
    titlePanel("Parkinson's Clinical Trials Design Tool"),
    sidebarLayout(
      sidebarPanel(
        fluidRow(
          column(12,
            selectInput("outcome",
                        "Clinical Outcome",
                        names(OutcomeVars),
                        selected = AppInfo$selectedOutcome),
            selectInput("metric",
                        "Metric",
                        OutcomeVars[[AppInfo$selectedOutcome]]),
            sliderInput("range",
                        "Predicted Outcome Range",
                        min = 0,
                        max = 1,
                        value = c(0, 1)),
            bsTooltip("range",
                      "Predicted values of patients to include",
                      placement = "right",
                      options = list(container = "body")),
            selectInput("alternative",
                        "Alternative Hypothesis",
                        c("Two-sided", "Less Than", "Greater Than")),
            bsTooltip("alternative",
                      "Alternative hypothesis for the PD Cohort Mean",
                      placement = "right",
                      options = list(container = "body")),
            strong("Relative Mean Difference (%)"),
            fluidRow(
              column(6,
                numericInput("mindiff",
                             "Min",
                             value = 25,
                             min = 0,
                             max = 100,
                             step = 5)
              ),
              column(6,
                numericInput("maxdiff",
                             "Max",
                             value = 100,
                             min = 25,
                             step = 5)
              )
            ),
            bsTooltip("mindiff",
                      "Sets a minimum value for the alternative mean",
                      placement = "right",
                      options = list(container = "body")),
            bsTooltip("maxdiff",
                      "Sets a maximum value for the alternative mean",
                      placement = "right",
                      options = list(container = "body")),
            selectInput("groups",
                        "Study Groups",
                        c("Treatment Only", "Treatment and Control")),
            bsTooltip("groups",
                      "One or two-arm trial design",
                      placement = "right",
                      options = list(container = "body")),
            conditionalPanel(
              condition = "input.groups == 'Treatment and Control'",
              numericInput("ratio",
                           "Treatment:Control Ratio",
                           value = 0,
                           min = 0,
                           step = 0.1)
            ),
            bsTooltip("ratio",
                      "Planned enrollment ratio of Treatment to Control patients",
                      placement = "right",
                      options = list(container = "body")),
            numericInput("alpha",
                         "Significance Level",
                         value = 0.05,
                         min = 0.01,
                         max = 0.99,
                         step = 0.01),
            bsTooltip("alpha",
                      "Probability of a false positive test",
                      placement = "right",
                      options = list(container = "body")),
            numericInput("power",
                         "Power",
                         value = 0.80,
                         min = 0.01,
                         max = 0.99,
                         step = 0.01),
            bsTooltip("power",
                      "Probability of a true positive test",
                      placement = "right",
                      options = list(container = "body"))
          )
        )
      ),
      mainPanel(
        h4("PD Cohort Statistics"),
        verbatimTextOutput("summary"),
        h4("Sample Size Estimates"),
        tabsetPanel(type = "tabs",
          tabPanel("Plot",
                   br(),
                   ggvisOutput("ssPlot")),
          tabPanel("Table",
                   br(),
                   DT::dataTableOutput("ssTable")),
          tabPanel("Model Info",
                   verbatimTextOutput("modelInfo"),
                   plotOutput("varImp"))
        )
      )
    ),
    tags$div(id = "cite", HTML(paste("Version", AppInfo$version,
                                     "&copy 2016, Brian J Smith")))
  )
))
