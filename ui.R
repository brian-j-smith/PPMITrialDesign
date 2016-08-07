library(shiny)

load("Outcomes.RData")

shinyUI(
  fluidPage(
    titlePanel("PPMI Trial Design Tool"),
    sidebarLayout(
      sidebarPanel(
        fluidRow(
          column(12,
            selectInput("outcome",
                        "Clinical Outcome",
                        OutcomeVars),
            sliderInput("range",
                        "Predicted Outcome Range",
                        min = 0,
                        max = 1,
                        value = c(0, 1)),
            strong("Null Mean:"),
            textOutput("nullmean",
                       container = span),
            hr(),
            strong("Mean Difference (%)"),
            fluidRow(
              column(6,
                numericInput("mindiff",
                             "Min",
                             value = 100,
                             max = 100,
                             step = 5)
              ),
              column(6,
                numericInput("maxdiff",
                             "Max",
                             value = 100,
                             min = 100,
                             step = 5)
              )
            ),
            radioButtons("groups",
                         "Study Groups",
                         c("Treatment Only", "Treatment and Control")),
            conditionalPanel(
              condition = "input.groups == 'Treatment and Control'",
              numericInput("ratio",
                           "Treatment:Control Ratio",
                           value = 1,
                           min = 0.1,
                           step = 0.1)
            ),
            radioButtons("alternative",
                         "Alternative Hypothesis",
                         c("Two-sided", "One-sided"),
                         inline = TRUE),
            numericInput("alpha",
                         "Significance Level",
                         value = 0.05,
                         min = 0.01,
                         max = 0.99,
                         step = 0.01),
            numericInput("power",
                         "Power",
                         value = 0.80,
                         min = 0.01,
                         max = 0.99,
                         step = 0.01)
          )
        )
      ),
      mainPanel(
        h4("PD Cohort Statistics"),
        verbatimTextOutput("summary"),
        br(),
        h4("Sample Size Estimates"),
        plotOutput("samplesize")
      )
    )
))
