library(shiny)
library(DT)
library(ggvis)

## Caret and modeling packages
library(caret)
library(gbm)
library(glmnet)
library(kernlab)
library(MASS)
library(nnet)
library(pls)
library(randomForest)

## Observed and predicted clinical outcomes
load("Outcomes.RData")

## Global variables
offsetdiff <- 5
digits <- 4
altlabel <- character(0)

## Format outcome range for display
format.range <- function(range) {
  paste0("[", range[1], ", ", range[2], "]")
}
  
## ggvis tooltip function
samplesize.label <- function(x) {
  if(is.null(x)|| !all(c("pct", "N") %in% names(x))) return(NULL)
  paste0("Rel Diff: ", x$pct, "%<br />",
         "Abs Diff: ", altlabel[x$index], "<br />",
         "Sample Size: ", x$N)
}

## Descriptive statistics
describe <- function(x, digits = max(3, getOption("digits") - 3)) {
  stats <- c(mean(x), sd(x), quantile(x, c(0, 0.25, 0.5, 0.75, 1)))
  structure(signif(stats, digits), names = c("Mean", "Std Dev", "Min", "25th",
                                             "Median", "75th", "Max"))
}

## Server functionality
shinyServer(function(input, output, session) {
  
  observeEvent(input$outcome, {
    updateSelectInput(session, "metric", choices = OutcomeVars[[input$outcome]])
  })
  
  observeEvent(input$metric, {
    val <- range(AllVals()$pred)
    updateSliderInput(session, "range", value = val, min = val[1], max = val[2])
  })
  
  observeEvent(input$groups, {
    updateNumericInput(session, "ratio",
                       value = switch(input$groups,
                                      "Treatment Only" = 0,
                                      "Treatment and Control" = 1))
  })
  
  observeEvent(input$mindiff, {
    if(!is.finite(input$mindiff)) {
      NULL
    } else if(input$mindiff < 0) {
      updateNumericInput(session, "mindiff", value = 0)
    } else if(!is.finite(input$maxdiff)) {
      updateNumericInput(session, "maxdiff", value = input$mindiff + offsetdiff)
    } else if(input$mindiff > input$maxdiff - offsetdiff) {
      updateNumericInput(session, "mindiff", value = input$maxdiff - offsetdiff)
    } else {
      updateNumericInput(session, "maxdiff", min = input$mindiff + offsetdiff)
    }
  })
  
  observeEvent(input$maxdiff, {
    if(!is.finite(input$maxdiff)) {
      NULL
    } else if(!is.finite(input$mindiff)) {
      updateNumericInput(session, "mindiff", value = input$maxdiff - offsetdiff)
    } else if (input$mindiff > input$maxdiff - offsetdiff) {
      updateNumericInput(session, "maxdiff", value = input$mindiff + offsetdiff)
    } else {
      updateNumericInput(session, "mindiff", max = input$maxdiff - offsetdiff)
    }
  })
  
  AllVals <- reactive({
    OutcomeVals[[input$metric]]
  })
  
  Vals <- reactive({
    subset(AllVals(), pred >= input$range[1] & pred <= input$range[2])
  })
  
  samplesize.data <- reactive({
    cohort <- c("All", format.range(input$range))
    df <- if(is.finite(input$mindiff) && is.finite(input$maxdiff)) {
      n <- 11
      data.frame(
        cohort = factor(rep(cohort, times = n), levels = cohort),
        pct = round(rep(seq(input$mindiff, input$maxdiff, length = n),
                        each = 2), 1)
      )
    } else {
      data.frame(
        cohort = factor(cohort, levels = cohort),
        pct = 100
      )
    }
    
    df$nullmean <- c(mean(AllVals()$obs), mean(Vals()$obs))
    df$meandiff <- df$nullmean * df$pct / 100

    if(input$alternative == "Two-sided") {
      alpha <- input$alpha / 2
      plusminus <- "+/-"
    } else if(input$alternative == "Less Than") {
      alpha <- input$alpha
      plusminus <- "-"
    } else if(input$alternative == "Greater Than") {
      alpha <- input$alpha
      plusminus <- "+"
    }
    altlabel <<- paste(signif(df$nullmean, digits), plusminus,
                       signif(df$meandiff, digits))
    
    sigmasq <- c(var(AllVals()$obs), var(Vals()$obs))
    df$N <- ceiling(sigmasq * (1 + input$ratio) *
                      ((qnorm(input$power) + qnorm(1 - alpha)) / df$meandiff)^2)
    
    df$index <- 1:nrow(df)
    df$x <- df$N
    df$y <- df$pct

    df
  })
  
  samplesize <- reactive({
    samplesize.data() %>%
      ggvis(x = ~ N, y = ~ pct, stroke = ~ cohort) %>%
      add_axis("x", title = "Treatment Group Sample Size") %>%
      add_axis("y", title = "Relative Difference (%)") %>%
      add_legend("stroke", title = "Cohort") %>%
      layer_lines(x = ~ x, y = ~ y) %>%
      layer_points(key := ~ index) %>%
      add_tooltip(samplesize.label, "hover")
  })
  
  modelFit <- reactive({
    OutcomeBest[[input$metric]]
  })
  
  output$summary <- renderPrint({
    n <- nrow(AllVals())
    m <- nrow(Vals())
    cat("Subjects with outcome measurements: ", n, "\n",
        "Predicted to be in ", format.range(input$range), ": ", m,
        " (", round(100 * m / n, 1), "%)\n\n", sep="")
    describe(Vals()$obs, digits=digits)
  })
  
  bind_shiny(samplesize, "ssPlot")
  
  output$ssTable <- DT::renderDataTable({
    df <- subset(samplesize.data(),
                 select = c(cohort, nullmean, pct, meandiff, N)) %>%
          transform(nullmean = signif(nullmean, digits),
                    meandiff = signif(meandiff, digits))
    m <- nrow(df)
    n <- ncol(df)
    datatable(df,
              colnames = c("Cohort", "Null Mean", "Relative Difference (%)",
                           "Absolute Difference", "Treatment Group Sample Size"),
              rownames = FALSE,
              options = list(order = list(list(0, 'desc')),
                             columnDefs = list(list(className = 'dt-center',
                                                    width = '100px',
                                                    targets = 1:n - 1)),
                             pageLength = m / 2,
                             lengthMenu = c(m / 2, m),
                             searching = FALSE))
  })
  
  output$modelInfo <- renderPrint({
    print(modelFit())
  })
  
  output$varImp <- renderPlot({
    plot(varImp(modelFit(), nonpara=FALSE),
         top = 20, main = "Top 20 Baseline Factors",
         xlab = "Relative Importance",
         scales = list(tck = c(1, 0)))
  })
  
})
