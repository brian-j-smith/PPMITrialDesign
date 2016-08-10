library(shiny)
library(ggvis)

## Global variables
offsetdiff <- 5
digits <- 4
labels <- character(0)

## Format outcome range for display
format.range <- function(range) {
  paste0("[", range[1], ", ", range[2], "]")
}
  
## ggvis tooltip function
samplesize.label <- function(x) {
  if(is.null(x)|| !all(c("pct", "N") %in% names(x))) return(NULL)
  paste0("Difference: ", round(x$pct, 1), "%<br />",
         "Alternative Mean: ", labels[x$index], "<br />",
         "Sample Size: ", ceiling(x$N))
}

## Server functionality
shinyServer(function(input, output, session) {
  
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
  
  observeEvent(input$outcome, {
    val <- range(AllVals()$pred)
    updateSliderInput(session, "range", value = val, min = val[1], max = val[2])
  })
  
  AllVals <- reactive({
    OutcomeVals[[input$outcome]]
  })
  
  Vals <- reactive({
    subset(AllVals(), pred >= input$range[1] & pred <= input$range[2])
  })
  
  output$nullmean <- renderText({
    signif(mean(Vals()$obs), digits)
  })
  
  output$summary <- renderPrint({
    n <- nrow(AllVals())
    m <- nrow(Vals())
    cat("Subjects with outcome measurements: ", n, "\n",
        "Number within ", format.range(input$range), ": ", m,
        " (", round(100 * m / n, 1), "%)\n\n", sep="")
    summary(Vals()$obs)
  })
  
  samplesize <- reactive({
    cohort <- c("All", format.range(input$range))
    df <- if(is.finite(input$mindiff) && is.finite(input$maxdiff)) {
      n <- 11
      data.frame(
        cohort = factor(rep(cohort, times = n), levels = cohort),
        pct = rep(seq(input$mindiff, input$maxdiff, length = n), each = 2)
      )
    } else {
      data.frame(
        cohort = factor(cohort, levels = cohort),
        pct = 100
      )
    }
    
    nullmean <- c(mean(AllVals()$obs), mean(Vals()$obs))
    df$meandiff <- nullmean * df$pct / 100

    if(input$alternative == "Two-sided") {
      alpha <- input$alpha / 2
      labels <<- paste(signif(nullmean - df$meandiff, digits),
                       "or",
                       signif(nullmean + df$meandiff, digits))
    } else if(input$alternative == "Less Than") {
      alpha <- input$alpha
      labels <<- as.character(signif(nullmean - df$meandiff, digits))
    } else if(input$alternative == "Greater Than") {
      alpha <- input$alpha
      labels <<- as.character(signif(nullmean + df$meandiff, digits))
    }

    sigmasq <- c(var(AllVals()$obs), var(Vals()$obs))    
    df$N <- sigmasq * (1 + input$ratio) *
              ((qnorm(input$power) + qnorm(1 - alpha)) / df$meandiff)^2
    
    df$index <- 1:nrow(df)
    df$x <- df$N
    df$y <- df$pct

    df %>%
      ggvis(x = ~ N, y = ~ pct, stroke = ~ cohort) %>%
      add_axis("x", title = "Treatment Group Sample Size") %>%
      add_axis("y", title = "Mean Difference (%)") %>%
      add_legend("stroke", title = "Cohort") %>%
      layer_lines(x = ~ x, y = ~ y) %>%
      layer_points(key := ~ index) %>%
      add_tooltip(samplesize.label, "hover")
  })
  
  samplesize %>% bind_shiny("samplesize")
  
})
