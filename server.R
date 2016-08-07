library(shiny)

shinyServer(function(input, output, session) {
  
  observe({
    if(input$groups == "Treatment Only") {
      updateNumericInput(session, "ratio", value = 1)
    }
  })
  
  observeEvent(input$mindiff, {
    if(input$mindiff <= input$maxdiff) {
      updateNumericInput(session, "maxdiff", min = input$mindiff)
    } else {
      updateNumericInput(session, "mindiff", value = input$maxdiff)
    }
  })
  
  observeEvent(input$maxdiff, {
    if(input$mindiff <= input$maxdiff) {
      updateNumericInput(session, "mindiff", max = input$maxdiff)
    } else {
      updateNumericInput(session, "maxdiff", value = input$mindiff)
    }
  })
  
  observeEvent(input$outcome, {
    val <- range(AllVals()$pred)
    updateSliderInput(session, "range", value = val,
                      min = val[1], max = val[2])
  })
  
  AllVals <- reactive({
    OutcomeVals[[input$outcome]]
  })
  
  Vals <- reactive({
    subset(AllVals(), pred >= input$range[1] & pred <= input$range[2])
  })
  
  nullmean <- reactive({
    mean(Vals()$obs)
  })
  
  output$nullmean <- renderText({
    signif(nullmean(), 4)
  })
  
  output$summary <- renderPrint({
    n <- nrow(AllVals())
    m <- nrow(Vals())
    cat("Subjects with outcome measurements = ", n, "\n",
        "Number within range = ", m,
        " (", round(100 * m / n, 1), "%)\n\n", sep="")
    summary(Vals()$obs)
  })
  
  output$samplesize <- renderPlot({
    s2 <- var(Vals()$obs)
    alpha <- input$alpha * switch(input$alternative,
                                  "Two-sided" = 0.5,
                                  "One-sided" = 1,
                                  NA)
    p <- seq(input$mindiff, input$maxdiff)
    n <- s2 * (1 + 1 / input$ratio) *
           ((qnorm(input$power) + qnorm(1 - alpha)) /
             (nullmean() * p / 100))^2
    par(mar=c(5,4,1,4))
    plot(n, p, xlab="Treatment Group Sample Size",
         ylab="Mean Difference (%)", type = "n", axes = FALSE)
    abline(v = seq(min(n), max(n), length = 5), col = "gray85")
    abline(h = seq(min(p), max(p), length = 5), col = "gray85")
    box()
    lines(n, p, type = ifelse(length(p) == 1, "p", "l"))
    axis(1)
    axis(2)
    altmean <- pretty((1 + axTicks(2) / 100) * nullmean())
    axis(4, at=(altmean / nullmean() - 1) * 100, labels=altmean)
    mtext("Alternative Mean", 4, line=3)
  })
  
})
