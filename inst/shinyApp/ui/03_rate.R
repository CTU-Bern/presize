# rate page
# UI ----
ratepage <- tabItem(
    tabName = "rate",
    h2("Precision of a rate"),
    "Enter the rate you expect. To estimate the confidence interval width from a number of events, enter the number of events in 'Number of events'. To estimate the number of observations required to get a confidence interval width of X, enter the width in 'Confidence interval width'.",
    tags$br(),
    h4("Please enter the following"),
    numericInput(
      "rate_r", "Rate", value = .5, min = 0, max = 1
    ),
    h4("Please enter one of the following"),
    uiOutput("rate_resetable_input"),
    actionButton("rate_reset_input",
                 "Reset 'Number of events' or 'Confidence interval width'"),
    # numericInput("rate_x", "Number of Events", value = NULL),
    # numericInput("rate_ciwidth", "Confidence interval width",
    #              value = NULL),
    h4("Other settings"),
    selectInput("rate_method",
      label = "Confidence interval method",
      choices = c(
        "Score" = "score",
        "Variance stabilizing" = "vs",
        "Exact" = "exact",
        "Wald" = "wald"
      ),
      selected = "wilson"
    ),
    tags$hr(),
    h4("Results"),
    verbatimTextOutput("rate_out"),
    tableOutput("rate_tab"),
    "Code to replicate in R:",
    verbatimTextOutput("rate_code"),
    h4("References"),
    "Barker, L. (2002) A Comparison of Nine Confidence Intervals for a Poisson Parameter When the Expected Number of Events is ≤ 5, ",
    tags$i("The American Statistician"),
    ", 56:2, 85-89, ",
    tags$a(href = "https://doi.org/10.1198/000313002317572736", "DOI: 10.1198/000313002317572736")
  )

# SERVER ----
rate_fn <- function(input, code = FALSE){
  if(is.na(input$rate_x) & is.na(input$rate_ciwidth)) {
    cat("Awaiting 'number of observations' or 'confidence interval width'")
  } else {
    z <- ifelse(is.na(input$rate_x),
                paste0("conf.width = ", input$rate_ciwidth),
                paste0("x = ", input$rate_x))
    x <- paste0("prec_rate(r = ", input$rate_r, ", ", z, ", conf.level = ",
                input$conflevel, ", method = '", input$rate_method, "')")
    if(code){
      cat(x)
    } else {
      eval(parse(text = x))
    }
  }
}
