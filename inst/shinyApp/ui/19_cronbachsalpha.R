# Cohens kappa ----
# UI ----
calphapage <- tabItem(tabName = "calpha",
        h2("Cronbach's alpha"),
        "Cronbach's alpha is used to assess the internal consistency of tests and measures.",
        tags$br(),
        h4("Please enter all of the following parameters:"),
        numericInput("calpha_k", "Number of measurements/items in test", min = 1, step = 1, value = 1),
        sliderInput("calpha", "Desired/expected Cronbach's alpha", min = 0, max = 1, step = .01, value = .5),
        h4("Please enter one of the following:"),
        uiOutput("calpha_resetable_input"),
        actionButton("calpha_reset_input",
                     "Reset 'Sample size' or 'Confidence interval width'"),
        h4("Result"),
        verbatimTextOutput("calpha_out"),
        tableOutput("calpha_tab"),
        "Code to replicate in R:",
        verbatimTextOutput("calpha_code"),
        h4("References"),
        "Bonett & Wright (2015) Cronbach's alpha reliability: Interval estimation, hypothesis testing, and sample size planning.", tags$i("Journal of Organizational Behaviour"), "36(1):3-15", tags$a(href = "https://doi.org/10.1002/job.1960", "DOI: 10.1002/job.1960")
)

# SERVER ----
calpha_fn <- function(input, code = FALSE){
        db(input, "calpha")
        if(is.na(input$calpha_n) & is.na(input$calpha_ciwidth)) {
                cat("Awaiting 'number of observations' or 'confidence interval width'")
        } else {
                z <- ifelse(is.na(input$calpha_n),
                            paste0("conf.width = ", input$calpha_ciwidth),
                            paste0("n = ", input$calpha_n))
                props <- unlist(strsplit(input$kappa_props, ","))
                props <- as.numeric(trimws(props))
                props <- paste("c(", paste(props, collapse = ", "), ")", sep = "")
                x <- sprintf("prec_cronb(k = %g, calpha = %g, %s, \n           conf.level = %s)", input$calpha_k, input$calpha, z, input$conflevel)
                if(code){
                        cat(x)
                } else {
                        eval(parse(text = x))
                }
        }
}


## data for baplot
# badat <- BlandAltmanLeh:::bland.altman.stats(rnorm(20), rnorm(20))
# dump("badat", "inst/extdata/baplotdata")

