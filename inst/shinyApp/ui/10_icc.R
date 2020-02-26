# ICC
# UI ----
iccpage <- tabItem(tabName = "icc",
        h2("Precision of an intraclass correlation coefficient"),
        "Enter the correlation coefficient you expect. To estimate the confidence interval width from a number of events, enter the number of events in 'Number of events'. To estimate the number of observations required to get a confidence interval width of X, enter the width in 'Confidence interval width'.",
        h4("Please enter the following"),
        numericInput("icc_rho", "Intraclass correlation coefficient",
                     value = 0, min = 0, max = 1),
        numericInput("icc_k", "Number of observations per subject", value = NULL),
        h4("Please enter one of the following"),
        numericInput("icc_n", "Number of subjects", value = NULL),
        numericInput("icc_ciwidth", "Confidence interval width",
                     value = NULL, min = 0, max = 1),
        h4("Results"),
        verbatimTextOutput("icc_out"),
        tableOutput("icc_tab"),
        "Code to replicate in R:",
        verbatimTextOutput("icc_code"),
        h4("References"),
        "Bonett DG (2002). Sample size requirements for estimating intraclass correlations with desired precision.", tags$i("Statistics in Medicine"), "21:1331-1335. ", tags$a(href = "https://doi.org/10.1002/sim.1108", "doi: 10.1002/sim.1108")

)

# SERVER ----
icc_fn <- function(input, code = FALSE){
        if(is.na(input$icc_n) & is.na(input$icc_ciwidth)) {
                cat("Awaiting 'number of observations' or 'confidence interval width'")
        } else {
                z <- ifelse(is.na(input$icc_n),
                            paste0(", conf.width = ", input$icc_ciwidth),
                            paste0(", n = ", input$icc_n))
                x <- paste0("prec_icc(rho = ", input$icc_rho, ", k = ",
                            input$icc_k, ", ",
                            z, ", conf.level = ", input$conflevel,
                            ")")
                if(code){
                        cat(x)
                } else {
                        eval(parse(text = x))
                }
        }
}

