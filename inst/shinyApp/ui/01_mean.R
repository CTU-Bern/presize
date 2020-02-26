# mean
# UI ----
meanpage <- tabItem(tabName = "mean",
        h2("Precision of a mean"),
        "Enter the mean and standard deviation you expect. To estimate the confidence interval width from a population of size X, enter the population size in 'Number of observations'. To estimate the number of observations required to get a confidence interval width of X, enter the width in 'Confidence interval width'.",
        tags$br(),
        h4("Please enter both of the following"),
        numericInput("mean_mean", "Mean",
                     value = 10),
        numericInput("mean_sd", "Standard deviation",
                     value = 2),
        tags$br(),
        h4("Please enter one of the following"),
        numericInput("mean_n", "Number of observations",
                     value = NULL),
        numericInput("mean_ciwidth", "Confidence interval width",
                     value = NULL),
        tags$hr(),
        h4("Results"),
        verbatimTextOutput("mean_out"),
        tableOutput("mean_tab"),
        "Code to replicate in R:",
        verbatimTextOutput("mean_code")

)

# SERVER ----
mean_fn <- function(input, code){
        if(is.na(input$mean_n) & is.na(input$mean_ciwidth)) {
                cat("Awaiting 'number of observations' or 'confidence interval width'")
        } else {
                z <- ifelse(is.na(input$mean_n),
                            paste0(", conf.width = ", input$mean_ciwidth),
                            paste0(", n = ", input$mean_n))
                x <- paste0("prec_mean(mu = ", input$mean_mean, ", sd = ", input$mean_sd,
                            z, ", conf.level = ", input$conflevel, ")")
                if(code){
                        cat(x)
                } else {
                        eval(parse(text = x))
                }
        }
}

