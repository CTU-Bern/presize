# limit of agreement
# UI ----
limitpage <- tabItem(tabName = "limit",
        h2("Precision of limits of agreement"),
        "Bland-Altmann (also known as Tukey mean-difference) plots are often used to assess the agreement between two methods of measuring a quantity. A typical plot might look like the following figure. The blue line represents the mean difference between the methods, while the red lines represent the confidence interval of that difference (the limit of agreement). The dotted lines represent the confidence intervals around the limit of agreement.",
        plotOutput("limitplot"),
        "This page calculates the width of the confidence interval around the limit of agreement (as indicated by the black arrows), the width of which is only a function of sample size. To calculate the width of the confidence interval of the difference itself (e.g. the grey line), a paired mean difference can be used.",
        tags$br(),
        "Enter the sample size or confidence interval width to calculate the other.",
        h4("Please enter one of the following"),
        numericInput("limit_n", "Sample size",
                     value = NULL),
        numericInput("limit_ciwidth", "Confidence interval width",
                     value = NULL, min = 0, max = 1),
        h4("Result"),
        verbatimTextOutput("limit_out"),
        tableOutput("limit_tab"),
        "Code to replicate in R:",
        verbatimTextOutput("limit_code"),
        h4("References"),
        "Bland & Altman (1986) Statistical methods for assessing agreement between two methods of clinical measurement.", tags$i("Lancet"), "i(8476):307-310", tags$a(href = "https://doi.org/10.1016/S0140-6736(86)90837-8", "doi: 10.1016/S0140-6736(86)90837-8")

)

# SERVER ----
limit_fn <- function(input, code= FALSE){
        if(is.na(input$limit_n) & is.na(input$limit_ciwidth)) {
                cat("Awaiting 'number of observations' or 'confidence interval width'")
        } else {
                z <- ifelse(is.na(input$limit_n),
                            paste0("conf.width = ", input$limit_ciwidth),
                            paste0("n = ", input$limit_n))
                x <- paste0("prec_lim_agree(", z, ", conf.level = ", input$conflevel,
                            ")")
                if(code){
                        cat(x)
                } else {
                        eval(parse(text = x))
                }
        }
}


