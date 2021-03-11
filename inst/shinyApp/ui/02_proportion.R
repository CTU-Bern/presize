# proportion page
# UI ----
proportionpage <- tabItem(tabName = "proportion",
        h2("Precision of a proportion"),
        "Enter the proportion you expect. To estimate the confidence interval width from a population of size X, enter the population size in 'Number of observations'. To estimate the number of observations required to get a confidence interval width of X, enter the width in 'Confidence interval width'.",
        tags$br(),
        h4("Please enter the following"),
        sliderInput("prop_p", "Proportion",
                    value = .5, min = 0, max = 1),
        tags$br(),
        h4("Please enter one of the following"),
        # numericInput("prop_n", "Number of observations",
        #              value = NULL),
        # numericInput("prop_ciwidth", "Confidence interval width",
        #              value = NULL, min = 0, max = 1),
        uiOutput("prop_resetable_input"),
        actionButton("prop_reset_input",
                     "Reset 'Number of observations' or 'Confidence interval width'"),
        h4("Other settings"),
        "The Wilson confidence interval is recommended, but others are available.",
        selectInput("prop_method", label = "Confidence interval method",
                    choices = c("Wilson" = "wilson",
                                "Agresti-Coull" = "agresti-coull",
                                "Exact" = "exact",
                                "Wald" = "wald"),
                    selected = "wilson"),
        tags$hr(),
        h4("Results"),
        verbatimTextOutput("prop_out"),
        tableOutput("prop_tab"),
        "Code to replicate in R:",
        verbatimTextOutput("prop_code"),
        h4("References"),
        "Brown LD, Cai TT, DasGupta A (2001) Interval Estimation for a Binomial Proportion, ", tags$i("Statistical Science"), ", 16:2, 101-117, ", tags$a(href="doi:10.1214/ss/1009213286","doi:10.1214/ss/1009213286")
)

# SERVER ----
prop_fn <- function(input, code = FALSE) {
        if(is.na(input$prop_n) & is.na(input$prop_ciwidth)) {
                cat("Awaiting 'number of observations' or 'confidence interval width'")
        } else {
                z <- ifelse(is.na(input$prop_n),
                            paste0("conf.width = ", input$prop_ciwidth),
                            paste0("n = ", input$prop_n))
                x <- paste0("prec_prop(p = ", input$prop_p, ", ", z, ", conf.level = ",
                            input$conflevel, ", method = '", input$prop_method, "')")
                if(code){
                        cat(x)
                } else {
                        eval(parse(text = x))
                }
        }
}
