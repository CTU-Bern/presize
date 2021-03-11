# specificity
# UI ----
specpage <- tabItem(tabName = "spec",
        h2("Precision of specificity"),
        "Specificity is the proportion of negative test results that are identified as such. It is also known as the true negative rate. It is actually a simple proportion, but as the total sample size, rather than the number of non-cases, is typically of interect this function requires an estimate of the prevalence of cases.",
        h4("Please enter the following"),
        sliderInput("spec_spec", "Specificity",
                    min = 0, max = 1, value = .5),
        sliderInput("spec_prev", "Prevalence",
                    min = 0, max = 1, value = .4),
        h4("Please enter one of the following"),
        numericInput("spec_ntot", "Total sample size",
                     value = NULL),
        numericInput("spec_ciwidth", "Confidence interval width",
                     value = NULL, min = 0, max = 1),
        h4("Other settings"),
        selectInput("spec_method", label = "Confidence interval method",
                    choices = c("Wilson" = "wilson",
                                "Agresti-Coull" = "agresti-coull",
                                "Exact" = "exact",
                                "Wald" = "wald"),
                    selected = "wilson"),
        selectInput("spec_round", label = "When rounding the number of cases, round...",
                    choices = c("up" = "ceiling",
                                "down" = "floor"),
                    selected = "up"),
        "The number of cases is calculated as sample size * prev, which can result in fractions so rounding is necessary.",
        h4("Results"),
        verbatimTextOutput("spec_out"),
        tableOutput("spec_tab"),
        "Code to replicate in R:",
        verbatimTextOutput("spec_code"),
        h4("References"),
        "Brown LD, Cai TT, DasGupta A (2001) Interval Estimation for a Binomial Proportion, ", tags$i("Statistical Science"), ", 16:2, 101-117, ", tags$a(href="doi:10.1214/ss/1009213286","doi:10.1214/ss/1009213286")

)

# SERVER ----
spec_fn <- function(input, code = FALSE){
        if(is.na(input$spec_ntot) & is.na(input$spec_ciwidth)) {
                cat("Awaiting 'number of observations' or 'confidence interval width'")
        } else {
                z <- ifelse(is.na(input$spec_ntot),
                            paste0("conf.width = ", input$spec_ciwidth),
                            paste0("ntot = ", input$spec_ntot))
                x <- paste0("prec_spec(spec = ", input$spec_spec,
                            ", prev = ", input$spec_prev,
                            ", ", z, ", conf.level = ", input$conflevel,
                            ", round = '", input$spec_round,
                            "', method = '", input$spec_method, "')")
                if(code){
                        cat(x)
                } else {
                        eval(parse(text = x))
                }
        }
}
