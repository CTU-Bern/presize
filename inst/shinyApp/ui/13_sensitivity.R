# sensitivity
# UI ----
senspage <- tabItem(tabName = "sens",
        h2("Precision of sensitivity"),
        "",
        tags$br(),
        h4("Please enter the following"),
        sliderInput("sens_sens", "Sensitivity", min = 0, max = 1, value = NULL),
        sliderInput("sens_prev", "Prevalence", min = 0, max = 1, value = NULL),
        tags$br(),
        h4("Please enter one of the following"),
        numericInput("sens_ntot", "Total sample size",
                     value = NULL),
        numericInput("sens_ciwidth", "Confidence interval width",
                     value = NULL, min = 0, max = 1),
        h4("Optional parameters"),
        selectInput("sens_method", label = "Confidence interval method",
                    choices = c("Wilson" = "wilson",
                                "Agresti-Coull" = "agresti-coull",
                                "Exact" = "exact",
                                "Wald" = "wald"),
                    selected = "wilson"),
        selectInput("sens_round", label = "When rounding the number of cases, round...",
                    choices = c("up" = "ceiling",
                                "down" = "floor"),
                    selected = "up"),
        "The number of cases is calculated as sample size * prev, which can result in fractions so rounding is necessary.",
        tags$hr(),
        verbatimTextOutput("sens_out"),
        "Code to replicate in R:",
        verbatimTextOutput("sens_code"),
        h4("References"),
        "Brown LD, Cai TT, DasGupta A (2001) Interval Estimation for a Binomial Proportion, ", tags$i("Statistical Science"), ", 16:2, 101-117, ", tags$a(href="doi:10.1214/ss/1009213286","doi:10.1214/ss/1009213286")

)

# SERVER ----
sens_fn <- function(input, code){
        if(is.na(input$sens_ntot) & is.na(input$sens_ciwidth)) {
                cat("Awaiting 'number of observations' or 'confidence interval width'")
        } else {
                z <- ifelse(is.na(input$sens_ntot),
                            paste0("conf.width = ", input$sens_ciwidth),
                            paste0("ntot = ", input$sens_ntot))
                x <- paste0("prec_sens(sens = ", input$sens_sens,
                            ", prev = ", input$sens_prev,
                            ", ", z, ", conf.level = ", input$conflevel,
                            ", round = '", input$sens_round,
                            "', method = '", input$sens_method, "')")
                if(code){
                        cat(x)
                } else {
                        eval(parse(text = x))
                }
        }
}
