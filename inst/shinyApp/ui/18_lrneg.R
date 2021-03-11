# LR positive
# UI ----
lrnpage <- tabItem(tabName = "lrneg",
        h2("Negative likelihood ratio"),
        "Calculate precision or sample size for the negative likelihood ratio based on sensitivity and specificity. Formula 10 from Simel et al is used.",
        tags$br(),
        "Groups here refer to e.g. the disease status.",
        tags$br(),
        "",
        h4("Please enter the following"),
        sliderInput("lrn_prev", "Prevalence",
                    min = 0, max = 1, value = .5),
        sliderInput("lrn_sens", "Sensitivity",
                    min = 0, max = 1, value = .5),
        sliderInput("lrn_spec", "Specificity",
                    min = 0, max = 1, value = .5),
        h4("Please enter one of the following"),
        numericInput("lrn_n", "Total sample size",
                     value = NULL),
        "The number in each group (e.g. diseased and healthy) is calculated as sample size * prevalence (group 1, which might be diseased people) and sample size * 1-prevalence (group 2).",
        numericInput("lrn_ciwidth", "Confidence interval width",
                     value = NULL, min = 0, max = 1),

        h4("Results"),
        verbatimTextOutput("lrn_out"),
        tableOutput("lrn_tab"),
        "Code to replicate in R:",
        verbatimTextOutput("lrn_code"),
        h4("References"),
        "Simel, DL, Samsa, GP and Matchar, DB (1991) Likelihood ratios with confidence: Sample size estimation for diagnostic test studies. ", tags$i("J Clin Epidemiol"), "44(8), 763-770, DOI 10.1016/0895-4356(91)90128-v"

)

# SERVER ----
lrn_fn <- function(input, code = FALSE){
        if(is.na(input$lrn_n) & is.na(input$lrn_ciwidth)) {
                cat("Awaiting 'number of observations' or 'confidence interval width'")
        } else {
                z <- ifelse(is.na(input$lrn_n),
                            paste0("conf.width = ", input$lrn_ciwidth),
                            paste0("n = ", input$lrn_n))
                x <- paste0("prec_neg_lr(prev = ", input$lrn_prev,
                            ", sens = ", input$lrn_sens,
                            ", spec = ", input$lrn_spec,
                            ", ", z,
                            ", conf.level = ", input$conflevel,
                            ")")
                if(code){
                        cat(x)
                } else {
                        eval(parse(text = x))
                }
        }
}
