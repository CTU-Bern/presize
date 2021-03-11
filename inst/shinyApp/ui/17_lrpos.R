# LR positive
# UI ----
lrppage <- tabItem(tabName = "lrpos",
        h2("Positive likelihood ratio"),
        "Calculate precision or sample size for the positive likelihood ratio based on sensitivity and specificity. Formula 10 from Simel et al is used.",
        tags$br(),
        "Groups here refer to e.g. the disease status.",
        tags$br(),
        "",
        h4("Please enter the following"),
        sliderInput("lrp_prev", "Prevalence",
                    min = 0, max = 1, value = .5),
        sliderInput("lrp_sens", "Sensitivity",
                    min = 0, max = 1, value = .5),
        sliderInput("lrp_spec", "Specificity",
                    min = 0, max = 1, value = .5),
        h4("Please enter one of the following"),
        numericInput("lrp_n", "Total sample size",
                     value = NULL),
        "The number in each group (e.g. diseased and healthy) is calculated as sample size * prevalence (group 1, which might be diseased people) and sample size * 1-prevalence (group 2).",
        numericInput("lrp_ciwidth", "Confidence interval width",
                     value = NULL, min = 0, max = 1),

        h4("Results"),
        verbatimTextOutput("lrp_out"),
        tableOutput("lrp_tab"),
        "Code to replicate in R:",
        verbatimTextOutput("lrp_code"),
        h4("References"),
        "Simel, DL, Samsa, GP and Matchar, DB (1991) Likelihood ratios with confidence: Sample size estimation for diagnostic test studies. ", tags$i("J Clin Epidemiol"), "44(8), 763-770, DOI 10.1016/0895-4356(91)90128-v"

)

# SERVER ----
lrp_fn <- function(input, code = FALSE){
        if(is.na(input$lrp_n) & is.na(input$lrp_ciwidth)) {
                cat("Awaiting 'number of observations' or 'confidence interval width'")
        } else {
                z <- ifelse(is.na(input$lrp_n),
                            paste0("conf.width = ", input$lrp_ciwidth),
                            paste0("n = ", input$lrp_n))
                x <- paste0("prec_pos_lr(prev = ", input$lrp_prev,
                            ", sens = ", input$lrp_sens,
                            ", spec = ", input$lrp_spec,
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
