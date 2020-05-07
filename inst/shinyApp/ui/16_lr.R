# specificity
# UI ----
lrpage <- tabItem(tabName = "lr",
        h2("Likelihood ratios"),
        "The method available here (formula 10 from Simel et al) allows for many scenarios. The appropriate proportions should be entered.",
        tags$br(),
        "Groups here refer to e.g. the disease status.",
        tags$br(),
        "To compute for the positive likelihood ratio, enter the sensitivity in the proportion for group 1 and 1 - specificity in the proportion for group 2.",
        tags$br(),
        "To compute for the negative likellihood ratio, enter 1-specificity in the proportion for group 1 and specificity in the proportion for group 2.",
        tags$br(),
        "The method can also be used for conditional likelihoods by using the appropriate proportions (e.g. the proportion of positive or negative tests against inconclusive ones, i.e. yields), analogous to the simple case described above.",
        h4("Please enter the following"),
        sliderInput("lr_prev", "Prevalence", min = 0, max = 1, value = NULL),
        sliderInput("lr_p1", "Proportion of events in group 1",
                    min = 0, max = 1, value = NULL),
        sliderInput("lr_p2", "Proportion of events in group 2",
                    min = 0, max = 1, value = NULL),
        h4("Please enter one of the following"),
        numericInput("lr_n", "Total sample size",
                     value = NULL),
        numericInput("lr_ciwidth", "Confidence interval width",
                     value = NULL, min = 0, max = 1),
        "The number in each group is calculated as sample size * prev, which can result in fractions so rounding is necessary.",
        h4("Results"),
        verbatimTextOutput("lr_out"),
        tableOutput("lr_tab"),
        "Code to replicate in R:",
        verbatimTextOutput("lr_code"),
        h4("References"),
        "Simel, DL, Samsa, GP and Matchar, DB (1991) Likelihood ratios with confidence: Sample size estimation for diagnostic test studies. ", tags$i("J Clin Epidemiol"), "44(8), 763-770, DOI 10.1016/0895-4356(91)90128-v"

)

# SERVER ----
lr_fn <- function(input, code = FALSE){
        if(is.na(input$lr_n) & is.na(input$lr_ciwidth)) {
                cat("Awaiting 'number of observations' or 'confidence interval width'")
        } else {
                z <- ifelse(is.na(input$lr_n),
                            paste0("conf.width = ", input$lr_ciwidth),
                            paste0("n = ", input$lr_n))
                x <- paste0("prec_lr(prev = ", input$lr_prev,
                            ", p1 = ", input$lr_p1,
                            ", p2 = ", input$lr_p2,
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
