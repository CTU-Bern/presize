# rate ratio
# UI ----
raterpage <- tabItem(tabName = "rateratio",
        h2("Precision of a rate ratio"),
        "Enter the proportions of events you expect in the groups. If you intend to use uneven allocation ratios (e.g. 2 allocated to group 1 for each participant allocated to group 2), adjust the allocation ratio accordingly. To estimate the confidence interval width from a number of events, enter the number of events in 'Number of events'. To estimate the number of observations required to get a confidence interval width of X, enter the width in 'Confidence interval width'.",
        tags$br(),
        h4("Please enter the following"),
        numericInput("rateratio_rate_exp", "Event rate in the exposed group",
                     value = NULL),
        numericInput("rateratio_rate_control", "Event rate in the control group",
                     value = NULL),
        numericInput("rateratio_r", "Allocation ratio", value = 1),
        h4("Please enter one of the following"),

        numericInput("rateratio_n_exp", "Number of observations in the exposed group", value = NULL),
        "(the number of observations in group 2 is estimated based on the number of exposed individuals and the allocation ratio)",
        numericInput("rateratio_ciwidth", "Confidence interval width",
                     value = NULL),
        h4("Results"),
        verbatimTextOutput("rateratio_out"),
        "Code to replicate in R:",
        verbatimTextOutput("rateratio_code"),
        h4("References"),
        "Rothamn KJ, Greenland S (2018) Planning Study Size Based on Precision Rather Than Power.", tags$i("Epidemiology"), "29:599-603", tags$a(href = "https://doi.org/10.1097/EDE.0000000000000876","doi:10.1097/EDE.0000000000000876")

)

# SERVER ----
rateratio_fn <- function(input, code = FALSE){
        if(is.na(input$rateratio_n_exp) & is.na(input$rateratio_ciwidth)) {
                cat("Awaiting 'number of observations' or 'confidence interval width'")
        } else {
                z <- ifelse(is.na(input$rateratio_n_exp),
                            paste0(", conf.width = ", input$rateratio_ciwidth),
                            paste0(", n1 = ", input$rateratio_n_exp))
                x <- paste0("prec_rateratio(rate1 = ", input$rateratio_rate_exp,
                            ", rate2 = ", input$rateratio_rate_control,
                            ", r = ", input$rateratio_r,
                            z, ", conf.level = ", input$conflevel,
                            ")")
                if(code){
                        cat(x)
                } else {
                        eval(parse(text = x))
                }
        }
}
