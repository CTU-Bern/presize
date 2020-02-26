# mean diff
# UI ----
meandiffpage <- tabItem(tabName = "meandiff",
        h2("Precision of a mean difference"),
        "Enter the mean difference and standard deviations you expect. If you intend to use uneven allocation ratios (e.g. 2 allocated to group 1 for each participant allocated to group 2), adjust the allocation ratio accordingly. To estimate the confidence interval width from a number of events, enter the number of events in 'Number of events'. To estimate the number of observations required to get a confidence interval width of X, enter the width in 'Confidence interval width'.",
        tags$br(),
        h4("Please enter the following"),
        numericInput("meandiff_delta", "Mean difference",
                     value = 1),
        numericInput("meandiff_sd1", "Standard deviation of group 1",
                     value = NULL),
        checkboxInput("meandiff_var", label = "Group variances unequal"),
        conditionalPanel("input.meandiff_var == true",
                         numericInput("meandiff_sd2", "Standard deviation of group 2",
                                      value = NULL)),
        numericInput("meandiff_r", "Allocation ratio", value = 1),
        h4("Please enter one of the following"),
        numericInput("meandiff_n", "Number of observations in group 1", value = NULL),
        "(the number of observations in group 2 is estimated based on group 1 and the allocation ratio)",
        numericInput("meandiff_ciwidth", "Confidence interval width",
                     value = NULL),
        tags$hr(),
        h4("Results"),
        verbatimTextOutput("meandiff_out"),
        "Code to replicate in R:",
        verbatimTextOutput("meandiff_code")
)

# SERVER ----
meandiff_fn <- function(input, code){
        db(input, "meandiff")
        if(is.na(input$meandiff_n) & is.na(input$meandiff_ciwidth)) {
                cat("Awaiting 'number of observations' or 'confidence interval width'")
        } else {
                z <- ifelse(is.na(input$meandiff_n),
                            paste0(", conf.width = ", input$meandiff_ciwidth),
                            paste0(", n1 = ", input$meandiff_n))
                sd2 <- ifelse(input$meandiff_var,
                              paste0("sd2 = ", input$meandiff_sd2),
                              paste0("sd2 = ", input$meandiff_sd1))
                x <- paste0("prec_meandiff(delta = ", input$meandiff_delta,
                            ", sd1 = ", input$meandiff_sd1,
                            ", ",
                            sd2,
                            ", r = ", input$meandiff_r,
                            z, ", conf.level = ", input$conflevel,
                            ", variance = '",
                            ifelse(input$meandiff_var, "unequal", "equal"), "')")
                if(code){
                        cat(x)
                } else {
                        eval(parse(text = x))
                }
        }
}
