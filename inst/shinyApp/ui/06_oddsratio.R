#  OR
# UI ----
orpage <- tabItem(tabName = "or",
        h2("Precision of an odds ratio"),
        "Enter the proportions of events you expect in the groups. If you intend to use uneven allocation ratios (e.g. 2 allocated to group 1 for each participant allocated to group 2), adjust the allocation ratio accordingly. To estimate the confidence interval width from a number of events, enter the number of events in 'Number of events'. To estimate the number of observations required to get a confidence interval width of X, enter the width in 'Confidence interval width'.",
        tags$br(),
        h4("Please enter the following"),
        sliderInput("or_p1", "Proportion of events in group 1*",
                    value = NULL, min = 0, max = 1),
        sliderInput("or_p2", "Proportion of events in group 2*",
                    value = NULL, min = 0, max = 1),
        numericInput("or_r", "Allocation ratio*", value = 1),
        h4("Please enter one of the following"),
        numericInput("or_n1", "Number of observations in group 1", value = NULL),
        "(the number of observations in group 2 is estimated based on group 1 and the allocation ratio)",
        numericInput("or_ciwidth", "Confidence interval width",
                     value = NULL),
        h4("Other settings"),
        selectInput("or_method", label = "Method",
                    choices = c("Gart" = "gart",
                                "Woolf" = "wolf",
                                "Independence-smoothed logit" = "indip_smooth"),
                    selected = "indip_smooth"),
        h4("Results"),
        verbatimTextOutput("or_out"),
        tableOutput("or_tab"),
        "Code to replicate in R:",
        verbatimTextOutput("or_code"),
        h4("References"),
        "Fagerland MW, Lydersen S, Laake P (2015). Recommended confidence intervals for two independent binomial proportions. ", tags$i("Statistical Methods in Medical Research"), ", 24(2):224-254.", tags$a(href="https://doi.org/10.1177/0962280211415469","doi:10.1177/0962280211415469")

)

# SERVER ----
or_fn <- function(input, code = FALSE){
        if(is.na(input$or_n1) & is.na(input$or_ciwidth)) {
                cat("Awaiting 'number of observations' or 'confidence interval width'")
        } else {
                z <- ifelse(is.na(input$or_n1),
                            paste0(", conf.width = ", input$or_ciwidth),
                            paste0(", n1 = ", input$or_n1))
                x <- paste0("prec_or(p1 = ", input$or_p1,
                            ", p2 = ", input$or_p2,
                            ", r = ", input$or_r,
                            z, ", conf.level = ", input$conflevel,
                            ", method = '", input$or_method, "')")
                if(code){
                        cat(x)
                } else {
                        eval(parse(text = x))
                }
        }
}

