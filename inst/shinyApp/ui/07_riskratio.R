# risk ratio
# UI ----
riskrpage <- tabItem(tabName = "riskratio",
        h2("Precision of a risk ratio"),
        "Enter the proportions of events you expect in the groups. If you intend to use uneven allocation ratios (e.g. 2 allocated to group 1 for each participant allocated to group 2), adjust the allocation ratio accordingly. To estimate the confidence interval width from a number of events, enter the number of events in 'Number of events'. To estimate the number of observations required to get a confidence interval width of X, enter the width in 'Confidence interval width'.",
        tags$br(),
        h4("Please enter the following"),
        sliderInput("riskratio_p1", "Proportion of events in group 1",
                    value = NULL, min = 0, max = 1),
        sliderInput("riskratio_p2", "Proportion of events in group 2",
                    value = NULL, min = 0, max = 1),
        numericInput("riskratio_r", "Allocation ratio", value = 1),
        h4("Please enter one of the following"),
        numericInput("riskratio_n1", "Number of observations in group 1", value = NULL),
        "(the number of observations in group 2 is estimated based on group 1 and the allocation ratio)",
        numericInput("riskratio_ciwidth", "Confidence interval width",
                     value = NULL),
        h4("Other settings"),
        selectInput("riskratio_method", label = "Method",
                    choices = c("Koopman" = "koopman",
                                "Katz" = "katz"),
                    selected = "koopman"),
        h4("Results"),
        verbatimTextOutput("riskratio_out"),
        "Code to replicate in R:",
        verbatimTextOutput("riskratio_code"),
        h4("References"),
        "Fagerland MW, Lydersen S, and Laake P (2015). Recommended confidence intervals for two independent binomial proportions, ", tags$i("Statistical methods in medical research"), "24(2):224-254.",
        tags$br(),
        "Katz D, Baptista J, Azen SP, and Pike MC (1978) Obtaining Confidence Intervals for the Risk Ratio in Cohort Studies. ", tags$i("Biometrics "), "34:469-474",
        tags$br(),
        "Koopman PAR (1984) Confidence Intervals for the Ratio of Two Binomial Proportions,", tags$i("Biometrics"), " 40:513-517"

)

# SERVER ----
riskratio_fn <- function(input, code){
        if(is.na(input$riskratio_n1) & is.na(input$riskratio_ciwidth)) {
                cat("Awaiting 'number of observations' or 'confidence interval width'")
        } else {
                z <- ifelse(is.na(input$riskratio_n1),
                            paste0(", conf.width = ", input$riskratio_ciwidth),
                            paste0(", n1 = ", input$riskratio_n1))
                x <- paste0("prec_riskratio(p1 = ", input$riskratio_p1,
                            ", p2 = ", input$riskratio_p2,
                            ", r = ", input$riskratio_r,
                            z, ", conf.level = ", input$conflevel,
                            ", method = '", input$riskratio_method, "')")
                if(code){
                        cat(x)
                } else {
                        eval(parse(text = x))
                }
        }
}
