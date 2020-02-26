#  Risk diff
# UI ----
riskdiffpage <- tabItem(tabName = "riskdiff",
        h2("Precision of a risk difference"),
        "Enter the proportions of events you expect in the groups. If you intend to use uneven allocation ratios (e.g. 2 allocated to group 1 for each participant allocated to group 2), adjust the allocation ratio accordingly. To estimate the confidence interval width from a number of events, enter the number of events in 'Number of events'. To estimate the number of observations required to get a confidence interval width of X, enter the width in 'Confidence interval width'.",
        h4("Please enter the following"),
        sliderInput("riskdiff_p1", "Proportion of events in group 1",
                    value = NULL, min = 0, max = 1),
        sliderInput("riskdiff_p2", "Proportion of events in group 2",
                    value = NULL, min = 0, max = 1),
        numericInput("riskdiff_r", "Allocation ratio", value = 1),
        h4("Please enter one of the following"),
        numericInput("riskdiff_n1", "Number of observations in group 1", value = NULL),
        "(the number of observations in group 2 is estimated based on group 1 and the allocation ratio)",
        numericInput("riskdiff_ciwidth", "Confidence interval width",
                     value = NULL),
        h4("Other settings"),
        selectInput("riskdiff_method", label = "Method",
                    choices = c("Newcombe" = "newcombe",
                                "Miettinen-Nurminen" = "mn",
                                "Agresti-Caffo" = "ac",
                                "Wald" = "wald"),
                    selected = "newcombe"),
        h4("Results"),
        verbatimTextOutput("riskdiff_out"),
        "Code to replicate in R:",
        verbatimTextOutput("riskdiff_code"),
        h3("References"),
        "Agresti A (2003)", tags$b("Categorical Data Analysis"), ", Second Edition, Wiley Series in Probability and Statistics", tags$a(href="https://doi.org/10.1002/0471249688","DOI: 10.1002/0471249688"),
        tags$br(),
        "Agresti A and Caffo B (2000) Simple and Effective Confidence Intervals for Proportions and Differences of Proportions Result from Adding Two Successes and Two Failures, ",
        tags$i("The American Statistician"), "54(4):280-288",
        tags$br(),
        "Miettinen O and Nurminen M (1985) Comparative analysis of two rates, ", tags$i("Statistics in Medicine"), ", 4:213-226",
        tags$br(),
        "Newcombe RG (1998) Interval estimation for the difference between independent proportions: comparison of eleven methods, ", tags$i("Statistics in Medicine"), ", 17:873-890",
        tags$br(),
        "Fagerland MW, Lydersen S, and Laake P (2015). Recommended confidence intervals for two independent binomial proportions, ", tags$i("Statistical methods in medical research"), "24(2):224-254."
)

#SERVER
riskdiff_fn <- function(input, code = FALSE){
        if(is.na(input$riskdiff_n1) & is.na(input$riskdiff_ciwidth)) {
                cat("Awaiting 'number of observations' or 'confidence interval width'")
        } else {
                z <- ifelse(is.na(input$riskdiff_n1),
                            paste0(", conf.width = ", input$riskdiff_ciwidth),
                            paste0(", n1 = ", input$riskdiff_n1))
                x <- paste0("prec_riskdiff(p1 = ", input$riskdiff_p1,
                            ", p2 = ", input$riskdiff_p2,
                            ", r = ", input$riskdiff_r,
                            z, ", conf.level = ", input$conflevel,
                            ", method = '", input$riskdiff_method, "')")
                if(code){
                        cat(x)
                } else {
                        eval(parse(text = x))
                }
        }
}
