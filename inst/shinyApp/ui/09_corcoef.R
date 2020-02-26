# correlation coefficient
# UI ----
corpage <- tabItem(tabName = "cor",
        h2("Precision of a correlation coefficient"),
        "Enter the correlation coefficient you expect. To estimate the confidence interval width from a number of events, enter the number of events in 'Number of events'. To estimate the number of observations required to get a confidence interval width of X, enter the width in 'Confidence interval width'.",
        h4("Please enter the following"),
        sliderInput("cor_r", "Correlation coefficient",
                    value = 0, min = -1, max = 1, step = .01),
        selectInput("cor_method", label = "Type of correlation coefficient",
                    choices = c("Pearson" = "pearson",
                                "Kendall" = "kendall",
                                "Spearman" = "spearman"),
                    selected = "pearson"),
        h4("Please enter one of the following"),
numericInput("cor_n", "Number of observations", value = NULL),
        numericInput("cor_ciwidth", "Confidence interval width",
                     value = NULL, min = 0, max = 1),
        h4("Results"),
        verbatimTextOutput("cor_out"),
        tableOutput("cor_tab"),
        "Code to replicate in R:",
        verbatimTextOutput("cor_code"),
        h4("References"),
        "Bonett DG, and Wright TA (2000) Sample size requirements for estimating Pearson, Kendall and Spearman correlations. ",
        tags$i("Psychometrika"), " 65:23-28", tags$a(href = "https://doi.org/10.1007/BF02294183","doi:10.1007/BF02294183")

)

# SERVER ----
cor_fn <- function(input, code = FALSE){
        if(is.na(input$cor_n) & is.na(input$cor_ciwidth)) {
                cat("Awaiting 'number of observations' or 'confidence interval width'")
        } else {
                z <- ifelse(is.na(input$cor_n),
                            paste0(", conf.width = ", input$cor_ciwidth),
                            paste0(", n = ", input$cor_n))
                x <- paste0("prec_cor(r = ", input$cor_r,
                            z, ", conf.level = ", input$conflevel,
                            ", method = '", input$cor_method,
                            "')")
                if(code){
                        cat(x)
                } else {
                        eval(parse(text = x))
                }
        }
}

