# Cohens kappa ----
# UI ----
kappapage <- tabItem(tabName = "kappa",
        h2("Cohen's kappa"),
        "Kappa is used to assess the agreement between for multiple raters, each classiying items into mutually exclusive catoregories. This function supports up to 6 raters and 5 categories.",
        tags$br(),
        "Please enter all of the following parameters:",
        sliderInput("kappa", "Kappa", min = 0, max = 1, value = 0.5),
        sliderInput("kappa_raters", "Number of raters", min = 2, max = 6, step = 1, value = 2),
        sliderInput("kappa_cats", "Number of categories", min = 2, max = 5, step = 1, value = 2),
        textInput("kappa_props", "Expected proportion of each category (must sum to 1, enter values separated by a comma)", value = ""),
        "Please enter one of the following:",
        numericInput("kappa_n", "Sample size", min = 0, value = NULL),
        numericInput("kappa_ciwidth", "Confidence interval width", min = 0, value = NULL),
        h3("Result"),
        verbatimTextOutput("kappa_out"),
        tableOutput("kappa_tab"),
        "Code to replicate in R:",
        verbatimTextOutput("kappa_code")
)

# SERVER ----
kappa_fn <- function(input, code = FALSE){
        db(input, "kappa")
        if(is.na(input$kappa_n) & is.na(input$kappa_ciwidth)) {
                cat("Awaiting 'number of observations' or 'confidence interval width'")
        } else {
                z <- ifelse(is.na(input$kappa_n),
                            paste0("conf.width = ", input$kappa_ciwidth),
                            paste0("n = ", input$kappa_n))
                props <- unlist(strsplit(input$kappa_props, ","))
                props <- as.numeric(trimws(props))
                props <- paste("c(", paste(props, collapse = ", "), ")", sep = "")
                x <- sprintf("prec_kappa(kappa = %g, %s, raters = %g, \n           n_category = %g, props = %s, conf.level = %s)", input$kappa, z, input$kappa_raters, input$kappa_cats, props, input$conflevel)
                if(code){
                        cat(x)
                } else {
                        eval(parse(text = x))
                }
        }
}


## data for baplot
# badat <- BlandAltmanLeh:::bland.altman.stats(rnorm(20), rnorm(20))
# dump("badat", "inst/extdata/baplotdata")

