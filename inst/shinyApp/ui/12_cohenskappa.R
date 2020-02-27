# Cohens kappa ----
# UI ----
kappapage <- tabItem(tabName = "kappa",
        h2("Cohen's kappa"),
        "Kappa is used to assess the agreement between multiple raters, each classifying items into mutually exclusive categories. This function supports up to 6 raters and 5 categories.",
        tags$br(),
        h4("Please enter all of the following parameters:"),
        sliderInput("kappa", "Estimated kappa", min = 0, max = 1, value = 0.5),
        sliderInput("kappa_raters", "Number of raters", min = 2, max = 6, step = 1, value = 2),
        sliderInput("kappa_cats", "Number of categories", min = 2, max = 5, step = 1, value = 2),
        textInput("kappa_props", "Expected proportion of each category (must sum to 1, enter values separated by a comma)", value = ""),
        h4("Please enter one of the following:"),
        numericInput("kappa_n", "Sample size", min = 0, value = NULL),
        numericInput("kappa_ciwidth", "Confidence interval width", min = 0, value = NULL),
        h4("Result"),
        verbatimTextOutput("kappa_out"),
        tableOutput("kappa_tab"),
        "Code to replicate in R:",
        verbatimTextOutput("kappa_code"),
        h4("References"),
        "Donner & Rotondi (2010) Sample Size Requirements for Interval Estimation of the Kappa Statistic for Interobserver Agreement Studies with a Binary Outcome and Multiple Raters.", tags$i("International Journal of Biostatistics "), "6:31", tags$a(href = "https://doi.org/10.2202/1557-4679.1275", "doi: 10.2202/1557-4679.1275"),
        tags$br(),
        "Rotondi & Donner (2012) A Confidence Interval Approach to Sample Size Estimation for Interobserver Agreement Studies with Multiple Raters and Outcomes.", tags$i("Journal of Clinical Epidemiology"), "65:778-784", tags$a(href = "https://doi.org/10.1016/j.jclinepi.2011.10.019", "doi: 10.1016/j.jclinepi.2011.10.019")
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

