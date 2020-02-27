# auc
# UI ----
aucpage <- tabItem(tabName = "auc",
        h2("AUC (Area under the curve)"),
        "The AUC refers to the areas under the Receiver Operating Characteristic (ROC) curve - the grey area in the figure below. The higher the AUC, the better a predictive model performs.",
        plotOutput("auc_fig"),
        tags$br(),
        "Please enter the following",
        sliderInput("auc_auc", "AUC", min = 0, max = 1, value = NULL),
        sliderInput("auc_prev", "Prevalence", min = 0, max = 1, value = NULL),
        tags$br(),
        "Please enter one of the following",
        numericInput("auc_n", "Total sample size",
                     value = NULL),
        numericInput("auc_ciwidth", "Confidence interval width",
                     value = NULL, min = 0, max = 1),
        tags$hr(),
        verbatimTextOutput("auc_out"),
        tableOutput("auc_tab"),
        "Code to replicate in R:",
        verbatimTextOutput("auc_code"),
        h3("References"),
        "Hanley, JA and McNeil, BJ (1982) The Meaning and Use of the Area under a Receiver Operating Characteristic (ROC) Curve.",
        tags$i("Radiology"), "148, 29-36")

# SERVER ----
auc_fn <- function(input, code = FALSE){
        db(input, "auc")
        if(is.na(input$auc_n) & is.na(input$auc_ciwidth)) {
                cat("Awaiting 'number of observations' or 'confidence interval width'")
        } else {
                z <- ifelse(is.na(input$auc_n),
                            paste0("conf.width = ", input$auc_ciwidth),
                            paste0("n = ", input$auc_n))
                x <- paste0("prec_auc(auc = ", input$auc_auc,
                            ", prev = ", input$auc_prev,
                            ", ", z, ", conf.level = ", input$conflevel,
                            ")")
                if(code){
                        cat(x)
                } else {
                        eval(parse(text = x))
                }
        }
}

