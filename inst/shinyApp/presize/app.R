#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(presize)

icon <- icon("calculator")

# Define UI for application that draws a histogram
ui <- dashboardPage(skin = "red",
         dashboardHeader(title = "presize - precision based sample size calculation",
                         titleWidth = 500),
         dashboardSidebar(width = 300,
             sidebarMenu(menuItem("Help/Instructions", tabName = "help")),
             sliderInput("conflevel", "Confidence level",
                          value = 0.95, min = 0, max = 1),
             sidebarMenu(
                 menuItem("Descriptive statistics",
                    menuItem("Mean", tabName = "mean", icon = icon),
                    menuItem("Proportion", tabName = "proportion", icon = icon),
                    menuItem("Rate", tabName = "rate", icon = icon)
                 ),
                 menuItem("Absolute difference",
                    menuItem("Mean difference", tabName = "meandiff", icon = icon),
                    menuItem("Risk difference", tabName = "riskdiff", icon = icon)
                 ),
                 menuItem("Relative difference",
                    menuItem("Odds ratio", tabName = "or", icon = icon),
                    menuItem("Risk ratio", tabName = "riskratio", icon = icon),
                    menuItem("Rate ratio", tabName = "rateratio", icon = icon)
                 ),
                 menuItem("Correlation measures",
                          menuItem("Correlation coefficient", tabName = "cor", icon = icon),
                          menuItem("Intraclass correlation coefficient (ICC)", tabName = "icc", icon = icon),
                          menuItem("Limit of agreement", tabName = "limit", icon = icon)

                 ),
                 menuItem("Diagnostic measures",
                          menuItem("Sensitivity", tabName = "sens", icon = icon),
                          menuItem("Specificity", tabName = "spec", icon = icon),
                          menuItem("AUC", tabName = "auc", icon = icon)
                          )
             )
         ),
         dashboardBody(
             tabItems(tabItem(tabName = "help",
                              h1(tags$code("presize"), " : precision based sample size calculation"),
                              "It is sometimes desirable to power a study on the precision of an estimate rather than for a particular hypothesis test.",
                              tags$code("presize"),
                              "provides a range of functions for performing these calculations. ",
                              tags$code("presize"),
                              "returns either the confidence interval width that could be expected given a sample size, or the sample size that would be necessary to attain a given confidence interval width.",
                              tags$br(),
                              tags$br(),
                              "The different estimators are grouped according to their type (e.g. mean and proportion are under 'Descriptive statistics', while odds and risk ratios are under 'Relative differences'.",
                              tags$br(),
                              tags$br(),
                              "Each statistic has a set of fields. Mandatory fields are marked with an asterisk (*). There are also two fields that pertain to the sample size and confidence interval width, indicated by a dagger (†). Only one of these should be entered."),


# mean -----
                      tabItem(tabName = "mean",
                              h2("Precision of a mean"),
                              "Enter the mean and standard deviation you expect. To estimate the confidence interval width from a population of size X, enter the population size in 'Number of observations'. To estimate the number of observations required to get a confidence interval width of X, enter the width in 'Confidence interval width'.",
                              tags$br(),
                              "* Must be provided. ",
                              tags$br(),
                              "† Only one should be provided.",

                              numericInput("mean_mean", "Mean*",
                                           value = 10),
                              numericInput("mean_sd", "Standard deviation*",
                                           value = 2),
                              numericInput("mean_n", "Number of observations†",
                                           value = NULL),
                              numericInput("mean_ciwidth", "Confidence interval width†",
                                           value = NULL),
                              tags$hr(),
                              verbatimTextOutput("mean_out"),
                              "Code to replicate in R:",
                              verbatimTextOutput("mean_code")

                      ),
# proportion ----
                      tabItem(tabName = "proportion",
                              h2("Precision of a proportion"),
                              "Enter the proportion you expect. To estimate the confidence interval width from a population of size X, enter the population size in 'Number of observations'. To estimate the number of observations required to get a confidence interval width of X, enter the width in 'Confidence interval width'.",
                              tags$br(),
                              "* Must be provided. ",
                              tags$br(),
                              "† Only one should be provided.",

                              sliderInput("prop_p", "Proportion*",
                                           value = .5, min = 0, max = 1),
                              numericInput("prop_n", "Number of observations†",
                                           value = NULL),
                              numericInput("prop_ciwidth", "Confidence interval width†",
                                           value = NULL, min = 0, max = 1),
                              "The Wilson confidence interval is recommended, but others are available.",
                              selectInput("prop_method", label = "Confidence interval method",
                                          choices = c("Wilson" = "wilson",
                                                      "Agresti-Coull" = "agresti-coull",
                                                      "Exact" = "exact",
                                                      "Wald" = "wald"),
                                          selected = "wilson"),
                              tags$hr(),
                              verbatimTextOutput("prop_out"),
                              "Code to replicate in R:",
                              verbatimTextOutput("prop_code"),
                              h3("References"),
                              "Brown LD, Cai TT, DasGupta A (2001) Interval Estimation for a Binomial Proportion, ", tags$i("Statistical Science"), ", 16:2, 101-117, ", tags$a(href="doi:10.1214/ss/1009213286","doi:10.1214/ss/1009213286")
                      ),

                      tabItem(tabName = "rate",
                              h2("Precision of a rate"),
                              "Enter the rate you expect. To estimate the confidence interval width from a number of events, enter the number of events in 'Number of events'. To estimate the number of observations required to get a confidence interval width of X, enter the width in 'Confidence interval width'.",
                              tags$br(),
                              "* Must be provided. ",
                              tags$br(),
                              "† Only one should be provided.",

                              numericInput("rate_r", "Rate*",
                                           value = .5, min = 0, max = 1),
                              numericInput("rate_x", "Number of Events†",
                                           value = NULL),
                              numericInput("rate_ciwidth", "Confidence interval width†",
                                           value = NULL),
                              selectInput("rate_method", label = "Confidence interval method",
                                          choices = c("Score" = "score",
                                                      "Variance stabilizing" = "vs",
                                                      "Exact" = "exact",
                                                      "Wald" = "wald"),
                                          selected = "wilson"),
                              tags$hr(),
                              verbatimTextOutput("rate_out"),
                              "Code to replicate in R:",
                              verbatimTextOutput("rate_code"),
                              h3("References"),
                              "Barker, L. (2002) A Comparison of Nine Confidence Intervals for a Poisson Parameter When the Expected Number of Events is ≤ 5, ", tags$i("The American Statistician"), ", 56:2, 85-89, ", tags$a(href="https://doi.org/10.1198/000313002317572736","DOI: 10.1198/000313002317572736")
                      ),
                      # mean diff ----
                      tabItem(tabName = "meandiff",
                              h2("Precision of a mean difference"),
                              "Enter the mean difference and standard deviations you expect. If you intend to use uneven allocation ratios (e.g. 2 allocated to group 1 for each participant allocated to group 2), adjust the allocation ratio accordingly. To estimate the confidence interval width from a number of events, enter the number of events in 'Number of events'. To estimate the number of observations required to get a confidence interval width of X, enter the width in 'Confidence interval width'.",
                              tags$br(),
                              "* Must be provided. ",
                              tags$br(),
                              "† Only one should be provided.",

                              numericInput("meandiff_delta", "Mean difference*",
                                           value = 1),
                              numericInput("meandiff_sd1", "Standard deviation of group 1*",
                                           value = NULL),
                              checkboxInput("meandiff_var", label = "Group variances unequal"),
                              conditionalPanel("input.meandiff_var == true",
                                               numericInput("meandiff_sd2", "Standard deviation of group 2*",
                                           value = NULL)),
                              numericInput("meandiff_r", "Allocation ratio", value = 1),
                              numericInput("meandiff_n", "Number of observations in group 1†", value = NULL),
                              "(number of observations in group 2 is estimated based on group 1 and the allocation ratio)",
                              numericInput("meandiff_ciwidth", "Confidence interval width†",
                                           value = NULL),
                              tags$hr(),
                              verbatimTextOutput("meandiff_out"),
                              "Code to replicate in R:",
                              verbatimTextOutput("meandiff_code")
                      ),
                    #  Risk diff ----
                      tabItem(tabName = "riskdiff",
                              h2("Precision of a risk difference"),
                              "Enter the proportions of events you expect in the groups. If you intend to use uneven allocation ratios (e.g. 2 allocated to group 1 for each participant allocated to group 2), adjust the allocation ratio accordingly. To estimate the confidence interval width from a number of events, enter the number of events in 'Number of events'. To estimate the number of observations required to get a confidence interval width of X, enter the width in 'Confidence interval width'.",
                              tags$br(),
                              "* Must be provided. ",
                              tags$br(),
                              "† Only one should be provided.",

                              sliderInput("riskdiff_p1", "Proportion of events in group 1*",
                                           value = NULL, min = 0, max = 1),
                              sliderInput("riskdiff_p2", "Proportion of events in group 2*",
                                           value = NULL, min = 0, max = 1),
                              numericInput("riskdiff_r", "Allocation ratio*", value = 1),
                              numericInput("riskdiff_n1", "Number of observations in group 1†", value = NULL),
                              "(number of observations in group 2 is estimated based on group 1 and the allocation ratio)",
                              numericInput("riskdiff_ciwidth", "Confidence interval width†",
                                           value = NULL),
                              selectInput("riskdiff_method", label = "Method",
                                          choices = c("Newcombe" = "newcombe",
                                                      "Miettinen-Nurminen" = "mn",
                                                      "Agresti-Caffo" = "ac",
                                                      "Wald" = "wald"),
                                          selected = "newcombe"),
                              tags$hr(),
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
                              ),
                      #  OR ----
                      tabItem(tabName = "or",
                              h2("Precision of an odds ratio"),
                              "Enter the proportions of events you expect in the groups. If you intend to use uneven allocation ratios (e.g. 2 allocated to group 1 for each participant allocated to group 2), adjust the allocation ratio accordingly. To estimate the confidence interval width from a number of events, enter the number of events in 'Number of events'. To estimate the number of observations required to get a confidence interval width of X, enter the width in 'Confidence interval width'.",
                              tags$br(),
                              "* Must be provided. ",
                              tags$br(),
                              "† Only one should be provided.",

                              sliderInput("or_p1", "Proportion of events in group 1*",
                                          value = NULL, min = 0, max = 1),
                              sliderInput("or_p2", "Proportion of events in group 2*",
                                          value = NULL, min = 0, max = 1),
                              numericInput("or_r", "Allocation ratio*", value = 1),
                              numericInput("or_n1", "Number of observations in group 1†", value = NULL),
                              "(number of observations in group 2 is estimated based on group 1 and the allocation ratio)",
                              numericInput("or_ciwidth", "Confidence interval width†",
                                           value = NULL),
                              selectInput("or_method", label = "Method",
                                          choices = c("Gart" = "gart",
                                                      "Woolf" = "wolf",
                                                      "Independence-smoothed logit" = "indip_smooth"),
                                          selected = "indip_smooth"),
                              tags$hr(),
                              verbatimTextOutput("or_out"),
                              "Code to replicate in R:",
                              verbatimTextOutput("or_code"),
                              h3("References"),
                              "Fagerland MW, Lydersen S, Laake P (2015). Recommended confidence intervals for two independent binomial proportions. ", tags$i("Statistical Methods in Medical Research"), ", 24(2):224-254.", tags$a(href="https://doi.org/10.1177/0962280211415469","doi:10.1177/0962280211415469")

                      ),
                    # risk ratio ----
                      tabItem(tabName = "riskratio",
                              h2("Precision of an risk ratio"),
                              "Enter the proportions of events you expect in the groups. If you intend to use uneven allocation ratios (e.g. 2 allocated to group 1 for each participant allocated to group 2), adjust the allocation ratio accordingly. To estimate the confidence interval width from a number of events, enter the number of events in 'Number of events'. To estimate the number of observations required to get a confidence interval width of X, enter the width in 'Confidence interval width'.",
                              tags$br(),
                              "* Must be provided. ",
                              tags$br(),
                              "† Only one should be provided.",

                              sliderInput("riskratio_p1", "Proportion of events in group 1*",
                                          value = NULL, min = 0, max = 1),
                              sliderInput("riskratio_p2", "Proportion of events in group 2*",
                                          value = NULL, min = 0, max = 1),
                              numericInput("riskratio_r", "Allocation ratio*", value = 1),
                              numericInput("riskratio_n1", "Number of observations in group 1†", value = NULL),
                              "(number of observations in group 2 is estimated based on group 1 and the allocation ratio)",
                              numericInput("riskratio_ciwidth", "Confidence interval width†",
                                           value = NULL),
                              selectInput("riskratio_method", label = "Method",
                                          choices = c("Koopman" = "koopman",
                                                      "Katz" = "katz"),
                                          selected = "koopman"),
                              tags$hr(),
                              verbatimTextOutput("riskratio_out"),
                              "Code to replicate in R:",
                              verbatimTextOutput("riskratio_code"),
                              h3("References"),
                              "Fagerland MW, Lydersen S, and Laake P (2015). Recommended confidence intervals for two independent binomial proportions, ", tags$i("Statistical methods in medical research"), "24(2):224-254.",
                              tags$br(),
                              "Katz D, Baptista J, Azen SP, and Pike MC (1978) Obtaining Confidence Intervals for the Risk Ratio in Cohort Studies. ", tags$i("Biometrics "), "34:469-474",
                              tags$br(),
                              "Koopman PAR (1984) Confidence Intervals for the Ratio of Two Binomial Proportions,", tags$i("Biometrics"), " 40:513-517"

                      ),
                    # rate ratio ----
                      tabItem(tabName = "rateratio",
                              h2("Precision of an rate ratio"),
                              "Enter the proportions of events you expect in the groups. If you intend to use uneven allocation ratios (e.g. 2 allocated to group 1 for each participant allocated to group 2), adjust the allocation ratio accordingly. To estimate the confidence interval width from a number of events, enter the number of events in 'Number of events'. To estimate the number of observations required to get a confidence interval width of X, enter the width in 'Confidence interval width'.",
                              tags$br(),
                              "* Must be provided. ",
                              tags$br(),
                              "† Only one should be provided.",

                              numericInput("rateratio_rate_exp", "Event rate in the exposed group*",
                                          value = NULL),
                              numericInput("rateratio_rate_control", "Event rate in the control group*",
                                          value = NULL),
                              numericInput("rateratio_r", "Allocation ratio*", value = 1),
                              numericInput("rateratio_n_exp", "Number of observations in the exposed group†", value = NULL),
                              "(number of observations in group 2 is estimated based on the number of exposed individuals and the allocation ratio)",
                              numericInput("rateratio_ciwidth", "Confidence interval width†",
                                           value = NULL),
                              tags$hr(),
                              verbatimTextOutput("rateratio_out"),
                              "Code to replicate in R:",
                              verbatimTextOutput("rateratio_code"),
                              h3("References"),
                              "Rothamn KJ, Greenland S (2018) Planning Study Size Based on Precision Rather Than Power.", tags$i("Epidemiology"), "29:599-603", tags$a(href = "https://doi.org/10.1097/EDE.0000000000000876","doi:10.1097/EDE.0000000000000876")

                      ),

# correlation coefficient ----
                      tabItem(tabName = "cor",
                              h2("Precision of a correlation coefficient"),
                              "Enter the correlation coefficient you expect. To estimate the confidence interval width from a number of events, enter the number of events in 'Number of events'. To estimate the number of observations required to get a confidence interval width of X, enter the width in 'Confidence interval width'.",
                              tags$br(),
                              "* Must be provided. ",
                              tags$br(),
                              "† Only one should be provided.",

                              sliderInput("cor_r", "Correlation coefficient*",
                                           value = 0, min = -1, max = 1, step = .01),
                              selectInput("cor_method", label = "Type of correlation coefficient",
                                          choices = c("Pearson" = "pearson",
                                                      "Kendall" = "kendall",
                                                      "Spearman" = "spearman"),
                                          selected = "pearson"),
                              numericInput("cor_n", "Number of observations†", value = NULL),
                              numericInput("cor_ciwidth", "Confidence interval width†",
                                           value = NULL, min = 0, max = 1),
                              tags$hr(),
                              verbatimTextOutput("cor_out"),
                              "Code to replicate in R:",
                              verbatimTextOutput("cor_code"),
                              h3("References"),
                              "Bonett DG, and Wright TA (2000) Sample size requirements for estimating Pearson, Kendall and Spearman correlations. ",
                              tags$i("Psychometrika"), " 65:23-28", tags$a(href = "https://doi.org/10.1007/BF02294183","doi:10.1007/BF02294183")

                      ),
# ICC ----
                      tabItem(tabName = "icc",
                              h2("Precision of an intraclass correlation coefficient"),
                              "Enter the correlation coefficient you expect. To estimate the confidence interval width from a number of events, enter the number of events in 'Number of events'. To estimate the number of observations required to get a confidence interval width of X, enter the width in 'Confidence interval width'.",
                              tags$br(),
                              "* Must be provided. ",
                              tags$br(),
                              "† Only one should be provided.",

                              numericInput("icc_rho", "ρ (Intraclass correlation coefficient)*",
                                          value = 0, min = 0, max = 1),
                              numericInput("icc_k", "Number of observations per subject*", value = NULL),
                              numericInput("icc_n", "Number of subjects†", value = NULL),
                              numericInput("icc_ciwidth", "Confidence interval width†",
                                           value = NULL, min = 0, max = 1),
                              tags$hr(),
                              verbatimTextOutput("icc_out"),
                              "Code to replicate in R:",
                              verbatimTextOutput("icc_code"),
                              h3("References"),
                              "Bonett DG (2002). Sample size requirements for estimating intraclass correlations with desired precision.", tags$i("Statistics in Medicine"), "21:1331-1335. ", tags$a(href = "https://doi.org/10.1002/sim.1108", "doi: 10.1002/sim.1108")

                      ),
# limit of agreement
                      tabItem(tabName = "limit",
                              h2("Precision of limits of agreement"),
                              "Confidence interval width of the limit of agreement is only a function of sample size. Enter the sample size or confidence interval width to calculate the other.",
                              tags$br(),
                              "* Must be provided. ",
                              tags$br(),
                              "† Only one should be provided.",

                              numericInput("limit_n", "Sample size†",
                                           value = NULL),
                              numericInput("limit_ciwidth", "Confidence interval width†",
                                           value = NULL, min = 0, max = 1),
                              tags$hr(),
                              verbatimTextOutput("limit_out"),
                              "Code to replicate in R:",
                              verbatimTextOutput("limit_code"),
                              h3("References"),
                              "Bland & Altman (1986) Statistical methods for assessing agreement between two methods of clinical measurement.", tags$i("Lancet"), "i(8476):307-310", tags$a(href = "https://doi.org/10.1016/S0140-6736(86)90837-8", "doi: 10.1016/S0140-6736(86)90837-8")

                      ),

                      tabItem(tabName = "sens",
                              h2("Precision of sensitivity"),
                              "",
                              tags$br(),
                              "* Must be provided. ",
                              tags$br(),
                              "† Only one should be provided.",

                              sliderInput("sens_sens", "Sensitivity*", min = 0, max = 1, value = NULL),
                              sliderInput("sens_prev", "Prevalence*", min = 0, max = 1, value = NULL),
                              numericInput("sens_ntot", "Total sample size†",
                                                            value = NULL),
                              numericInput("sens_ciwidth", "Confidence interval width†",
                                           value = NULL, min = 0, max = 1),
                              selectInput("sens_method", label = "Confidence interval method",
                                          choices = c("Wilson" = "wilson",
                                                      "Agresti-Coull" = "agresti-coull",
                                                      "Exact" = "exact",
                                                      "Wald" = "wald"),
                                          selected = "wilson"),
                              selectInput("sens_round", label = "When rounding the number of cases, round...",
                                          choices = c("up" = "ceiling",
                                                      "down" = "floor"),
                                          selected = "up"),
                              "The number of cases is calculated as sample size * prev, which can result in fractions so rounding is necessary.",
                              tags$hr(),
                              verbatimTextOutput("sens_out"),
                              "Code to replicate in R:",
                              verbatimTextOutput("sens_code"),
                              h3("References"),
                              "Brown LD, Cai TT, DasGupta A (2001) Interval Estimation for a Binomial Proportion, ", tags$i("Statistical Science"), ", 16:2, 101-117, ", tags$a(href="doi:10.1214/ss/1009213286","doi:10.1214/ss/1009213286")

                      ),

                      tabItem(tabName = "spec",
                              h2("Precision of specificity"),
                              "",
                              tags$br(),
                              "* Must be provided. ",
                              tags$br(),
                              "† Only one should be provided.",

                              sliderInput("spec_spec", "Specificity*", min = 0, max = 1, value = NULL),
                              sliderInput("spec_prev", "Prevalence*", min = 0, max = 1, value = NULL),
                              numericInput("spec_ntot", "Total sample size†",
                                           value = NULL),
                              numericInput("spec_ciwidth", "Confidence interval width†",
                                           value = NULL, min = 0, max = 1),
                              selectInput("spec_method", label = "Confidence interval method",
                                          choices = c("Wilson" = "wilson",
                                                      "Agresti-Coull" = "agresti-coull",
                                                      "Exact" = "exact",
                                                      "Wald" = "wald"),
                                          selected = "wilson"),
                              selectInput("spec_round", label = "When rounding the number of cases, round...",
                                          choices = c("up" = "ceiling",
                                                      "down" = "floor"),
                                          selected = "up"),
                              "The number of cases is calculated as sample size * prev, which can result in fractions so rounding is necessary.",
                              tags$hr(),
                              verbatimTextOutput("spec_out"),
                              "Code to replicate in R:",
                              verbatimTextOutput("spec_code"),
                              h3("References"),
                              "Brown LD, Cai TT, DasGupta A (2001) Interval Estimation for a Binomial Proportion, ", tags$i("Statistical Science"), ", 16:2, 101-117, ", tags$a(href="doi:10.1214/ss/1009213286","doi:10.1214/ss/1009213286")

                      ),

                      tabItem(tabName = "auc",
                              h2("AUC (Area under the curve)"),
                              "",
                              tags$br(),
                              "* Must be provided. ",
                              tags$br(),
                              "† Only one should be provided.",

                              sliderInput("auc_auc", "AUC*", min = 0, max = 1, value = NULL),
                              sliderInput("auc_prev", "Prevalence*", min = 0, max = 1, value = NULL),
                              numericInput("auc_n", "Total sample size†",
                                           value = NULL),
                              numericInput("auc_ciwidth", "Confidence interval width†",
                                           value = NULL, min = 0, max = 1),
                              tags$hr(),
                              verbatimTextOutput("auc_out"),
                              "Code to replicate in R:",
                              verbatimTextOutput("auc_code"),
                              h3("References"),
                              "Hanley, JA and McNeil, BJ (1982) The Meaning and Use of the Area under a Receiver Operating Characteristic (ROC) Curve.", tags$i("Radiology"), "148, 29-36")


             ) # close tabItems

        )

)

# Define server logic required to draw a histogram
server <- function(input, output) {

    # to help with debugging
    db <- function(type){
        input2 <- reactiveValuesToList(input)
        input2 <- input2[grepl(type, names(input))]
        sapply(input2, cat, "\n", file = stderr())
    }

    # mean
    mean_fn <- function(input, code){
        if(is.na(input$mean_n) & is.na(input$mean_ciwidth)) {
            cat("Awaiting 'number of observations' or 'confidence interval width'")
        } else {
            z <- ifelse(is.na(input$mean_n),
                        paste0(", conf.width = ", input$mean_ciwidth),
                        paste0(", n = ", input$mean_n))
            x <- paste0("prec_mean(mu = ", input$mean_mean, ", ", input$mean_sd,
                       z, ", conf.level = ", input$conflevel, ")")
            if(code){
                cat(x)
            } else {
                eval(parse(text = x))
            }
        }
    }
    output$mean_out <- renderPrint(mean_fn(input, FALSE))
    output$mean_code <- renderPrint(mean_fn(input, TRUE))

    # proportion
    prop_fn <- function(input, code = FALSE) {
        if(is.na(input$prop_n) & is.na(input$prop_ciwidth)) {
            cat("Awaiting 'number of observations' or 'confidence interval width'")
        } else {
            z <- ifelse(is.na(input$prop_n),
                        paste0("conf.width = ", input$prop_ciwidth),
                        paste0("n = ", input$prop_n))
            x <- paste0("prec_prop(p = ", input$prop_p, ", ", z, ", conf.level = ",
                       input$conflevel, ", method = '", input$prop_method, "')")
            if(code){
                cat(x)
            } else {
                eval(parse(text = x))
            }
        }
    }
    output$prop_code <- renderPrint(prop_fn(input, TRUE))
    output$prop_out <- renderPrint(prop_fn(input, FALSE))


    # rate
    rate_fn <- function(input, code){
        if(is.na(input$rate_x) & is.na(input$rate_ciwidth)) {
            cat("Awaiting 'number of observations' or 'confidence interval width'")
        } else {
            z <- ifelse(is.na(input$rate_x),
                        paste0("conf.width = ", input$rate_ciwidth),
                        paste0("x = ", input$rate_x))
            x <- paste0("prec_rate(r = ", input$rate_r, ", ", z, ", conf.level = ",
                       input$conflevel, ", method = '", input$rate_method, "')")
            if(code){
                cat(x)
            } else {
                eval(parse(text = x))
            }
        }
    }
    output$rate_out <- renderPrint(rate_fn(input, FALSE))
    output$rate_code <- renderPrint(rate_fn(input, TRUE))

    # meandiff
    meandiff_fn <- function(input, code){
        db("meandiff")
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
    output$meandiff_out <- renderPrint(meandiff_fn(input, FALSE))
    output$meandiff_code <- renderPrint(meandiff_fn(input, TRUE))

    # riskdiff
    riskdiff_fn <- function(input, code){
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
    output$riskdiff_out <- renderPrint(riskdiff_fn(input, FALSE))
    output$riskdiff_code <- renderPrint(riskdiff_fn(input, TRUE))

    # or
    or_fn <- function(input, code){
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
    output$or_out <- renderPrint(or_fn(input, FALSE))
    output$or_code <- renderPrint(or_fn(input, TRUE))

    # risk ratio
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
    output$riskratio_out <- renderPrint(riskratio_fn(input, FALSE))
    output$riskratio_code <- renderPrint(riskratio_fn(input, TRUE))

    # rate ratio
    rateratio_fn <- function(input, code){
        if(is.na(input$rateratio_n_exp) & is.na(input$rateratio_ciwidth)) {
            cat("Awaiting 'number of observations' or 'confidence interval width'")
        } else {
            z <- ifelse(is.na(input$rateratio_n_exp),
                        paste0(", conf.width = ", input$rateratio_ciwidth),
                        paste0(", n_exp = ", input$rateratio_n_exp))
            x <- paste0("prec_rateratio(rate_exp = ", input$rateratio_rate_exp,
                        ", rate_control = ", input$rateratio_rate_control,
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
    output$rateratio_out <- renderPrint(rateratio_fn(input, FALSE))
    output$rateratio_code <- renderPrint(rateratio_fn(input, TRUE))

    # correlation coefficient
    cor_fn <- function(input, code){
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
    output$cor_out <- renderPrint(cor_fn(input, FALSE))
    output$cor_code <- renderPrint(cor_fn(input, TRUE))

    # ICC
    icc_fn <- function(input, code){
        if(is.na(input$icc_n) & is.na(input$icc_ciwidth)) {
            cat("Awaiting 'number of observations' or 'confidence interval width'")
        } else {
            z <- ifelse(is.na(input$icc_n),
                        paste0(", conf.width = ", input$icc_ciwidth),
                        paste0(", n = ", input$icc_n))
            x <- paste0("prec_icc(rho = ", input$icc_rho, ", k = ",
                        input$icc_k, ", ",
                        z, ", conf.level = ", input$conflevel,
                        ")")
            if(code){
                cat(x)
            } else {
                eval(parse(text = x))
            }
        }
    }
    output$icc_out <- renderPrint(icc_fn(input, FALSE))
    output$icc_code <- renderPrint(icc_fn(input, TRUE))

    # limit of agreement
    limit_fn <- function(input, code){
        if(is.na(input$limit_n) & is.na(input$limit_ciwidth)) {
            cat("Awaiting 'number of observations' or 'confidence interval width'")
        } else {
            z <- ifelse(is.na(input$limit_n),
                        paste0(", conf.width = ", input$limit_ciwidth),
                        paste0(", n = ", input$limit_n))
            x <- paste0("prec_lim_agree(", z, ", conf.level = ", input$conflevel,
                        ")")
            if(code){
                cat(x)
            } else {
                eval(parse(text = x))
            }
        }
    }
    output$limit_out <- renderPrint(limit_fn(input, FALSE))
    output$limit_code <- renderPrint(limit_fn(input, TRUE))

    # sens
    sens_fn <- function(input, code){
        if(is.na(input$sens_ntot) & is.na(input$sens_ciwidth)) {
            cat("Awaiting 'number of observations' or 'confidence interval width'")
        } else {
            z <- ifelse(is.na(input$sens_ntot),
                        paste0("conf.width = ", input$sens_ciwidth),
                        paste0("ntot = ", input$sens_ntot))
            x <- paste0("prec_sens(sens = ", input$sens_sens,
                        ", prev = ", input$sens_prev,
                        ", ", z, ", conf.level = ", input$conflevel,
                        ", round = '", input$sens_round,
                        "', method = '", input$sens_method, "')")
            if(code){
                cat(x)
            } else {
                eval(parse(text = x))
            }
        }
    }
    output$sens_out <- renderPrint(sens_fn(input, FALSE))
    output$sens_code <- renderPrint(sens_fn(input, TRUE))

    # spec
    spec_fn <- function(input, code){
        if(is.na(input$spec_ntot) & is.na(input$spec_ciwidth)) {
            cat("Awaiting 'number of observations' or 'confidence interval width'")
        } else {
            z <- ifelse(is.na(input$spec_ntot),
                        paste0("conf.width = ", input$spec_ciwidth),
                        paste0("ntot = ", input$spec_ntot))
            x <- paste0("prec_spec(spec = ", input$spec_spec,
                        ", prev = ", input$spec_prev,
                        ", ", z, ", conf.level = ", input$conflevel,
                        ", round = '", input$spec_round,
                        "', method = '", input$spec_method, "')")
            if(code){
                cat(x)
            } else {
                eval(parse(text = x))
            }
        }
    }
    output$spec_out <- renderPrint(spec_fn(input, FALSE))
    output$spec_code <- renderPrint(spec_fn(input, TRUE))

    # auc
    auc_fn <- function(input, code){
        db("auc")
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
    output$auc_out <- renderPrint(auc_fn(input, FALSE))
    output$auc_code <- renderPrint(auc_fn(input, TRUE))

}

# Run the application
shinyApp(ui = ui, server = server)

