#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

require(shiny)
require(shinydashboard)
require(presize)

icon <- icon("calculator")

lf <- list.files("ui", full.names = TRUE)
sapply(lf, source)

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
                          menuItem("Limit of agreement", tabName = "limit", icon = icon),
                          menuItem("Cohen's kappa", tabName = "kappa", icon = icon)

                 ),
                 menuItem("Diagnostic measures",
                          menuItem("Sensitivity", tabName = "sens", icon = icon),
                          menuItem("Specificity", tabName = "spec", icon = icon),
                          menuItem("AUC", tabName = "auc", icon = icon),
                          menuItem("Positive likelihood ratio", tabName = "lrpos", icon = icon),
                          menuItem("Negative likelihood ratio", tabName = "lrneg", icon = icon)
                          ),
                 width = 12),
             sidebarPanel(
                 tags$style(HTML(".well {
                                 background-color: #222d32;
                                 border-color: #222d32;
                                 }")),
                 br(),
                 br(),
                 br(),
                 "presize was developed by members of the Swiss Clinical Trials Organisation",
                 br(),
                 img(src = "scto_ctu_member_cmyk.jpg"),
                 width = 12
             )
         ),
         dashboardBody(
             tabItems(helppage,
                      meanpage,
                      proportionpage,
                      ratepage,
                      meandiffpage,
                      riskdiffpage,
                      orpage,
                      riskrpage,
                      raterpage,
                      corpage,
                      iccpage,
                      limitpage,
                      kappapage,
                      senspage,
                      specpage,
                      aucpage,
                      lrppage,
                      lrnpage
             ) # close tabItems

        ) # close body

) # close UI


# Define server logic required to draw a histogram
server <- function(input, output, session) {



    output$version <- renderText(installed.packages()["presize","Version"])
    output$ex_n <- renderText(sprintf("%1.0f", prec_mean(20, sd = 3, conf.width = 5)$n))
    output$ex_ciw <- renderText(sprintf("%1.0f", prec_mean(20, sd = 3, n = 50)$conf.width))

    # mean ----
    output$mean_out <- renderPrint(mean_fn(input))
    output$mean_code <- renderPrint(mean_fn(input, TRUE))
    output$mean_tab <- renderTable({
        tmp <- mean_fn(input, FALSE)
        tmp1 <- res_vars[res_vars$column %in%
                     c("mu", "sd", "n", "lwr", "upr", "conf.width", "conf.level"),]
        tmp1[na.omit(match(names(tmp), tmp1$column)),]
    })

    # proportion ----
    output$prop_code <- renderPrint(prop_fn(input, TRUE))
    output$prop_out <- renderPrint(prop_fn(input))
    output$prop_tab <- renderTable({
        tmp <- prop_fn(input)
        tmp1 <- res_vars[res_vars$column %in%
                     c("p", "padj", "n", "lwr", "upr", "conf.width", "conf.level"),]
        tmp1[na.omit(match(names(tmp), tmp1$column)),]
    })

    # rate ----
    output$rate_out <- renderPrint(rate_fn(input))
    output$rate_code <- renderPrint(rate_fn(input, TRUE))
    output$rate_tab <- renderTable({
        tmp <- rate_fn(input, FALSE)
        tmp1 <- res_vars[res_vars$column %in%
                     c("r", "radj", "x", "time", "conf.width", "conf.level",
                       "lwr", "upr"),]
        tmp1[na.omit(match(names(tmp), tmp1$column)),]
    })

    # meandiff ----
    output$meandiff_out <- renderPrint(meandiff_fn(input))
    output$meandiff_code <- renderPrint(meandiff_fn(input, TRUE))
    output$meandiff_tab <- renderTable({
        tmp <- meandiff_fn(input)
        tmp1 <- res_vars[res_vars$column %in%
                             c("delta", "sd1", "sd2", "n1", "n2", "conf.width",
                               "conf.level", "lwr", "upr", "ar"),]
        tmp1$column[tmp1$column == "ar"] <- "r"
        tmp1[na.omit(match(names(tmp), tmp1$column)),]
    })

    # riskdiff ----
    output$riskdiff_out <- renderPrint(riskdiff_fn(input))
    output$riskdiff_code <- renderPrint(riskdiff_fn(input, TRUE))
    output$riskdiff_tab <- renderTable({
        tmp <- riskdiff_fn(input)
        tmp1 <- res_vars[res_vars$column %in%
                             c("p1", "p2", "n1", "n2", "ar", "ntot", "delta",
                               "conf.width","conf.level", "lwr", "upr"),]
        tmp1$column[tmp1$column == "ar"] <- "r"
        tmp1[na.omit(match(names(tmp), tmp1$column)),]
    })

    # or ----
    output$or_out <- renderPrint(or_fn(input))
    output$or_code <- renderPrint(or_fn(input, TRUE))
    output$or_tab <- renderTable({
        tmp <- or_fn(input)
        tmp1 <- res_vars[res_vars$column %in% c(names(tmp), "ar"),]
        tmp1 <- tmp1[-which(tmp1$column == "r"), ]
        tmp1$column[tmp1$column == "ar"] <- "r"
        tmp1[na.omit(match(names(tmp), tmp1$column)),]
    })

    # risk ratio ----
    output$riskratio_out <- renderPrint(riskratio_fn(input))
    output$riskratio_code <- renderPrint(riskratio_fn(input, TRUE))
    output$riskratio_tab <- renderTable({
        tmp <- riskratio_fn(input)
        tmp1 <- res_vars[res_vars$column %in% c(names(tmp), "ar"),]
        tmp1 <- tmp1[-which(tmp1$column == "r"), ]
        tmp1$column[tmp1$column == "ar"] <- "r"
        tmp1[na.omit(match(names(tmp), tmp1$column)),]
    })

    # rate ratio ----
    output$rateratio_out <- renderPrint(rateratio_fn(input))
    output$rateratio_code <- renderPrint(rateratio_fn(input, TRUE))
    output$rateratio_tab <- renderTable({
        tmp <- rateratio_fn(input)
        tmp1 <- res_vars[res_vars$column %in% c(names(tmp)),]
        tmp1 <- tmp1[-which(tmp1$column == "r"), ]
        tmp1$column[tmp1$column == "ar"] <- "r"
        tmp1[na.omit(match(names(tmp), tmp1$column)),]
    })

    # correlation coefficient ----
    output$cor_out <- renderPrint(cor_fn(input))
    output$cor_code <- renderPrint(cor_fn(input, TRUE))
    output$cor_tab <- renderTable({
        tmp <- cor_fn(input)
        tmp1 <- res_vars[res_vars$column %in% c(names(tmp), "cr"),]
        tmp1 <- tmp1[-which(tmp1$column == "r"), ]
        tmp1$column[tmp1$column == "cr"] <- "r"
        tmp1[na.omit(match(names(tmp), tmp1$column)),]
    })

    # ICC ----
    output$icc_out <- renderPrint(icc_fn(input))
    output$icc_code <- renderPrint(icc_fn(input, TRUE))
    output$icc_tab <- renderTable({
        tmp <- icc_fn(input)
        tmp1 <- res_vars[res_vars$column %in% names(tmp),]
        tmp1[na.omit(match(names(tmp), tmp1$column)),]
    })

    # limit of agreement ----
    output$limit_out <- renderPrint(limit_fn(input))
    output$limit_code <- renderPrint(limit_fn(input, TRUE))
    output$limitplot <- renderPlot({
        source(system.file("extdata", "baplotdata", package = "presize"))
        po <- as.data.frame(badat[1:2])
        seg <- data.frame(x = c(0,0),
                          yu = badat$CI.lines[1:2],
                          yl = badat$CI.lines[5:6])
        ggplot2::ggplot(po, aes(x = means, y = diffs)) +
            ggplot2::geom_point() +
            ggplot2::geom_hline(yintercept = badat$lines[2], col = "blue") +
            ggplot2::geom_hline(yintercept = badat$lines[c(1,3)], linetype = "dashed", col = "red") +
            ggplot2::geom_hline(yintercept = badat$CI.lines[c(1,2,5,6)], linetype = "twodash") +
            ggplot2::geom_segment(aes(x = 0, xend = 0,
                             y = badat$CI.lines[1], yend = badat$CI.lines[2]),
                         arrow = arrow(ends = "both"), size = 1.5) +
            ggplot2::geom_segment(aes(x = -0.5, xend = -0.5,
                             y = badat$CI.lines[5], yend = badat$CI.lines[6]),
                         arrow = arrow(ends = "both"), size = 1.5) +
            ggplot2::geom_segment(aes(x = 0.5, xend = 0.5,
                             y = badat$lines[1], yend = badat$lines[3]),
                         arrow = arrow(ends = "both"),
                         col = "darkgrey") +
            ggplot2::theme_classic() +
            ggplot2::xlab("Mean") + ggplot2::ylab("Difference")
    })
    output$limit_tab <- renderTable({
        tmp <- limit_fn(input)
        tmp1 <- res_vars[res_vars$column %in% names(tmp),]
        tmp1[na.omit(match(names(tmp), tmp1$column)),]
    })

    # kappa ----
    output$kappa_out <- renderPrint(kappa_fn(input))
    output$kappa_code <- renderPrint(kappa_fn(input, TRUE))
    output$kappa_tab <- renderTable({
        tmp <- kappa_fn(input)
        tmp1 <- res_vars[res_vars$column %in% names(tmp),]
        tmp1[na.omit(match(names(tmp), tmp1$column)),]
    })

    # sens ----
    output$sens_out <- renderPrint(sens_fn(input))
    output$sens_code <- renderPrint(sens_fn(input, TRUE))
    output$sens_tab <- renderTable({
        tmp <- sens_fn(input)
        tmp1 <- res_vars[res_vars$column %in% c(names(tmp), "nsens"),]
        tmp1 <- tmp1[-which(tmp1$column == "n"), ]
        tmp1$column[tmp1$column == "nsens"] <- "n"
        tmp1[na.omit(match(names(tmp), tmp1$column)),]
    })

    # spec ----
    output$spec_out <- renderPrint(spec_fn(input))
    output$spec_code <- renderPrint(spec_fn(input, TRUE))
    output$spec_tab <- renderTable({
        tmp <- spec_fn(input)
        tmp1 <- res_vars[res_vars$column %in% c(names(tmp), "nspec"),]
        tmp1 <- tmp1[-which(tmp1$column == "n"), ]
        tmp1$column[tmp1$column == "nspec"] <- "n"
        tmp1[na.omit(match(names(tmp), tmp1$column)),]
    })

    # auc ----
    output$auc_out <- renderPrint(auc_fn(input))
    output$auc_code <- renderPrint(auc_fn(input, TRUE))
    output$auc_tab <- renderTable({
        tmp <- auc_fn(input)
        tmp1 <- res_vars[res_vars$column %in% c(names(tmp), "auc_n1", "auc_n2"),]
        tmp1 <- tmp1[-which(tmp1$column %in% c("n1", "n2")), ]
        tmp1$column[tmp1$column == "nspec"] <- "n"
        tmp1[na.omit(match(names(tmp), tmp1$column)),]
    })
    output$auc_fig <- renderPlot({
        x <- seq(0,1,.1)
        y <- c(0, .6, .77, .85, .9, .92, .93, .94, .96, .99, 1)
        dat <- data.frame(x = x, y = y)
        ggplot2::ggplot(dat, ggplot2::aes(x = x, y = y)) +
            ggplot2::geom_area(fill = "lightgrey") +
            ggplot2::geom_line(col = "red", size = 1) +
            ggplot2::geom_line(data = data.frame(x = c(0,1), y = c(0, 1)),
                      mapping = ggplot2::aes(x = x, y = y), col = "darkgrey",
                      linetype = 2) +
            ggplot2::theme_classic() +
            ggplot2::xlab("1-Specificity") +
            ggplot2::ylab("Sensitivity") +
            ggplot2::annotate("text", x = .6, y = .25, label = "AUC") +
            ggplot2::annotate("text", x = .2, y = .85, label = "ROC")
    })

    # likelihood ratio ----
    output$lrp_out <- renderPrint(lrp_fn(input))
    output$lrp_code <- renderPrint(lrp_fn(input, TRUE))
    output$lrp_tab <- renderTable({
        tmp <- lrp_fn(input)
        tmp1 <- res_vars[res_vars$column %in% c(names(tmp), "lr_n1", "lr_n2", "lr_p1", "lr_p2"),]
        tmp1 <- tmp1[-which(tmp1$column %in% c("n1", "n2", "p1", "p2")), ]
        tmp1$column[tmp1$column == "lr_n1"] <- "n1"
        tmp1$column[tmp1$column == "lr_n2"] <- "n2"
        tmp1$column[tmp1$column == "lr_p1"] <- "p1"
        tmp1$column[tmp1$column == "lr_p2"] <- "p2"
        tmp1[na.omit(match(names(tmp), tmp1$column)),]
    })

    output$lrn_out <- renderPrint(lrn_fn(input))
    output$lrn_code <- renderPrint(lrn_fn(input, TRUE))
    output$lrn_tab <- renderTable({
        tmp <- lrn_fn(input)
        tmp1 <- res_vars[res_vars$column %in% c(names(tmp), "lr_n1", "lr_n2", "lr_p1", "lr_p2"),]
        tmp1 <- tmp1[-which(tmp1$column %in% c("n1", "n2", "p1", "p2")), ]
        tmp1$column[tmp1$column == "lr_n1"] <- "n1"
        tmp1$column[tmp1$column == "lr_n2"] <- "n2"
        tmp1$column[tmp1$column == "lr_p1"] <- "p1"
        tmp1$column[tmp1$column == "lr_p2"] <- "p2"
        tmp1[na.omit(match(names(tmp), tmp1$column)),]
    })
}

# Run the application ----
shinyApp(ui = ui, server = server)

