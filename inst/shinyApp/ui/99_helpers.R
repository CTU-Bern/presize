# helper functions


# to help with debugging ---
db <- function(input, type){
  input2 <- reactiveValuesToList(input)
  input2 <- input2[grepl(type, names(input))]
  sapply(input2, cat, "\n", file = stderr())
}

# table of variables listed in output

res_vars <- c(mu = "mean", "sd" = "standard deviation",
              n = "sample size", "conf.width" = "confidence interval width",
              conf.level = "confidence level (1 - type 1 error rate)",
              lwr = "lower confidence interval limit",
              upr = "upper confidence interval limit",
              p = "proportion of events",
              padj = "adjusted proportion (used in computations)",
              r = "event rate",
              radj = "adjusted rate (used in computations)",
              x = "number of events",
              time = "time required to observe 'x' events",
              delta = "difference",
              sd1 = "standard deviation in group 1",
              sd2 = "standard deviation in group 2",
              n1 = "sample size of group 1",
              n2 = "sample size of group 2",
              p1 = "proportion of 'n1' with events",
              p2 = "proportion of 'n2' with events",
              ntot = "total number of observations",
              ar = "allocation ratio",
              cr = "correlation coefficient",
              or = "odds ratio",
              rr = "risk or rate ratio",
              rate1 = "event rate in group 1",
              rate2 = "event rate in group 2",
              rho = "intraclass correlation coefficient",
              k = "number of observations per individual",
              kappa = "Cohen's kappa",
              sens = "sensitivity",
              sensadj = "adjusted sensitivity (used in computations)",
              prev = "prevalence",
              spec = "specificity",
              specadj = "adjusted specificity (used in computations)",
              auc = "AUC",
              auc_n1 = "number of cases",
              auc_n2 = "number of non-cases")
res_vars <- data.frame(column = names(res_vars), meaning = res_vars)
