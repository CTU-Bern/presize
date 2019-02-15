# functions for precision based sample size
#
# Correlation measures
# - intraclass correlation


# intraclass correlation ---------------
#' Sample size or precision for an intraclass correlation
#'
#' \code{prec_icc} returns the sample size or the precision for the given
#' intraclass correlation
#'
#' Exactly one of the parameters \code{n, conf.width} must be passed as NULL,
#' and that parameter is determined from the other.
#'
#' Sample size or precision is calculated according to formula 3 in Bonett
#' (2002), which is an approximation. Whether ICC is calculated for a one-way or
#' a two-way ANOVA does not matter in the approximation. As suggested by the
#' author, \eqn{5*rho} is added to n, if \eqn{k = 2} and \eqn{rho \ge 7}.
#'
#' n is rounded up to the next whole number using \code{ceiling}.
#'
#' @references Bonett DG (2002). \emph{Sample size requirements for estimating
#'   intraclass correlations with desired precision}. Statistics in Medicine,
#'   21:1331-1335. \href{https://doi.org/10.1002/sim.1108}{doi:10.1002/sim.1108}
#' @param rho Desired intraclass correlation.
#' @param k number of observations per n (subject).
#' @inheritParams prec_riskdiff
prec_icc <- function(rho, k, n = NULL, conf.width = NULL, conf.level = 0.95,
                     #aov = c(1, 2, 3),
                          tol = .Machine$double.eps^0.25) {
  is.wholenumber <-
    function(x, tol = .Machine$double.eps^0.5)  abs(x - round(x)) < tol
  if (!is.null(k) && !is.numeric(k) &&!is.wholenumber(k))
    stop("'k' must be numeric and a whole number")
  if (sum(sapply(list(n, conf.width), is.null)) != 1)
    stop("exactly one of 'n', and 'conf.width' must be NULL")
  numrange_check(rho)
  numrange_check(conf.level)

  alpha <- 1 - conf.level
  z <- qnorm(1 - alpha / 2)
  z2 <- z * z

  if (is.null(n)) {
    est <- "sample size"
  } else
    est <- "precision"

  if (is.null(n))
    n <- 8 * z2 * (1 - rho) ^ 2 * (1 + (k - 1) * rho) ^ 2 /
    (k * (k - 1) * conf.width ^ 2) + 1

  # add cases after calculation of n, if n is unknown, or before calculaiton of
  # conf.widht, if n is known
  n <- n + (k == 2 & rho >= 0.7) * 5 * rho

  if (is.null(conf.width))
    conf.width <- sqrt(8) * z * (1 - rho) * (1 + (k - 1) * rho) /
    sqrt(k * (k - 1) * (n - 1))

  n <- ceiling(n)

  structure(list(rho = rho,
                 k = k,
                 n = n,
                 conf.width = conf.width,
                 conf.level = conf.level,
                 #lwr = rho - prec,
                 #upr = rho + prec,
                 note = ifelse(any(k == 2 & rho >= 0.7), "5*rho is added to n if k == 2 and rho >= 0.7", NULL),
                 method = paste(est, "for intraclass correlation")),
            class = "presize")
}

