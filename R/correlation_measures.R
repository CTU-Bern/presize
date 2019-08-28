# functions for precision based sample size
#
# Correlation measures
# - intraclass correlation
# - pearsons r


# intraclass correlation ---------------
#' Sample size or precision for an intraclass correlation
#'
#' \code{prec_icc} returns the sample size or the precision for the given
#' intraclass correlation
#'
#' Exactly one of the parameters \code{n, conf.width} must be passed as NULL,
#' and that parameter is determined from the others.
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
#' @param n number of subjects.
#' @examples
#' # Bonett (2002) gives an example using 4 raters, with an ICC of 0.85 and want
#' # a confidence width of 0.2. Bonett calculated that a sample size of 19.2 was
#' # required. This can be done via
#' prec_icc(0.85, 4, conf.width = 0.2)
#' # note that \code{presamp} rounds up to the nearist integer.
#'
#' # Bonett then goes on to estimate the width given the sample size, finding a
#' # value 'close to 0.2':
#' prec_icc(0.85, 4, 20)
#' @inheritParams prec_riskdiff
#' @export
prec_icc <- function(rho, k, n = NULL, conf.width = NULL, conf.level = 0.95) {
  is.wholenumber <-
    function(x, tol = .Machine$double.eps ^ 0.5)  abs(x - round(x)) < tol
  if (!is.null(k) && !is.numeric(k) && !is.wholenumber(k))
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
                 note = ifelse(any(k == 2 & rho >= 0.7), "5*rho is added to n if k == 2 and rho >= 0.7", NA),
                 method = paste(est, "for intraclass correlation")),
            class = "presize")
}



# Correlation coefficients ---------------
#' Sample size or precision for correlation coefficient
#'
#' \code{prec_cor} returns the sample size or the precision for the given
#' pearson, spearman, or kendall correlation coefficient
#'
#' Exactly one of the parameters \code{n, conf.width} must be passed as NULL,
#' and that parameter is determined from the other.
#'
#' Sample size or precision is calculated according to formula 2 in Bonett and
#' Wright (2000). The use of pearson is only recommended, if \eqn{n \ge 25}. The
#' pearson correlation coefficient assumes bivariate normality. If the
#' assumption of bivariate normality cannot be met, spearman or kendall should
#' be considered.
#'
#' n is rounded up to the next whole number using \code{ceiling}.
#'
#' \code{\link[stats]{uniroot}} is used to solve n.
#'
#' @references Bonett DG, and Wright TA (2000) \emph{Sample size requirements
#'   for estimating Pearson, Kendall and Spearman correlations} Psychometrika
#'   65:23-28. \href{https://doi.org/10.1007/BF02294183}{doi:10.1007/BF02294183}
#'
#' @param r Desired correlation coefficient.
#' @param n sample size
#' @param method Exactly one of \code{pearson} (\emph{default}), \code{kendall},
#'   or \code{spearman}. Methods can be abbreviated.
#' @inheritParams prec_riskdiff
#' @export
prec_cor <-  function(r, n = NULL, conf.width = NULL, conf.level = 0.95,
                      method = c("pearson", "kendall", "spearman"),
                      ...) {

  if (sum(sapply(list(n, conf.width), is.null)) != 1)
    stop("exactly one of 'n', and 'conf.width' must be NULL")
  numrange_check(r)
  numrange_check(conf.level)

  default_meth <- "pearson"
  if (length(method) > 1) {
    warning("more than one method was chosen, '", default_meth, "' will be used")
    method <- default_meth
  }

  methods <- c("pearson", "kendall", "spearman")
  id <- pmatch(method, methods)
  meth <- methods[id]
  if (is.na(id)) {
    warning("Method '", method, "' is not available, '", default_meth, "' will be used.")
    meth <- default_meth
  }


  alpha <- 1 - conf.level
  z <- qnorm(1 - alpha / 2)

  if (is.null(n)) {
    est <- "sample size"
  } else
    est <- "precision"


  if (meth == "pearson") {
    b <- 3
    c <- 1
  }
  if (meth == "kendall") {
    b <- 4
    c <- sqrt(0.437)
  }
  if (meth == "spearman") {
    b <- 3
    c <- sqrt(1 + r ^ 2 / 2)
  }

  calc_ci <- quote({
    Zz <- 0.5 * log( (1 + r) / (1 - r))
    A <- c * z / sqrt(n - b)
    ll <- Zz - A
    lu <- Zz + A
    ell <- exp(2 * ll)
    elu <- exp(2 * lu)
    lwr <- (ell - 1) / (ell + 1)
    upr <- (elu - 1) / (elu + 1)
    list(lwr = lwr,
         upr = upr,
         cw = upr - lwr)
  })

  if (is.null(conf.width)) {
    ci <- eval(calc_ci)
    conf.width <- ci$cw
  }
  if (is.null(n)) {
    f <- function(r, z, b, c, conf.width) uniroot(function(n) eval(calc_ci)$cw - conf.width,
                                            c(5, 1e+07), ...,
                                            extendInt = "yes")$root
    n <- mapply(f, r = r, z = z, b = b, c = c, conf.width = conf.width)
    n <- ceiling(n)
    eval(calc_ci)
  }

  structure(list(r = r,
                 n = n,
                 conf.width = conf.width,
                 conf.level = conf.level,
                 lwr = lwr,
                 upr = upr,
                 method = paste(est, "for", meth, "'s correlation coefficient")),
            class = "presize")
}





# Limit of agreement ---------------
#' Sample size or precision for limit of agreement on Bland Altman plots
#'
#' \code{prec_lim_agree} returns the sample size or the precision for the limit
#' of agreement
#'
#' Exactly one of the parameters \code{n, conf.width} must be passed as NULL,
#' and that parameter is determined from the other.
#'
#' Sample size or precision is calculated according to formulae in Bland & Altman (1986).
#'
#' @param n Sample size
#' @param conf.width precision (the full width of the conficende interval)
#' @param conf.level confidence level
#' @references Bland & Altman (1986) \emph{Statistical methods for assessing agreement
#' between two methods of clinical measurement} Lancet i(8476):307-310 \href{https://doi.org/10.1016/S0140-6736(86)90837-8}{doi:10.1016/S0140-6736(86)90837-8}
#' @export

prec_lim_agree <- function(n = NULL, conf.width = NULL, conf.level = 0.95){

  if (sum(sapply(list(n, conf.width), is.null)) != 1)
    stop("exactly one of 'n', and 'conf.width' must be NULL")

  alpha <- 1 - conf.level
  z <- qnorm(1 - alpha / 2)

  if (is.null(n)) {
    est <- "sample size"
  } else
    est <- "precision"

  if (is.null(conf.width)) {
    cwidth <- z * sqrt(3 / n)
  }

  if (!is.null(conf.width)) {
    cwidth <- conf.width
    n <- 3 / (conf.width / z) ^ 2
  }

  structure(list(n = n,
                 conf.width = cwidth,
                 conf.level = conf.level,
                 method = paste(est, "for limit of agreement")),
            class = "presize")

}
