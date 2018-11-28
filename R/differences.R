# functions for precision based sample size
#
# Absolute and relative differences
# - risk difference







# risk difference --------------------
#' Sample size or precision for risk difference
#'
#' \code{prec_riskdiff} returns the risk difference and the sample size or the
#' precision for the provided proportions
#'
#' Exactly one of the parameters \code{n1, conf.width} must be passed as NULL,
#' and that parameter is determined from the other.
#'
#' Newcombe (\code{newcombe}) proposed a confidence interval based on the wilson
#' score method for the single proportion (see \link{prec_prop}). The confidence
#' interval without continuity correction is implemented from equation 10 in
#' Newcombe (1998)
#'
#' Agresti-Caffo (\code{ac}) confidence interval is based on the Wald confidence
#' interval, adding 1 success to each cell of the 2 x 2 table (see Agresti and
#' Caffo 2000).
#'
#' \code{\link[stats]{uniroot}} is used to solve n for the newcombe and ac
#' method.
#'
#' The function uses \code{\link[base]{expand.grid}} to provide an estimate of n
#' or conf.width for every possible combination of supplied arguments.
#'
#' @param p1 Risk among unexposed
#' @param p2 Risk among exposed
#' @param n1 Number of patients in unexposed group
#' @param r allocation ratio (relative size of unexposed and exposed cohort
#'   (\code{n1} / \code{n2}))
#' @param method Exactly one of \code{newcombe} (\emph{default}), \code{ac}
#'   (Agresti-Caffo), \code{wald}. Methods can be abbreviated.
#' @inheritParams prec_mean
#'
#' @references
#' Agresti A and Caffo B (2000) \emph{Simple and Effective Confidence Intervals
#' for Proportions and Differences of Proportions Result from Adding Two
#' Successes and Two Failures}, The American Statistician, 54(4):280-288
#'
#' Newcombe RG (1998) \emph{Interval estimation for the difference between
#' independent proportions: comparison of eleven methods}, Statistics in
#' Medicine, 17:873-890
#'
#' @examples
#' # Validate Newcombe (1998)
#' prec_riskdiff(p1 = 56/70, p2 = 48/80, n1 = 70, r = 70/80, met = "newcombe")  # Table IIa
#' prec_riskdiff(p1 = 10/10, p2 = 0/10, n1 = 10, met = "newcombe")  # Table IIh
prec_riskdiff <- function(p1, p2, n1 = NULL, conf.width = NULL,
                     r = 1, conf.level = 0.95,
                     method = c("newcombe", "ac", "wald"),
                     tol = .Machine$double.eps^0.25) {
  if (sum(sapply(list(n1, conf.width), is.null)) != 1)
    stop("exactly one of 'n1', and 'conf.width' must be NULL")
  if (!is.null(p1) && !is.numeric(p1) || any(0 > p1 | p1 > 1))
    stop("'p1' must be numeric in [0, 1]")
  if (!is.null(p2) && !is.numeric(p2) || any(0 > p2 | p2 > 1))
    stop("'p2' must be numeric in [0, 1]")

  # first, capture the arguments
  argg <- as.list(environment())

  if (length(method) > 1) {
    warning("more than one method was chosen, 'newcombe' will be used")
    method <- "newcombe"
  }

  methods <- c("newcombe", "ac", "wald")
  id <- pmatch(method, methods)
  meth <- methods[id]
  if (is.na(id)) {
    warning("Method '", method, "' is not available, 'newcombe' will be used.")
    meth <- "newcombe"
  }

  # expand the arguments
  d <- expand_args(argg)
  p1 <- d$p1
  p2 <- d$p2
  delta <- p1 - p2
  r <- d$r
  if (is.null(n1)) {
    conf.width <- d$conf.width
    prec <- conf.width / 2
  }
  if (is.null(conf.width)) {
    n1 <- d$n1
    n2 <- n1 / r
  }
  conf.level <- d$conf.level

  alpha <- (1 - conf.level)
  z <- qnorm(1 - alpha / 2)
  z2 <- z * z

  if(meth == "newcombe") {
    nc <- quote({
      n2 <- n1 / r
      ci1 <- prec_prop(p1, n1, conf.level = conf.level, method = "wilson")
      ci2 <- prec_prop(p2, n2, conf.level = conf.level, method = "wilson")
      l1 <- ci1$lwr
      l2 <- ci2$lwr
      u1 <- ci1$upr
      u2 <- ci2$upr
      lwr <- delta - sqrt((p1 - l1) ^ 2 + (u2 - p2) ^ 2)
      upr <- delta + sqrt((u1 - p1) ^ 2 + (p2 - l2) ^ 2)
      cw <- upr - lwr
      list(lwr = lwr,
           upr = upr,
           cw = cw,
           n2 = n2)
    })
    if (is.null(conf.width)) {
      ci <- eval(nc)
      conf.width <- ci$cw
    }
    if (is.null(n1)) {
      f <- function(p1, p2, conf.level, conf.width) uniroot(function(n1) eval(nc)$cw - conf.width,
                                                            c(1, 1e+07), tol = tol)$root
      n1 <- mapply(f, p1 = p1, p2 = p2, conf.level = conf.level, conf.width = conf.width)
      ci <- eval(nc)
      n2 <- ci$n2
    }
    lwr <- ci$lwr
    upr <- ci$upr
  }

  if (meth == "wald") {
    if (is.null(conf.width)) {
      prec <- z * sqrt(p1 * (1 - p1) / n1 + p2 * (1 - p2) / n2)
      conf.width <- 2 * prec
    }
    if (is.null(n1)) {
      n1 <- z2 * (p1 * (1 - p1) + p2 * (1 - p2) * r) / (prec ^ 2)
      n2 <- n1 / r
    }
      lwr <- delta - prec
      upr <- delta + prec
  }

  if (meth == "ac") {
    ac <- quote({
      n2 <- n1 / r
      x1 <- p1 * n1
      x2 <- p2 * n2
      .x1 <- x1 + 1
      .x2 <- x2 + 1
      .n1 <- n1 + 2
      .n2 <- n2 + 2
      .p1 <- .x1 / .n1
      .p2 <- .x2 / .n2
      z * sqrt(.p1 * (1 - .p1) / .n1 + .p2 * (1 - .p2) / .n2)
    })
    if (is.null(conf.width)) {
      prec <- eval(ac)
      conf.width <- prec * 2
    }
    if (is.null(n1)) {
      f <- function(p1, p2, r, prec) uniroot(function(n1) eval(ac) - prec,
                              c(1, 1e+07), tol = tol)$root
      n1 <- mapply(f, p1 = p1, p2 = p2, r = r, prec = prec)
      n2 <- n1 / r
    }
    lwr <- delta - prec
    upr <- delta + prec
  }

  structure(list(p1 = p1,
                 p2 = p2,
                 n1 = n1,
                 n2 = n2,
                 ntot = n1 + n2,
                 r = r,
                 delta = delta,
                 lwr = lwr,
                 upr = upr,
                 conf.width = conf.width,
                 conf.level = conf.level,
                 #note = "n is number in *each* group",
                 method = meth),
            class = "presize")
}


