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
#' Newcombe is implemented as equation 10 in Newcombe (1998)
#'
#' \code{\link[stats]{uniroot}} is used to solve n for the newcombe method.
#'
#' The function uses \code{\link[base]{expand.grid}} to provide an estimate of n
#' or conf.width for every possible combination of supplied arguments.
#'
#' @param p1 Risk among unexposed
#' @param p2 Risk among exposed
#' @param n1 Number of patients in unexposed group
#' @param r allocation ratio (relative size of unexposed and exposed cohort
#'   (\code{n1} / \code{n2}))
#' @param method The method to use to calculate precision. Exactly one method
#'   may be provided. Methods can be abbreviated.
#' @inheritParams prec_mean
#'
#' @references Newcombe RG (1998) \emph{Interval estimation for the difference
#'   between independent proportions: comparison of eleven methods}, Statistics
#'   in Medicine, 17:873-890
#'
#' @examples
#' # Validate Newcombe (1998)
#' prec_riskdiff(p1 = 56/70, p2 = 48/80, n1 = 70, r = 70/80, met = "newcombe")  # Table IIa
#' prec_riskdiff(p1 = 10/10, p2 = 0/10, n1 = 10, met = "newcombe")  # Table IIh
prec_riskdiff <- function(p1, p2, n1 = NULL, conf.width = NULL,
                     r = 1, conf.level = 0.95,
                     method = c("newcombe"),
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

  methods <- c("newcombe")
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
                 note = "n is number in *each* group",
                 method = "Blablabla"),
            class = "presize")

}


