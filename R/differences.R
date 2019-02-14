# functions for precision based sample size
#
# Absolute and relative differences
# - mean difference
# - risk difference
# - risk ratio
# - odds ratio




# Mean difference ---------------
#' Sample size or precision for a mean difference
#'
#' \code{prec_meandiff} returns the sample size or the precision for the
#' provided mean difference and standard deviations
#'
#' Exactly one of the parameters \code{n, conf.width} must be passed as NULL,
#' and that parameter is determined from the other.
#'
#'
#' @param delta difference in means between the two groups.
#' @param sd1 standard deviation in group 1.
#' @param sd2 standard deviation in group 2.
#' @param variance \code{equal} (\emph{default}) or \code{unequal} variance.
#' @inheritParams prec_riskdiff
#' @return Object of class "presize", a list of arguments (including the
#'   computed one) augmented with method and note elements.
#' @examples
#' prec_meandiff(delta = 5, sd1 = 2.5, n1 = 20, var = "equal")
#' prec_meandiff(delta = 5, sd1 = 2.5, conf.width = 3, var = "equal")
prec_meandiff <- function(delta, sd1, sd2 = sd1, n1 = NULL, r = 1, conf.width = NULL, conf.level = 0.95,
                          variance = c("equal", "unequal"),
                          tol = .Machine$double.eps^0.25) {
  if (!is.null(delta) && !is.numeric(delta))
    stop("'delta' must be numeric")
  if (!is.null(sd1) && !is.numeric(sd1))
    stop("'sd1' must be numeric")
  if (!is.null(sd2) && !is.numeric(sd2))
    stop("'sd2' must be numeric")
  if (!is.null(r) && !is.numeric(r))
    stop("'r' must be numeric")
  if (sum(sapply(list(n1, conf.width), is.null)) != 1)
    stop("exactly one of 'n', and 'conf.width' must be NULL")
  numrange_check(conf.level)
  numrange_check(r, 0, Inf)

  alpha <- 1 - conf.level
  if (is.null(n1)) {
    prec <- conf.width * 0.5
    est <- "sample size"
  } else est <- "precision"

  fac <- sd1 / sd2
  if (length(variance) > 1) {
    if (any(2/3 > fac | fac > 1.5)) {
      variance <- "unequal"
    } else {
      variance <- "equal"
    }
    message("more than one variance was chosen. Variance was changed to ", variance)
  }

  variances <- c("equal", "unequal")
  id <- pmatch(variance, variances)
  var <- variances[id]

  if (is.na(id)) {
    stop("variance is not correctly specified. Specify it as either 'equal' or 'unequal'.")
  }


  # assume equal standard deviation
  if (var == "equal") {
    if (any(2/3 > fac | fac > 1.5))
      message("equal variance was chosen, but at least one variance appears to be unequal.")

    md <- quote({
      n2 <- n1 * r
      s <- sqrt(((n1 - 1) * sd1 ^ 2 + (n2 - 1) * sd2 ^ 2) / (n1 + n2 + 2))
      se <- s * sqrt(1/n1 + 1/n2)
      t <- qt(1 - alpha / 2, n1 + n2 - 2)
      t * se
    })

    if (is.null(conf.width))
      prec <- eval(md)
    if (is.null(n1)) {
      f <- function(r, sd1, sd2, alpha, prec) uniroot(function(n1) eval(md) - prec,
                                                      c(2, 1e+07), tol = tol)$root
      n1 <- mapply(f, r = r, sd1 = sd1, sd2 = sd2, alpha = alpha, prec = prec)
    }
  }

  # assume unequal standard deviation
  if (var == "unequal") {
    md_ueq <- quote({
      n2 <- n1 * r
      s <- sd1 ^ 2 / n1 + sd2 ^ 2 / n2
      v <- s ^ 2 / (sd1 ^ 4 / (n1 ^ 2 * (n1 - 1)) + sd2 ^ 4 / (n2 ^ 2 * (n2 - 1)))
      t <- qt(1 - alpha / 2, v)
      t * sqrt(s)
    })
    if (is.null(conf.width))
      prec <- eval(md_ueq)
    if (is.null(n1)){
      f <- function(sd1, sd2, r, alpha, prec) uniroot(function(n1) eval(md_ueq) - prec,
                                                      c(2, 1e+07), tol = tol)$root
      n1 <- mapply(f, sd1 = sd1, sd2 = sd2, r = r, alpha = alpha, prec = prec)
    }
  }

  n2 <- n1 * r
  if (is.null(conf.width))
    conf.width <- prec * 2

  structure(list(delta = delta,
                 sd1 = sd1,
                 sd2 = sd2,
                 n1 = n1,
                 n2 = n2,
                 conf.width = conf.width,
                 conf.level = conf.level,
                 lwr = delta - prec,
                 upr = delta + prec,
                 #note = paste(var, "variance"),
                 method = paste(est, "for mean difference with", var, "variance")),
            class = "presize")
}





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
#' Miettinen-Nurminen (\code{mn}) provide a closed from equation for the
#' restricted maximum likelihood estimate . The implementation is based on
#' code provided by Yongyi Min on
#' \url{http://users.stat.ufl.edu/~aa/cda/R/two-sample/R2/index.html}
#'
#' Agresti-Caffo (\code{ac}) confidence interval is based on the Wald confidence
#' interval, adding 1 success to each cell of the 2 x 2 table (see Agresti and
#' Caffo 2000).
#'
#' \code{\link[stats]{uniroot}} is used to solve n for the newcombe, ac, and mn
#' method.
#'
#'
#' @param p1 Risk among unexposed
#' @param p2 Risk among exposed
#' @param n1 Number of patients in unexposed group
#' @param r allocation ratio (relative size of unexposed and exposed cohort
#'   (\code{n1} / \code{n2}))
#' @param method Exactly one of \code{newcombe} (\emph{default}), \code{mn}
#'   (Miettinen-Nurminen), \code{ac} (Agresti-Caffo), \code{wald}. Methods can
#'   be abbreviated.
#' @inheritParams prec_mean
#'
#' @references
#' Agresti A (2003) \emph{Categorical Data Analysis}, Second Edition, Wiley
#' Series in Probability and Statistics,
#' \href{https://doi.org/10.1002/0471249688}{doi:10.1002/0471249688}
#'
#' Agresti A and Caffo B (2000) \emph{Simple and Effective Confidence Intervals
#' for Proportions and Differences of Proportions Result from Adding Two
#' Successes and Two Failures}, The American Statistician, 54(4):280-288
#'
#' Miettinen O and Nurminen M (1985) \emph{Comparative analysis of two rates},
#' Statistics in Medicine, 4:213-226
#'
#' Newcombe RG (1998) \emph{Interval estimation for the difference between
#' independent proportions: comparison of eleven methods}, Statistics in
#' Medicine, 17:873-890
#'
#' Fagerland MW, Lydersen S, and Laake P (2015). \emph{Recommended confidence
#' intervals for two independent binomial proportions}, Statistical methods in
#' medical research 24(2):224-254.
#'
#' @examples
#' # Validate Newcombe (1998)
#' prec_riskdiff(p1 = 56/70, p2 = 48/80, n1 = 70, r = 70/80, met = "newcombe")  # Table IIa
#' prec_riskdiff(p1 = 10/10, p2 = 0/10, n1 = 10, met = "newcombe")  # Table IIh
#'
#' prec_riskdiff(p1 = c(56/70, 9/10, 6/7, 5/56),
#'               p2 = c(48/80, 3/10, 2/7, 0/29),
#'               n1 = c(70, 10, 7, 56),
#'               r = c(70/80, 1, 1, 56/29),
#'               method = "wald")
#'
#' @importFrom stats qchisq
prec_riskdiff <- function(p1, p2, n1 = NULL, conf.width = NULL,
                     r = 1, conf.level = 0.95,
                     method = c("newcombe", "mn", "ac", "wald"),
                     tol = .Machine$double.eps^0.25) {
  if (sum(sapply(list(n1, conf.width), is.null)) != 1)
    stop("exactly one of 'n1', and 'conf.width' must be NULL")
  if (!is.null(p1) && !is.numeric(p1) || any(0 > p1 | p1 > 1))
    stop("'p1' must be numeric in [0, 1]")
  if (!is.null(p2) && !is.numeric(p2) || any(0 > p2 | p2 > 1))
    stop("'p2' must be numeric in [0, 1]")

  if (length(method) > 1) {
    warning("more than one method was chosen, 'newcombe' will be used")
    method <- "newcombe"
  }

  methods <- c("newcombe", "mn", "ac", "wald")
  id <- pmatch(method, methods)
  meth <- methods[id]
  if (is.na(id)) {
    warning("Method '", method, "' is not available, 'newcombe' will be used.")
    meth <- "newcombe"
  }

  delta <- p1 - p2
  if (is.null(n1)) {
    prec <- conf.width / 2
    est <- "sample size"
  }
  if (is.null(conf.width)) {
    n2 <- n1 / r
    est <- "precision"
  }

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

  if (meth == "mn") {
   # The functions diffscoreci and z2stat are copied from http://users.stat.ufl.edu/~aa/cda/R/two-sample/R2/index.html
    diffscoreci <- function(x1,n1,x2,n2,conflev){
      px = x1/n1
      py = x2/n2
      z = qchisq(conflev,1)
      proot = px-py
      dp = 1-proot
      niter = 1
      while(niter <= 50){
        dp = 0.5*dp
        up2 = proot+dp
        score = z2stat(px,n1,py,n2,up2)
        if(score<z){ proot = up2 }
        niter = niter+1
        if((dp<0.0000001) || (abs(z-score)<.000001)){
          niter = 51
          ul = up2}
      }

      proot = px-py
      dp = 1+proot
      niter = 1
      while(niter <= 50){
        dp = 0.5*dp
        low2 = proot-dp
        score = z2stat(px,n1,py,n2,low2)
        if(score<z){ proot = low2 }
        niter = niter+1
        if((dp<0.0000001) || (abs(z-score)<.000001)){
          ll = low2
          niter = 51}
      }
      c(ll,ul)
    }
    z2stat <- function (p1x,nx,p1y,ny,dif){

      diff = p1x-p1y-dif
      if ( abs(diff) == 0 ) {
        fmdiff = 0}
      else{
        t = ny/nx
        a = 1+t
        b = -(1+ t + p1x + t*p1y + dif*(t+2))
        c = dif*dif + dif*(2*p1x + t +1) + p1x + t*p1y
        d = -p1x*dif*(1+dif)
        v = (b/a/3)^3 - b*c/(6*a*a) + d/a/2
        s = sqrt( (b/a/3)^2 - c/a/3)
        if(v>0){u=s}
        else{u=-s}
        w = (3.141592654+acos(v/u^3))/3
        p1d = 2*u*cos(w) - b/a/3
        p2d = p1d - dif
        var = p1d*(1-p1d)/nx + p2d*(1-p2d)/ny
        fmdiff = diff^2/var
      }
      return(fmdiff)
    }

    if (is.null(conf.width)) {
      x1 <- n1 * p1
      x2 <- n2 * p2
      ci <- mapply(diffscoreci, x1 = x1, n1 = n1, x2 = x2, n2 = n2, conflev = conf.level)
      lwr <- ci[1,]
      upr <- ci[2,]
      conf.width <- upr - lwr
    }
    if (is.null(n1)) {
      f <- function(p1, p2, conf.width, r = r, conf.level = conf.level) {
        uniroot(function(n1) {
          n2 <- n1 / r
          x1 <- p1 * n1
          x2 <- p2 * n2
          ci <- diffscoreci(x1, n1, x2, n2, conf.level)
          ci[2] - ci[1] - conf.width
        },
        c(2, 1e+07))$root
      }
      n1 <- mapply(f, p1 = p1, p2 = p2, conf.width = conf.width, r = r, conf.level = conf.level)
      n2 <- n1 / r
      x1 <- n1 * p1
      x2 <- n2 * p2
      ci <- mapply(diffscoreci, x1 = x1, n1 = n1, x2 = x2, n2 = n2, conflev = conf.level)
      lwr <- ci[1,]
      upr <- ci[2,]
    }
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
                 method = paste(est, "for a risk difference with", meth, "confidence interval")),
            class = "presize")
}




# risk ratio --------------------
#' Sample size or precision for risk ratio
#'
#' \code{prec_riskratio} returns the risk ratio and the sample size or the
#' precision for the provided proportions
#'
#' Exactly one of the parameters \code{n1, conf.width} must be passed as NULL,
#' and that parameter is determined from the other.
#'
#' Koopman (\code{koopman}) provides an asymptotic score confidence interval
#' that is always consistent with Pearsons chi-squared test. It is the
#' recommended interval (Fagerland et al.).
#'
#' Katz (\code{katz}) use a logarithmic transformation to calculate the
#' confidence interval. The CI cannot be computed if one of the proportions is
#' zero. If both proportions are 1, the estimate of the standard error becomes
#' zero, resulting in a CI of [1, 1].
#'
#' \code{\link[stats]{uniroot}} is used to solve n for the katz, and koopman
#' method.
#'
#' @param method Exactly one of \code{koopman} (\emph{default}), \code{katz}.
#'   Methods can be abbreviated.
#' @inheritParams prec_mean
#' @inheritParams prec_riskdiff
#'
#' @references
#' Fagerland MW, Lydersen S, and Laake P (2015). \emph{Recommended confidence
#' intervals for two independent binomial proportions}, Statistical methods in
#' medical research 24(2):224-254.
#'
#' Katz D, Baptista J, Azen SP, and Pike MC (1978) \emph{Obtaining Confidence
#' Intervals for the Risk Ratio in Cohort Studies}, Biometrics 34:469-474
#'
#' Koopman PAR (1984) \emph{Confidence Intervals for the Ratio of Two Binomial
#' Proportions}, Biometrics 40:513-517
#'
#' @importFrom stats qchisq
#' @examples
#' # Validate funciton with example in Fagerland et al. (2015), Table 5.
#' prec_riskratio(p1 = 7/34, p2 = 1/34, n1 = 34, r = 1, met = "katz")
#' # 7 (0.91 to 54)
#' prec_riskratio(p1 = 7/34, p2 = 1/34, n1 = 34, r = 1, met = "koopman")
#' # 7 (1.21 to 43)
#'
#' # Validate the Koopman method with example in Koopman (1984)
#' prec_riskratio(p1 = 36/40, p2 = 16/80, n1 = 40, r = 2, met = "koopman")
#' # 4.5 (2.94 to 7.15)

prec_riskratio <- function(p1, p2, n1 = NULL, r = 1, conf.width = NULL,
                           conf.level = 0.95,
                           method = c("koopman", "katz"),
                           tol = .Machine$double.eps^0.25) {
  # check input
  if (sum(sapply(list(n1, conf.width), is.null)) != 1)
    stop("exactly one of 'n1', and 'conf.width' must be NULL")
  if (!is.null(p1) && !is.numeric(p1) || any(0 > p1 | p1 > 1))
    stop("'p1' must be numeric in [0, 1]")
  if (!is.null(p2) && !is.numeric(p2) || any(0 > p2 | p2 > 1))
    stop("'p2' must be numeric in [0, 1]")

  default_meth <- "koopman"
  if (length(method) > 1) {
    warning("more than one method was chosen, '", default_meth, "' will be used")
    method <- default_meth
  }

  methods <- c("koopman", "katz")
  id <- pmatch(method, methods)
  meth <- methods[id]
  if (is.na(id)) {
    warning("Method '", method, "' is not available, '", default_meth, "' will be used.")
    meth <- default_meth
  }


  # General parameters
  rr <- p1 / p2

  alpha <- (1 - conf.level)
  z <- qnorm(1 - alpha / 2)
  z2 <- z * z

  if (is.null(n1)) {
    prec <- conf.width / 2
    est <- "sample size"
  }
  if (is.null(conf.width)) {
    n2 <- n1 * r
    est <- "precision"
  }

  # Koopman CI (default)
  if (meth == "koopman") {
    # function to wrap kp and call uniroot for the lower and upper ci.
    fkp <- function(n1, p1, p2, r, conf.level) {
      zchi <- qchisq(conf.level, 1)
      n2 <- n1 * r
      x1 <- n1 * p1
      x2 <- n2 * p2
      ntot <- n1 + n2
      midp <- rr

      kp <- quote({
        A <- phi * (n1 + x2) + x1 + n2
        pp <- (A - sqrt(A ^ 2 - 4 * phi * (n1 + n2) * (x1 + x2))) / (2 * (n1 + n2))
        U <- (x1 - n1 * pp) ^ 2 / (n1 * pp * (1 - pp)) * (1 + n1 * (phi - pp) / (n2 * (1 - pp)))
        U
      })

      if(x2 == 0) {
        upr <- Inf
        midp <- 1e+07
      }
      if(x1 == 0) {
        lwr <- 0
        midp <- .1
      }
      if(x2 > 0) {
        upr <- uniroot(function(phi) eval(kp) - zchi,
                       c(midp, 1e+07), tol = tol, extendInt = "upX")$root
      }
      if(x1 > 0) {
        lwr <- uniroot(function(phi) eval(kp) - zchi,
                       c(0, midp), tol = tol, extendInt = "downX")$root
      }
      c(lwr, upr)
    }

    if (is.null(n1)) {
      fkpn <- function(p1, p2, r, conf.width, conf.level)
        uniroot(function(n1) {
          ci <- fkp(n1 = n1, p1 = p1, p2 = p2, r = r, conf.level = conf.level)
          ci[2] - ci[1] - conf.width
        },
        c(2, 1e+07), tol = tol)$root

      n1 <- mapply(fkpn, p1 = p1, p2 = p2, r = r, conf.width = conf.width, conf.level = conf.level)
      n2 <- n1 * r
    }
    # the ci must be solved for an unknown conf.width, and an unknown n.
    ci <- mapply(fkp, p1 = p1, p2 = p2, n1 = n1, r = r, conf.level = conf.level)
    lwr <- ci[1,]
    upr <- ci[2,]
    conf.width <- upr - lwr
  }

  # Katz log ci
  if (meth == "katz") {
    kt <- quote({
      n2 <- n1 * r
      prec <- z * sqrt((1 - p1) / (n1 * p1) + (1 - p2) / (n2 * p2))
      upr <- rr * exp(prec)
      lwr <- rr / exp(prec)
      cw <- upr - lwr
      list(n2 = n2,
           upr = upr,
           lwr = lwr,
           cw = cw)})
    if (is.null(conf.width)) {
      ci <- eval(kt)
      conf.width <- ci$cw
    }
    if (is.null(n1)) {
      f <- function(p1, p2, rr, r, z, conf.width) uniroot(function(n1) eval(kt)$cw - conf.width,
                                                          c(2, 1e+07), tol = tol)$root
      n1 <- mapply(f, p1 = p1, p2 = p2, rr = rr, r = r, z = z, conf.width = conf.width)
      ci <- eval(kt)
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
                 rr = rr,
                 lwr = lwr,
                 upr = upr,
                 conf.width = conf.width,
                 conf.level = conf.level,
                 #note = "n is number in *each* group",
                 method = paste(est, "for a relative risk with", meth, "confidence interval")),
            class = "presize")
}




# Odds ratio ---------------
#' Sample size or precision for an odds ratio
#'
#' \code{prec_or} returns the sample size or the precision for the
#' provided proportions
#'
#' Exactly one of the parameters \code{n, conf.width} must be passed as NULL,
#' and that parameter is determined from the other.
#'
#' Woolf (\code{woolf}), Gart (\code{gart}), and Independence-smoothed logit
#' (\code{indip_smooth}) belong to a general family of adjusted confidence
#' intervals, adding 0 (woolf) to each cell, 0.5 (gart) to each cell, or an
#' adjustment for each cell based on observed data (independence-smoothed). In
#' gart and indip_smooth, estimate of the CI is not possible if $p1 == 0$, in
#' which case the OR becomes 0, but the lower level of the CI is $> 0$. Further,
#' if $p1 == 1$ and $p2 < 1$, or if $p1 > 0$ and $p2 == 0$, the OR becomes
#' $\infty$, but the upper limit of the CI is finite. For the approximate
#' intervals, \code{gart} and \code{indip_smooth} are the recommended intervals
#' (Fagerland et al. 2011).
#'
#' \code{\link[stats]{uniroot}} is used to solve n for the woolf, gart, and
#' indip_smooth method.
#'
#' @references
#' Fagerland MW, Lydersen S, Laake P (2015). \emph{Recommended
#' confidence intervals for two independent binomial proportions}. Statistical
#' Methods in Medical Research, 24(2):224–254.
#' \href{https://doi.org/10.1177/0962280211415469}{doi:10.1177/0962280211415469}
#'
#' @param method Exactly one of \code{indip_smooth} (\emph{default}),
#'   \code{gart}, or \code{wolf}. Methods can be abbreviated.
#' @inheritParams prec_riskdiff
#' @return Object of class "presize", a list of arguments (including the
#'   computed one) augmented with method and note elements.

prec_or <- function(p1, p2, n1 = NULL, r = 1, conf.width = NULL, conf.level = 0.95,
                    method = c("gart", "woolf", "indip_smooth"),
                    tol = .Machine$double.eps^0.25)  {
  # check input
  if (sum(sapply(list(n1, conf.width), is.null)) != 1)
    stop("exactly one of 'n1', and 'conf.width' must be NULL")
  if (!is.null(p1) && !is.numeric(p1) || any(0 > p1 | p1 > 1))
    stop("'p1' must be numeric in [0, 1]")
  if (!is.null(p2) && !is.numeric(p2) || any(0 > p2 | p2 > 1))
    stop("'p2' must be numeric in [0, 1]")

  default_meth <- "indip_smooth"
  if (length(method) > 1) {
    warning("more than one method was chosen, '", default_meth, "' will be used")
    method <- default_meth
  }

  meths <- c("gart", "woolf", "indip_smooth")
  id <- pmatch(method, meths)
  meth <- meths[id]
  if (is.na(id)) {
    warning("Method '", method, "' is not available, '", default_meth, "' will be used.")
    meth <- default_meth
  }

  # General parameters
  or <- (p1 / (1 - p1)) / (p2 / (1 - p2))

  alpha <- (1 - conf.level)
  z <- qnorm(1 - alpha / 2)
  z2 <- z * z

  if (is.null(n1)) {
    prec <- conf.width / 2
    est <- "sample size"
  }
  if (is.null(conf.width)) {
    n2 <- n1 * r
    est <- "precision"
  }




  # Woolf, Gart, or indip_smooth
  if (meth %in% c("gart", "indip_smooth", "woolf")) {
    # Quote to adjust the cell frequencies, for Woolf, gart, and indip_smooth method
    adjust_cells <- quote({
      n2 <- n1 * r
      x1 <- p1 * n1
      x2 <- p2 * n2
      y1 <- n1 - x1
      y2 <- n2 - x2
      m1 <- x1 + x2
      m2 <- y1 + y2
      n <- n1 + n2

      x1. <- x1 + eval(c1)
      x2. <- x2 + eval(c2)
      y1. <- y1 + eval(c3)
      y2. <- y2 + eval(c4)

      theta <- (x1. * y2.) / (x2. * y1.)

      prec <- z * sqrt(1/x1. + 1/y1. + 1/x2. + 1/y2.)

      lwr <- exp(log(theta) - prec)
      upr <- exp(log(theta) + prec)
      list(lwr = lwr,
           upr = upr,
           cw = upr - lwr,
           n2 = n2)
    })

    if (meth == "woolf")
      c1 <- c2 <- c3 <- c4 <- 0
    if (meth == "gart")
      c1 <- c2 <- c3 <- c4 <- 0.5
    if (meth == "indip_smooth") {
      c1 <- expression(2 * n1 * m1 / n ^ 2)
      c2 <- expression(2 * n2 * m1 / n ^ 2)
      c3 <- expression(2 * n1 * m2 / n ^ 2)
      c4 <- expression(2 * n2 * m2 / n ^ 2)
    }

    if (is.null(conf.width)) {
      ci <- eval(adjust_cells)
      conf.width <- ci$cw
    }

    if (is.null(n1)) {
      f <- function(p1, p2, conf.width) uniroot(function(n1)
        eval(adjust_cells)$cw - conf.width,
        c(1, 1e+07), tol = tol)$root
      n1 <- mapply(f, p1 = p1, p2 = p2, conf.width = conf.width)
      ci <- eval(adjust_cells)
      n2 <- ci$n2
    }
  }



  structure(list(p1 = p1,
                 p2 = p2,
                 n1 = n1,
                 n2 = n2,
                 ntot = n1 + n2,
                 r = r,
                 or = or,
                 lwr = lwr,
                 upr = upr,
                 conf.width = conf.width,
                 conf.level = conf.level,
                 #note = "n is number in *each* group",
                 method = paste(est, "for an odds ratio with", meth, "confidence interval")),
            class = "presize")

}



