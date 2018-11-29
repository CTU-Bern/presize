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
#' Miettinen-Nurminen (\code{mn}) provide a closed from equation for the
#' restricted maximum likelihood estimate . The implementation is based on
#' code provided by Yongyi Min on
#' \url{http://users.stat.ufl.edu/~aa/cda/R/two-sample/R2/index.html}
#'
#' Agresti-Caffo (\code{ac}) confidence interval is based on the Wald confidence
#' interval, adding 1 success to each cell of the 2 x 2 table (see Agresti and
#' Caffo 2000).
#'
#' \code{\link[stats]{uniroot}} is used to solve n for the newcombe and ac
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
#' @examples
#' # Validate Newcombe (1998)
#' prec_riskdiff(p1 = 56/70, p2 = 48/80, n1 = 70, r = 70/80, met = "newcombe")  # Table IIa
#' prec_riskdiff(p1 = 10/10, p2 = 0/10, n1 = 10, met = "newcombe")  # Table IIh
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
  if (is.null(n1))
    prec <- conf.width / 2
  if (is.null(conf.width))
    n2 <- n1 / r

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
                 method = meth),
            class = "presize")
}


